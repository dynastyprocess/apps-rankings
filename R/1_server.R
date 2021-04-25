# fantasypros_raw <-
#   read.csv("https://github.com/dynastyprocess/data/raw/master/files/db_fpecr.csv") %>%
#   filter(
#     scrape_date == max(scrape_date)
#   )
#
# write_parquet(fantasypros_raw, "data/fantasypros_raw.parquet")

load_fpdata <- function(type, position){

  if(position != "Rookies"){

    type_list <- c("Dynasty Positional"= "dp",
                   "Dynasty Overall 1QB"= "do",
                   "Dynasty Overall SF"= "dsf",
                   "Redraft Positional"= "rp",
                   "Redraft Overall 1QB"= "ro",
                   "Redraft Overall SF"= "rsf")

    type <- type_list[type]
  }

  if(position == "Rookies") type <- "drk"

  fantasypros_raw <- read_parquet("data/fantasypros_raw.parquet") %>%
    filter(ecr_type == type) %>%
    mutate(sd = case_when(is.null(sd)~ecr^0.6, # if SD is missing, proxy it as ecr^0.6 so that Z scores are still there
                          sd == 0 ~ ecr^0.6, # if SD is zero, Z scores are INF so set it as 1
                          ecr > 32 ~ ecr^0.6, # SDs for fantasypros are too low for players after a certain threshold which makes for wonky Z scores
                          TRUE ~ sd))

  if(str_detect(type, "o$|sf$|rk$")) {

    if(position == "All Offense") {
      fantasypros_raw <- fantasypros_raw %>%
        filter(pos %in% c("QB","RB","WR","TE"))%>%
        arrange(ecr)
    }

    if(position == "All Defense"){
      fantasypros_raw <- fantasypros_raw %>%
        filter(pos %in% c("DL","LB","DB"))%>%
        arrange(ecr)
    }

    return(fantasypros_raw)
  }

  fantasypros_raw <- fantasypros_raw %>%
    filter(pos == position) %>%
    arrange(ecr)

  return(fantasypros_raw)
}

parse_fpdata <- function(fp_data, session, input_rank_type, input_position){

  s_user_id <- session[['userData']][["auth0_info"]][["sub"]] %||% "no_user_id"
  s_user_nickname <- session$userData$auth0_info$nickname %||% "no_user_nickname"
  s_user_name <- session$userData$auth0_info$name %||% "no_user_name"

  fp_data %>%
    mutate(
      `Your Rank` = row_number(),
      Z = round((ecr - `Your Rank`) / sd, 1),
      sd = round(sd, 1),
      ecr_type = input_rank_type,
      ecr_position = input_position,
      user_id = s_user_id,
      user_nickname = s_user_nickname,
      user_name = s_user_name,
    ) %>%
    select(
      `Player Name` = player_name,
      Pos = pos,
      Team = tm,
      Age = age,
      `Your Rank`,
      `FP Rank` = ecr,
      Z,
      SD = sd,
      Best = best,
      Worst = worst,
      `Scrape Date` = scrape_date,
      fantasypros_id,
      ecr_type,
      ecr_position,
      user_id,
      user_name,
      user_nickname
    )

}

apply_reorder <- function(order_info, full_rankings, ranking_length){

  if (is.null(order_info) || class(order_info) != "character") {
    return()
  }

  order_info <- read_yaml(text = order_info)

  if(length(order_info)==0){
    return()
  }

  new_order <- seq_len(ranking_length)

  for (i in 1:length(order_info)) {
    j <- order_info[[i]]

    new_order[(j$oldPosition + 1)] <- j$newPosition + 1
  }

  new_rankings <- full_rankings %>%
    select(-`Your Rank`) %>%
    mutate(
      `Your Rank` = new_order,
      Z = round((`FP Rank` - `Your Rank`) /SD, 1)
    ) %>%
    arrange(`Your Rank`) %>%
    select(
      `Player Name`,
      Pos,
      Team,
      Age,
      `Your Rank`,
      `FP Rank`,
      Z,
      SD,
      Best,
      Worst,
      `Scrape Date`,
      fantasypros_id,
      ecr_type,
      ecr_position,
      user_id,
      user_name,
      user_nickname
    )

  return(new_rankings)
}

load_rankings_from_storage <- function(s_user_id, s_session_id, load_type){

  storage <- arrow::open_dataset(sources = "storage")

  if(load_type == "My History") {

    loaded_rankings <- storage %>%
      filter(
        user_id == s_user_id
      ) %>%
      select(
        any_of(c(
          "Timestamp" = "session_timestamp",
          "rankings_id"="session_id",
          "user_nickname",
          "Player Name",
          "Pos",
          "Team",
          "Age",
          "Your Rank",
          "FP Rank",
          "Z",
          "SD",
          "Best",
          "Worst",
          "fantasypros_id",
          "ecr_type",
          "ecr_position"
        ))
      ) %>%
      collect() %>%
      group_by(rankings_id) %>%
      filter(Timestamp == max(Timestamp)) %>%
      ungroup()

  }

  if(load_type == "Ranking ID"){

    loaded_rankings <- storage %>%
      filter(
        session_id == s_session_id
      ) %>%
      select(
        any_of(c(
          "Timestamp" = "session_timestamp",
          "rankings_id" = "session_id",
          "user_nickname",
          "Player Name",
          "Pos",
          "Team",
          "Age",
          "Your Rank",
          "FP Rank",
          "Z",
          "SD",
          "Best",
          "Worst",
          "fantasypros_id",
          "ecr_type",
          "ecr_position"
        ))
      ) %>%
      collect() %>%
      group_by(rankings_id) %>%
      filter(Timestamp == max(Timestamp)) %>%
      ungroup()
  }

  return(loaded_rankings)
}

fn_prepopulate_playernames<- function(history_rankings){

  if(load_type() == "My History") {
    player_names <- history_rankings %>%
      arrange(desc(Z)) %>%
      distinct(`Player Name`, .keep_all = TRUE) %>%
      slice(1:3) %>%
      pull(`Player Name`)
  }

if(load_type() == "Ranking ID") {
  player_names <- history_rankings %>%
    arrange(desc(Z)) %>%
    distinct(`Player Name`, .keep_all = TRUE) %>%
    slice(1:5, (nrow(.)- 4):nrow(.)) %>%
    pull(`Player Name`)
}

return(player_names)
}

fn_import_rankings <- function(session, import_rankings_id, df_fantasypros){

  imported_rankings <- open_dataset("storage") %>%
    filter(session_id == import_rankings_id) %>%
    collect()
}
# CHECK IF RANKINGS ID EXISTS
if(nrow(imported_rankings)==0) return(
  showModal(modalDialog(
    title = "Error",
    glue::glue("Could not find Rankings ID {input$import_rankings_id} in our database")
  ),
  session = session)
)
# CHECK IF RANKINGS ID TYPE MATCHES THE CURRENT TYPE
if(imported_rankings$ecr_type[[1]] != selected_rank_type() |
   imported_rankings$ecr_position[[1]] != selected_position()) {

  return(
    showModal(modalDialog(
      title = "Error",
      glue::glue("The imported rankings are for
                     {imported_rankings$ecr_type[[1]]} - {imported_rankings$ecr_position[[1]]}
                     and not for the currently selected {selected_rank_type()} - {selected_position()}")
    ),
    session = session)
  )

  overwrite_current <- df_fantasypros %>%
    select(-`Your Rank`) %>%
    left_join(
      imported_rankings %>% select(fantasypros_id,`Your Rank`),
      by = "fantasypros_id"
    ) %>%
    arrange(`Your Rank`) %>%
    relocate(`Your Rank`, .after = Age) %>%
    mutate(Z = round((`FP Rank` - `Your Rank`) /SD, 1))

  return(overwrite_current)
}
