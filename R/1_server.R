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
    mutate(sd = case_when(is.null(sd)~ecr^0.6,
                          sd == 0 ~ ecr^0.6,
                          # ecr > 32 ~ ecr^0.6,
                          TRUE ~ sd))

  if(str_detect(type, "o$|sf$|rk$")) {

    if(position == "All Offense") {
      fantasypros_raw <- fantasypros_raw %>%
        filter(pos %in% c("QB","RB","WR","TE"))
    }

    if(position == "All Defense"){
      fantasypros_raw <- fantasypros_raw %>%
        filter(pos %in% c("DL","LB","DB"))
    }

    return(fantasypros_raw)
  }

  fantasypros_raw <- fantasypros_raw %>%
    filter(pos == position)

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
