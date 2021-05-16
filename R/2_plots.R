plot_rankingsviz <- function(load_type, history_rankings, player_names){
  if(load_type == "My History") history_plot <- .plot_myhistory(history_rankings, player_names)
  if(load_type == "Ranking ID") history_plot <- .plot_sessionID(history_rankings, player_names)

  return(history_plot)
}


.plot_myhistory <- function(history_rankings, player_names = NULL){

  if(is.null(player_names)) {

    return(NULL)

  }

  # Scatterplot
  df_plot <-
    history_rankings %>%
    filter(`Player Name` %in% player_names) %>%
    ggplot(
      aes(x = Timestamp,
          y = Z,
          colour = `Player Name`,
          group = `Player Name`,
          data_id = `Player Name`,
          tooltip = glue::glue("
                                 {`Player Name`}
                                 Date: {as_date(Timestamp)}
                                 Type: {ecr_type} - {ecr_position}
                                 Your Rank: {`Your Rank`}
                                 FP Rank: {`FP Rank`}
                                 Z: {Z}
                                 ")
      )) +
    theme_minimal() +
    geom_point_interactive() +
    geom_line(stat = "smooth", alpha = 0.5, method = "glm", formula = "y~x", se = FALSE) +
    scale_fill_brewer(palette = "Set2",direction = -1,aesthetics = c("colour","fill")) +
    labs(
      title = glue("Your Rankings vs FantasyPros"),
      subtitle = "_measured in Z-score (standard deviations above/below FP average)_") +
    xlab(NULL) +
    ylab(NULL) +
    theme(legend.position = "bottom",
          text = element_text(family = "IBM Plex Sans Condensed"),
          plot.subtitle = element_markdown(),
          plot.title.position = "plot")

  return(df_plot)
}

.plot_sessionID <- function(history_rankings, player_names){

  if(is.null(player_names)) {

    return(NULL)

  }

  df_plot <-
    history_rankings %>%
    filter(`Player Name` %in% player_names) %>%
    mutate(
      `Player Name` = paste(`Player Name`, "-", Pos),
      `Player Name` = fct_reorder(`Player Name`, Z)
    ) %>%
    ggplot(aes(x = `Player Name`,
               y = Z,
               tooltip = glue::glue("
                                 {`Player Name`}
                                 Date: {as_date(Timestamp)}
                                 Type: {ecr_type} - {ecr_position}
                                 Your Rank: {`Your Rank`}
                                 FP Rank: {`FP Rank`}
                                 Z: {Z}
                                 "),
               fill = `Player Name`
    )) +
    coord_flip() +
    theme_minimal() +
    geom_col_interactive(color = "black") +
    xlab(NULL)+
    ylab(NULL)+
    geom_hline(yintercept = 0, size = 1) +
    # scale_fill_manual() +
    scale_fill_brewer(palette = "Set3",direction = -1,aesthetics = c("colour","fill")) +
    labs(
      title = glue("**Rankings Highlights**"),
      subtitle = glue("_**Highest** and **Lowest** measured by Z-score <br>
                      (standard deviations above/below FP average)_"),
      caption = glue(
        "_{history_rankings$user_nickname[[1]]}",
        " - {history_rankings$ecr_type[[1]]}",
        " - {history_rankings$ecr_position[[1]]}",
        " - {lubridate::as_date(history_rankings$Timestamp[[1]])}_")) +
    theme(legend.position = "none",
          text = element_text(family = "IBM Plex Sans Condensed"),
          plot.title = element_markdown(),
          plot.subtitle = element_markdown(),
          plot.caption = element_markdown(),
          plot.title.position = "plot")

  return(df_plot)
}
