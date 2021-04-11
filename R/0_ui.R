suppressPackageStartupMessages({

  # Data import
  library(ffscrapr)
  library(httr)
  library(jsonlite)
  library(here)
  library(arrow)

  # Data manipulation
  library(tidyverse)
  library(janitor)
  library(lubridate)
  library(glue)
  library(magrittr)
  library(rlang)
  library(yaml)

  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(reactable)
  library(DT)
  library(RColorBrewer)
  library(waiter)
  library(sever)
  library(auth0)
  library(uuid)

  # Data output
  library(writexl)

})

#### UI FUNCTIONS ####

ui_header <- function(title, ...) {
  bs4Dash::dashboardHeader(
    skin = "dark",
    fixed = TRUE,
    border = TRUE,
    # compact = TRUE,
    shiny::span(title, style = "font-size:1.5em;color:#ffffff"),
    ...
  )
}

ui_sidebar <- function(...) {
  bs4Dash::dashboardSidebar(
    title = "Apps",
    fixed = TRUE,
    skin = "dark",
    elevation = 3,
    collapsed = TRUE,
    opacity = 0.8,
    url = "https://dynastyprocess.com",
    expand_on_hover = TRUE,
    src = "https://avatars2.githubusercontent.com/u/63691873?s=400&u=d9289a2540799f19ca6d8ad866e219ee2d602ba9&v=4",
    bs4Dash::sidebarMenu(...)
  )
}

external_menuItem <- function(text = NULL, href = NULL, icon = NULL) {
  tags$li(
    tags$a(span(icon(icon), style = "font-size:1.1rem;"),
           p(text, style = "margin-left: .5rem;"),
           class = "nav-link", href = href
    ), class = "nav-item")
}


sever_dp <- function(){

  sever::sever(
    shiny::tagList(
      shiny::h1("Disconnected"),
      shiny::br(),
      shiny::p(shiny::em(joker::dadjoke())),
      shiny::br(),
      shiny::tags$button(
        "Reload",
        style = "color:#000;background-color:#fff;",
        class = "button button-raised",
        onClick = "location.reload();"
      )
    ),
    bg_color = "#000"
  )
}

#### Select Ranking Set & Position, if applicable ####

box_inputs <- function(){
  box(
    width = 8,
    inputId = 'box_inputs',
    status = "danger",
    title = "Select Ranking Set",
    fluidRow(
      column(
        width = 4,
        pickerInput(
          "rank_type",
          "Select Ranking Type",
          width = '100%',
          multiple = FALSE,
          choices = c(
            "Dynasty Overall 1QB",
            "Dynasty Overall SF",
            "Dynasty Positional",
            "Redraft Overall 1QB",
            "Redraft Overall SF",
            "Redraft Positional"
          ))
      ),
      column(
        width = 4,
        pickerInput(
          'position',
          label = "Select Position",
          choices = c("All Offense","QB","RB","WR","TE","Rookies","All Defense", "DL", "LB", "DB"),
          multiple = FALSE,
          width = '100%')
      )
    ),
    footer =
      span(
        actionButton('load','Load Ranking Set!',icon = icon('rocket'))
      )
  )
}


#### Table Box ####

box_rankings <- function(ranking_type, position){

  box_title <- glue("Your Rankings: {ranking_type} {position}")

  box(
    width = 8,
    inputId = 'box_rankings',
    status = "danger",
    title = box_title,
    fluidRow(
      column(width = 12,
             div(
               style = "text-align:center;",
               actionButton("save_rankings",
                            "Save Rankings",
                            icon = icon("quidditch"),
                            class = "btn-primary"),
               downloadButton("download_rankings",
                              "Download Rankings",
                              class = "btn-success")
             )
      )
    ),
    hr(),
    fluidRow(
      DTOutput("rankings_table")
    )
  )

}

#### Update Position Filter ####

update_position_filter <- function(session, rank_type){

  if (str_detect(rank_type, "Dynasty Overall")) {
    updatePickerInput(session, "position", choices = c("Rookies","All Offense"))
  }

  if (str_detect(rank_type, "Redraft Overall")) {
    updatePickerInput(session, "position", choices = c("All Offense", "All Defense"))
  }

  if (str_detect(rank_type, "Position")) {
    updatePickerInput(session, "position", choices = c("QB", "RB", "WR", "TE", "DL", "LB", "DB"))
  }
}

#### Rankings DT function ####
rankings_datatable <- function(input_df){

  colourlist <- colorRampPalette(brewer.pal(3, "PRGn"))

  input_df %>%
    datatable(
      extensions = "RowReorder",
      selection = "none",
      options = list(
        rowReorder = list(selector = "tr"),
        order = list(c(4, "asc")),
        dom = "ftir",
        paging = FALSE,
        searching = FALSE,
        scrollX = TRUE
      ),
      callback = JS("table.on('row-reorder',function(e, details, all){
                      Shiny.onInputChange('row_reorder', JSON.stringify(details));
                      });")
    ) %>%
    formatRound(c("Z", "SD"), 1) %>%
    formatStyle(columns = 0:10,
                valueColumns = "Z",
                backgroundColor = styleInterval(
                  quantile(range(-3, 3),
                           probs = seq(0.05, 0.95, 0.05),
                           na.rm = TRUE),
                  colourlist(20)))

}
