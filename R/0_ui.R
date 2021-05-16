suppressPackageStartupMessages({

  # Data import
  library(ffscrapr)
  library(httr)
  library(jsonlite)
  library(here)
  library(arrow)

  # Data manipulation
  library(tidyverse)
  library(lubridate)
  library(glue)
  library(magrittr)
  library(rlang)
  library(yaml)

  # Data viz
  library(RColorBrewer)
  library(ggiraph)
  library(ggtext)
  library(showtext)
  library(sysfonts)
  # library(ragg)
  # library(systemfonts)

  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(DT)
  library(waiter)
  library(sever)
  library(auth0)
  library(uuid)
  library(metathis)
  library(details)

  # Data output
  library(writexl)

  # options(shiny.useragg = TRUE)

  showtext_auto()

  font_add_google("IBM Plex Sans Condensed")

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
    width = 12,
    inputId = 'box_inputs',
    status = "danger",
    title = "Start",

    fluidRow(
      markdown(
        "The **DynastyProcess Rankings** app helps you build your own set of fantasy football rankings, using FantasyPros current rankings as a starting point!"
      )),
    fluidRow(
      details(summary = "How to Use",
              output = "html",
              markdown(
                "As you drag and drop players, the player will recolour to indicate how far away from the expert consensus you are.

***Green*** indicates that you are higher on that player, while ***purple*** indicates that you are lower on that player.

You can save, download, or import past rankings by referring to a specific rankings ID, as well as look at your past rankings on the History tab.

In future iterations, you will be able to use the rankings you make as the basis for a trade calculator!"
              )
      )
    ),
hr(),
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

#### FAQ ####

box_faq <- function(){

  box(
    width = 12,
    status = "danger",
    title = "Frequently Asked Questions",
    includeMarkdown("faq.md")
  )

}

#### Table Box ####

box_rankings <- function(ranking_type, position){

  box_title <- glue("Your Rankings: {ranking_type} {position}")

  box(
    width = 12,
    inputId = 'box_rankings',
    status = "danger",
    title = box_title,
    fluidRow(
      column(width = 12,
             div(
               style = "text-align:center;",
               actionButton("save_rankings",
                            "Save",
                            icon = icon("quidditch"),
                            class = "btn-primary"),
               downloadButton("download_rankings",
                              "Download",
                              class = "btn-success"),
               actionButton("import_rankings",
                            "Import",
                            icon = icon("file-import"),
                            class = "btn-danger")
             )
      )
    ),
    hr(),
    fluidRow(
      DTOutput("rankings_table")
    )
  )

}

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

fn_box_historyviz <- function(history_rankings, player_names){

  tagList(
    pickerInput(
      "historyplot_playernames",
      label = "Select Players",
      choices = unique(history_rankings$`Player Name`),
      selected = player_names,
      multiple = TRUE,
      options = list(
        `max-options` = 10,
        `live-search` = TRUE,
        `actions-box` = TRUE,
        `selected-text-format` = "count > 0"
      ),
    ),
    br(),
    girafeOutput("rankings_viz",height = 400)
  )
}

box_history_table <- function(){
  box(
    width = 12,
    title = "Rankings Lookup",
    status = "danger",
    DTOutput("history_rankings"),
    footer = div(style = "text-align:center;",downloadButton("download_loaded_rankings",class = "btn-success"))
  )
}
