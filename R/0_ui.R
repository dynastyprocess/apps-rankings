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
          choices = c("Dynasty Positional",
                      "Dynasty Overall 1QB",
                      "Dynasty Overall SF",
                      "Redraft Positional",
                      "Redraft Overall 1QB",
                      "Redraft Overall SF"))
      ),
      column(
        width = 4,
        pickerInput(
          'position',
          label = "Select Position",
          choices = c("All Offense","QB","RB","WR","TE","All Defense", "DL", "LB", "DB"),
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
      width = 12,
      inputId = 'box_rankings',
      status = "danger",
      title = box_title,
      fluidRow(
        DTOutput("table")
      )
    )

  }

