options(dplyr.summarise.inform = FALSE)

ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  title = "Rankings App - DynastyProcess.com",
  navbar = ui_header("Rankings App - DynastyProcess.com"),
  sidebar = ui_sidebar(
    menuItem("Rankings App", tabName = "rankings", icon = "hat-wizard"),
    external_menuItem("More by DynastyProcess", "https://dynastyprocess.com", icon = "quidditch")
  ),
  body = dashboardBody(
    includeCSS("dp.css"),
    use_waiter(),
    waiter_on_busy(html = spin_dots(), color = transparent(0.3)),
    use_sever(),
    tabItems(
      tabItem(
        tabName = "rankings",
        fluidRow(
          box_inputs(),
        ),
        DTOutput("table")
      )
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$debug, browser())

  sever_dp()

  #### Update Pickers ####

  observeEvent(input$rank_type, {

    if(str_detect(input$rank_type, "Overall")) {
      updatePickerInput(session, "position",choices = c("All Offense", "All Defense"))
    }

    if(str_detect(input$rank_type, "Position")) {
      updatePickerInput(session, "position", choices = c("QB","RB","WR","TE","DL","LB","DB"))
    }

  })

  #### Load Data ####

  df_fantasypros <- reactiveVal()

  observeEvent(input$load, {

    load_fpdata(input$rank_type,input$position) %>%
      df_fantasypros()

    Sys.sleep(2)

    updatebs4Card("box_inputs",session = session, action = "toggle")

  })

  output$table <- renderDT(df_fantasypros())

}

shinyApp(ui, server)
