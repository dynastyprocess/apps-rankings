options(dplyr.summarise.inform = FALSE)

ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  title = "Rankings App - DynastyProcess.com",
  navbar = ui_header(
    "Rankings App - DynastyProcess.com"
  ),
  sidebar = ui_sidebar(
    menuItem("Rankings App", tabName = "rankings", icon = "hat-wizard"),
    external_menuItem("More by DynastyProcess", "https://dynastyprocess.com", icon = "quidditch"),
    uiOutput("user_info"),
    auth0::logoutButton(label = "Log Out",icon = icon("sign-out"))
  ),
  body = dashboardBody(
    includeCSS("dp.css"),
    use_waiter(),
    waiter_on_busy(html = spin_dots(), color = transparent(0.3)),
    use_sever(),
    tabItems(
      tabItem(
        tabName = "rankings",
        # actionButton("debug", label = "debug"),
        # br(),
        fluidRow(
          box_inputs(),
        ),
        fluidRow(
          uiOutput("rankings"),
        )
      )
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$debug, browser())

  sever_dp()

  output$user_info <- renderUI({
    bs4SidebarUserPanel(img = session$userData$auth0_info$picture,
                        text = session$userData$auth0_info$nickname)
  })

  #### Update Pickers ####

  observeEvent(input$rank_type, update_position_filter(session,input$rank_type))

  #### Load Data ####

  df_fantasypros <- reactiveVal()
  init_df <- reactiveVal()
  nrow_df <- reactiveVal()
  unique_id <- reactiveVal()

  observeEvent(input$load, {

    uuid::UUIDgenerate(use.time = TRUE) %>%
      str_sub(end = 8) %>%
      unique_id()

    loaded_df <- load_fpdata(input$rank_type, input$position) %>%
      parse_fpdata(session, input$rank_type, input$position)

    # Set reactives with loaded data
    df_fantasypros(loaded_df)
    init_df(loaded_df)
    nrow_df(nrow(df_fantasypros()))

    Sys.sleep(2)

    updatebs4Card("box_inputs",session = session, action = "toggle")
  })

  #### Datatable ####

  output$rankings_table <- renderDT({
    init_df() %>%
      select(
        -ecr_type,
        -ecr_position,
        -fantasypros_id,
        -user_id,
        -user_name,
        -user_nickname
      ) %>%
      rankings_datatable()

  })


  observeEvent(input$row_reorder, { # watching the "reorder" events ----

    new_rankings <- apply_reorder(order_info = input$row_reorder,
                                  full_rankings =  df_fantasypros(),
                                  ranking_length =  nrow_df())

    if(!is.null(new_rankings)) {
      df_fantasypros(new_rankings)

      replaced_data <- new_rankings %>%
        select(
          -ecr_type,
          -ecr_position,
          -fantasypros_id,
          -user_id,
          -user_name,
          -user_nickname
        )

      DT::replaceData(proxy = DT::dataTableProxy("rankings_table"),
                      data = replaced_data
      )

    }
  })

  output$rankings <- renderUI({
    req(init_df())

    box_rankings(input$rank_type, input$position)
  })

  observeEvent(input$save_rankings,{

    req(df_fantasypros())

    showModal(modalDialog("Saving your ranks, please wait!"))

    file_name <- glue::glue("{unique_id()}_{{i}}.parquet")

    df_fantasypros() %>%
      mutate(
        user_id = str_replace(user_id, "\\|", "_"),
        timestamp = Sys.time()
      ) %>%
      write_dataset(
        path = "storage",
        format = "parquet",
        partitioning = c("ecr_type","ecr_position","user_id"),
        basename_template = file_name,
        hive_style = FALSE
      )

    Sys.sleep(2)

    showModal(urlModal(
      url = unique_id(),
      title = "Rankings Saved!",
      subtitle = "Your rankings have been saved to server!
      You can look up this particular set of rankings later by searching for this session_id. "
    ))
  })

  output$download_rankings <- downloadHandler(
    filename = function() {
      glue::glue("DPRankings_{format(Sys.time(),format = '%Y%m%d%H%M%S')}_{input$rank_type}_{input$position}.xlsx")
    },
    content = function(file) {
      df_fantasypros() %>%
        mutate(
          user_id = str_replace(user_id, "\\|", "_"),
          session_id = unique_id(),
          session_timestamp = Sys.time()
        ) %>%
        write_xlsx(file)
    }
  )


}

options(shiny.port = 8080)

shinyAppAuth0(ui, server)

# shinyApp(ui, server)
