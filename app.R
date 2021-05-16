options(dplyr.summarise.inform = FALSE)

ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  title = "Rankings App - DynastyProcess.com",
  navbar = ui_header(
    "Rankings App - DynastyProcess.com"
  ),
  sidebar = ui_sidebar(
    menuItem("Create Rankings", tabName = "rankings", icon = "hat-wizard"),
    menuItem("Search Rankings", tabName = "history", icon = "bolt"),
    external_menuItem("More by DynastyProcess", "https://dynastyprocess.com", icon = "quidditch"),
    uiOutput("user_info"),
    auth0::logoutButton(label = "Log Out",icon = icon("sign-out"))
  ),
  body = dashboardBody(
    meta() %>%
      meta_social(
        title = "Rankings - DynastyProcess.com",
        description = "An interface for creating fantasy football rankings",
        url = "https://apps.dynastyprocess.com",
        image = "https://raw.githubusercontent.com/dynastyprocess/graphics/main/.dynastyprocess/logo-hex-small.png",
        image_alt = "An interface for creating fantasy football rankings",
        twitter_creator = "@_TanHo",
        twitter_card_type = "summary",
        twitter_site = "@DynastyProcess"
      ),
    includeCSS("dp.css"),
    use_waiter(),
    waiter_on_busy(html = spin_dots(), color = transparent(0.3)),
    use_sever(),
    tabItems(
      tabItem(
        tabName = "rankings",
        box_inputs(),
        uiOutput("rankings"),
      ),
      tabItem(
        tabName = "history",
        # actionButton("debug", label = "debug"),
        # br(),
        fluidRow(
          box(
            width = 4,
            title = "Search Rankings",
            status = "danger",
            "You can look up your past rankings, or look up a specific Session ID here!",
            hr(),
            fluidRow(
              style = "text-align:center;",
              radioGroupButtons(
                "toggle_session_history",
                label = NULL,
                # size = 'normal',
                choices = c("My History", "Ranking ID"),
                selected =  "My History",
                status = "danger",
                justified = TRUE,
                width = '100%',
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
              ),
              br(),
              conditionalPanel(
                condition = "input.toggle_session_history == 'Ranking ID'",
                textInput("session_id",
                          label = NULL,
                          width = '100%',
                          placeholder = "Ranking ID")
              )
            ),
            footer = div(actionButton("load_rankings",
                                      "Load Rankings",
                                      icon = icon("list-ol"),
                                      class = "btn-success"),
                         style = "text-align:center;")
          ),
          uiOutput("box_historyviz", container = box,  width = 8,   title = "Rankings Chart",  status = "danger"),
        ),

        uiOutput("box_history")
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
  history_rankings <- reactiveVal()

  selected_rank_type <- eventReactive(input$load, input$rank_type)
  selected_position <- eventReactive(input$load, input$position)

  observeEvent(input$load, {

    uuid::UUIDgenerate(use.time = TRUE) %>%
      str_sub(end = 8) %>%
      unique_id()

    loaded_df <- load_fpdata(selected_rank_type(), selected_position()) %>%
      parse_fpdata(session, selected_rank_type(), selected_position())

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

    box_rankings(selected_rank_type(), selected_position())
  })

  observeEvent(input$save_rankings,{

    req(df_fantasypros())

    showModal(modalDialog("Saving your ranks, please wait!"))

    file_name <- glue::glue("{unique_id()}_{{i}}.parquet")

    df_fantasypros() %>%
      mutate(
        user_id = str_replace(user_id, "\\|", "_"),
        session_timestamp = Sys.time(),
        session_id = unique_id(),
      ) %>%
      mutate_if(is.numeric,as.double) %>%
      write_dataset(
        path = "storage",
        format = "parquet",
        partitioning = c("ecr_type","ecr_position","user_id","session_id"),
        basename_template = file_name,
        hive_style = TRUE
      )

    Sys.sleep(2)

    showModal(urlModal(
      url = unique_id(),
      title = "Rankings Saved!",
      subtitle = "Your rankings have been saved to server!
      You can look up this particular set of rankings later by searching for this rankings_id. "
    ))
  })

  output$download_rankings <- downloadHandler(
    filename = function() {
      glue::glue("DPRankings_{format(Sys.time(),format = '%Y%m%d%H%M%S')}_{selected_rank_type()}_{selected_position()}.xlsx")
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

  #### Import Rankings ####

  observeEvent(input$import_rankings, {
    showModal(modalDialog(
      easyClose = TRUE,
      title = "Load Rankings ID",
      textInput("import_rankings_id", label = NULL, placeholder = "Rankings ID"),
      footer = list(
        actionButton("import_rankings_load", "Load!", class = "btn-danger"),
        modalButton("Cancel")
      )
    ))
  })

  observeEvent(input$import_rankings_load, {


    req(input$import_rankings_id)
    overwrite_current_rankings <- fn_import_rankings(session,
                                                     input$import_rankings_id,
                                                     df_fantasypros(),
                                                     selected_rank_type(),
                                                     selected_position())

    # REORDER df_fantasypros() by the Imported Rankings
    # SET df_fantasypros to this new Import
    # REPLACE DATA

    df_fantasypros(overwrite_current_rankings)

    replaced_data <- overwrite_current_rankings %>%
      select(
        -ecr_type,
        -ecr_position,
        -fantasypros_id,
        -user_id,
        -user_name,
        -user_nickname,
        -session_timestamp)

    DT::replaceData(proxy = DT::dataTableProxy("rankings_table"),
                    data = replaced_data)

    Sys.sleep(1)

    showModal(modalDialog(
      title = "Success!",
      easyClose = TRUE,
      glue("Imported rankings ID {input$import_rankings_id}, which was previously submitted on: {overwrite_current_rankings$session_timestamp[[1]]} ")
    ))

  })

  #### Load Rankings ####
  load_type <- eventReactive(input$load_rankings, input$toggle_session_history)

  observeEvent(input$load_rankings, {

    if(input$toggle_session_history == "Ranking ID") req(input$session_id)

    showModal(modalDialog("Loading ranks, please wait!"))

    # s_user_id <- "twitter_839942645883551744"
    # s_session_id <- "ac9a0434"

    s_user_id <- str_replace(session[['userData']][["auth0_info"]][["sub"]], "\\|", "_")
    s_session_id <- input$session_id

    loaded_rankings <- load_rankings_from_storage(s_user_id,s_session_id,load_type())

    history_rankings(loaded_rankings)

    Sys.sleep(1)

    removeModal()
  })

  output$download_loaded_rankings <- downloadHandler(
    filename = function() {
      glue::glue(
        "DPRankings",
        "{format(Sys.time(),format = '%Y%m%d%H%M%S')}",
        "{load_type()}",
        "{if_else(load_type()=='My History',session$userData$auth0_info$nickname,input$session_id)}.xlsx",
        .sep = "_")
    },
    content = function(file) {
      history_rankings() %>%
        write_xlsx(file)
    }
  )

  output$history_rankings <- renderDT({

    req(input$load_rankings)

    table_historyrankings(history_rankings())
  })

  output$box_history <- renderUI({

    req(input$load_rankings)
    box_history_table()
  })

  #### Visualization Plots ####

  output$rankings_viz <- renderGirafe({

    req(input$load_rankings)
    req(input$historyplot_playernames)

    girafe(ggobj = plot_rankingsviz(load_type(), history_rankings(), input$historyplot_playernames),
           width_svg = 6,
           height_svg = 3.375,
           options = list(
             opts_selection(type = "single", only_shiny = FALSE)
           ))
  })

  output$box_historyviz <- renderUI({

    req(input$load_rankings)
    req(nrow(history_rankings() > 0))

    player_names <- fn_prepopulate_playernames(history_rankings(),load_type())

    fn_box_historyviz(history_rankings(), player_names)
  })

}

options(shiny.port = 8080)

shinyAppAuth0(ui, server)

# shinyApp(ui, server)
