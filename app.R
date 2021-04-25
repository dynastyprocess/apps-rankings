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
              column(
                width = 6,
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
                )),
              column(6,
                     conditionalPanel(
                       condition = "input.toggle_session_history == 'Ranking ID'",
                       textInput("session_id",
                                 label = NULL,
                                 placeholder = "Session ID")
                     )
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

    # CHECK IF RANKINGS ID EXISTS

    imported_rankings <- open_dataset("storage") %>%
      filter(session_id == input$import_rankings_id) %>%
      collect()

    if(nrow(imported_rankings)==0) return(
      showModal(modalDialog(
        title = "Error",
        glue::glue("Could not find Rankings ID {input$import_rankings_id} in our database")
      ))
    )

    # CHECK IF RANKINGS ID TYPE MATCHES THE CURRENT TYPE


    if(imported_rankings$ecr_type[[1]] != input$rank_type |
       imported_rankings$ecr_position[[1]] != input$position) {

      return(
        showModal(modalDialog(
          title = "Error",
          glue::glue("The imported rankings are for
                     {imported_rankings$ecr_type[[1]]} - {imported_rankings$ecr_position[[1]]}
                     and not for the currently selected {input$rank_type} - {input$position}")
        ))
      )
    }

    # REORDER df_fantasypros() by the Imported Rankings
    # SET df_fantasypros to this new Import
    # REPLACE DATA

    overwrite_current <- df_fantasypros() %>%
      select(-`Your Rank`) %>%
      left_join(
        imported_rankings %>% select(fantasypros_id,`Your Rank`),
        by = "fantasypros_id"
      ) %>%
      arrange(`Your Rank`) %>%
      relocate(`Your Rank`, .after = Age) %>%
      mutate(Z = round((`FP Rank` - `Your Rank`) /SD, 1))

    df_fantasypros(overwrite_current)

    replaced_data <- overwrite_current %>%
      select(
        -ecr_type,
        -ecr_position,
        -fantasypros_id,
        -user_id,
        -user_name,
        -user_nickname)

    DT::replaceData(proxy = DT::dataTableProxy("rankings_table"),
                    data = replaced_data)

    Sys.sleep(1)

    showModal(modalDialog(
      title = "Success!",
      easyClose = TRUE,
      glue("Imported rankings ID {input$import_rankings_id}, which was previously submitted on: {imported_rankings$session_timestamp[[1]]} ")
    ))

  })

  #### Load Rankings ####
  load_type <- eventReactive(input$load_rankings, input$toggle_session_history)

  observeEvent(input$load_rankings, {

    showModal(modalDialog("Loading ranks, please wait!"))

    storage <- arrow::open_dataset(sources = "storage")

    # s_user_id <- "twitter_839942645883551744"
    # s_session_id <- "ef7be5f8"

    s_user_id <- str_replace(session[['userData']][["auth0_info"]][["sub"]], "\\|", "_")
    s_session_id <- input$session_id


    if(load_type() == "My History") {

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

    if(load_type() == "Ranking ID"){

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

    history_rankings(loaded_rankings)

    Sys.sleep(1)

    removeModal()
  })

  output$history_rankings <- renderDT({

    req(input$load_rankings)

    colourlist <- colorRampPalette(brewer.pal(3, "PRGn"))

    history_rankings() %>%
      mutate_at(c("rankings_id","Player Name","Pos","fantasypros_id"), as.factor) %>%
      datatable(
        filter = "top",
        options = list(
          scrollX = TRUE
        ),
        class = "compact stripe nowrap",
        rownames = FALSE,
        selection = "none"
      ) %>%
      formatRound(c("Z", "SD"), 1) %>%
      formatDate("Timestamp") %>%
      formatStyle(columns = 1:16,
                  valueColumns = "Z",
                  backgroundColor = styleInterval(
                    quantile(range(-3, 3),
                             probs = seq(0.05, 0.95, 0.05),
                             na.rm = TRUE),
                    colourlist(20)))

  })

  output$box_history <- renderUI({

    req(input$load_rankings)

    box(
      width = 12,
      title = "Rankings Lookup",
      status = "danger",
      DTOutput("history_rankings")
    )

  })

  #### Visualization Plots ####

  output$rankings_viz <- renderGirafe({

    req(input$load_rankings)

    req(input$historyplot_playernames)

    if(load_type() == "My History") history_plot <- plot_myhistory(history_rankings(), input$historyplot_playernames)

    if(load_type() == "Ranking ID") history_plot <- plot_sessionID(history_rankings(), input$historyplot_playernames)

    girafe(ggobj = history_plot,
           width_svg = 8,
           height_svg = 4.5,
           options = list(
             opts_selection(type = "single", only_shiny = FALSE)
           ))
    # girafe(ggobj = history_plot)
  })

  output$box_historyviz <- renderUI({

    req(input$load_rankings)
    req(nrow(history_rankings() > 0))


    if(load_type() == "My History") {
      player_names <- history_rankings() %>%
        arrange(desc(Z)) %>%
        distinct(`Player Name`, .keep_all = TRUE) %>%
        slice(1:3) %>%
        pull(`Player Name`)
    }

    if(load_type() == "Ranking ID") {
      player_names <-history_rankings() %>%
        arrange(Z) %>%
        distinct(`Player Name`, .keep_all = TRUE) %>%
        slice(1:5, nrow(.)-5:nrow(.)) %>%
        pull(`Player Name`)
    }

    tagList(
      pickerInput(
        "historyplot_playernames",
        label = "Select Players",
        choices = unique(history_rankings()$`Player Name`),
        selected = player_names,
        multiple = TRUE,
        options = list(
          `max-options` = 10,
          `live-search` = TRUE,
          `actions-box` = TRUE,
          `count-selected-text` = "count > 0"
        ),
      ),
      br(),
      girafeOutput("rankings_viz")
    )

  })

  plot_myhistory <- function(history_rankings, player_names = NULL){

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
      geom_line(stat = "smooth", alpha = 0.5, method = "glm", formula = 'y ~ x', se = FALSE) +
      labs(
        title = glue("Your Rankings vs FantasyPros"),
        subtitle = "_measured in Z-score (standard deviations above/below FP average)_") +
      xlab(NULL) +
      ylab(NULL) +
      theme(legend.position = "bottom",
            plot.subtitle = element_markdown(),
            plot.title.position = "plot")

    # girafe(ggobj = df_plot,
    #        width_svg = 8,
    #        height_svg = 4.5,
    #        options = list(
    #          opts_selection(type = "single", only_shiny = FALSE)
    #        ))

    return(df_plot)
  }

  plot_sessionID <- function(history_rankings, player_names = NULL){
    # BarPlot
  }



}

options(shiny.port = 8080)

shinyAppAuth0(ui, server)

# shinyApp(ui, server)
