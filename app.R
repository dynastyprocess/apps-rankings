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
        fluidRow(
          box_inputs(),
        ),
        fluidRow(
          uiOutput("rankings"),
        ),
        br(),
        actionButton("debug", label = "debug")
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

  output$download_rankings <- downloadHandler(
    filename = function() {
      glue::glue("DPRankings_{Sys.time()}_{input$rank_type}_{input$position}.xlsx")
    },
    content = function(file) {
      write_xlsx(df_fantasypros(), file)
    }
  )

  #### Update Pickers ####

  observeEvent(input$rank_type, {

    if (str_detect(input$rank_type, "Dynasty Overall")) {
      updatePickerInput(session, "position", choices = c("Rookies","All Offense"))
    }

    if (str_detect(input$rank_type, "Redraft Overall")) {
      updatePickerInput(session, "position", choices = c("All Offense", "All Defense"))
    }

    if (str_detect(input$rank_type, "Position")) {
      updatePickerInput(session, "position", choices = c("QB", "RB", "WR", "TE", "DL", "LB", "DB"))
    }
  })

  #### Load Data ####

  df_fantasypros <- reactiveVal()
  init_df <- reactiveVal()
  replaced_df <- reactiveVal()

  nrow_df <- reactiveVal()

  my_order <- reactiveVal()

  observeEvent(input$load, {

    loaded_df <- load_fpdata(input$rank_type, input$position) %>%
      mutate(
        `Your Rank` = row_number(),
        Z = round((ecr - `Your Rank`) / sd, 1),
        sd = round(sd, 1),
        ecr_type = input$rank_type,
        ecr_position = input$position,
        user_id = session$userData$auth0_info$sub,
        user_nickname = session$userData$auth0_info$nickname,
        user_name = session$userData$auth0_info$name,
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

    df_fantasypros(loaded_df)

    init_df(loaded_df)

    nrow_df(nrow(df_fantasypros()))
    my_order(seq(1, nrow_df()))

    Sys.sleep(2)

    updatebs4Card("box_inputs",session = session, action = "toggle")
  })

  #### Datatable ####

  colourlist <- colorRampPalette(brewer.pal(3, "PRGn"))

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
  })


  observeEvent(input$row_reorder, { # watching the "reorder" events ----

    apply_reorder <- function(order_info, full_rankings, ranking_length){

      if (is.null(order_info) || class(order_info) != "character") {
        return()
      }

      order_info <- read_yaml(text = order_info)

      if(length(order_info)==0){
        return()
      }
      # browser()

      # old_order <- seq_len(nrow_df())
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

    unique_id <- uuid::UUIDgenerate(use.time = TRUE) %>%
      str_sub(end = 8)

    file_name <- glue::glue("{unique_id}_{{i}}.parquet")

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

    removeModal()

  })



}

# options(shiny.port = 8080)

shinyAppAuth0(ui, server)
# shinyApp(ui, server)
#
