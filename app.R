options(dplyr.summarise.inform = FALSE)

ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  title = "Rankings App - DynastyProcess.com",
  navbar = ui_header(
    "Rankings App - DynastyProcess.com"
    # rightUi = tagList(
    # uiOutput("user_menu")
    # bs4UserMenu(
    #   name = textOutput("user_nickname"),
    #   title = textOutput("user_nickname"),
    #   subtitle = textOutput("user_name"),
    #   src = textOutput("user_picture"),
    #   status = "danger"
    # )
    # )
  ),
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
        fluidRow(
          uiOutput("rankings"),
          uiOutput("user_info")
          # box(
          #   width = 3,
          #   inputId = 'box_actions',
          #   status = "danger",
          #   title = "Save/Download",
          #   br(),
          #   footer = list(
          #     actionButton("save", "Save", icon = icon("space-shuttle"), class = "btn-primary"),
          #     downloadButton("download", class = "btn-success")
          #   )


          # )
        ),
        br(),
        actionButton('debug',label = "debug")
      )
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$debug, browser())

  sever_dp()

  output$user_info <- renderUI({
    # column(12,
           bs4UserCard(
             status = "danger",
             width = 12,
             src = session$userData$auth0_info$picture,
             title = session$userData$auth0_info$nickname,
             subtitle = session$userData$auth0_info$name,
             actionButton("save", "Save", icon = icon("space-shuttle"), class = "btn-primary"),
             downloadButton("download_rankings", class = "btn-success")
           )
  })

  output$download_rankings <- downloadHandler(
    filename = function(){
      glue::glue("DPRankings_{Sys.time()}_{input$rank_type}_{input$position}.xlsx")
      },
    content = function(file){
      write_xlsx(
        df_fantasypros(),
        file
      )
    })

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

  nrow_df <- reactiveVal()

  my_order <- reactiveVal()

  observeEvent(input$load, {

    load_fpdata(input$rank_type,input$position) %>%
      mutate(`Your Rank` = row_number(),
             Z = round((ecr-`Your Rank`)/sd,1),
             sd = round(sd,1),
             user_id = session$userData$auth0_info$sub,
             user_nickname = session$userData$auth0_info$nickname,
             user_name = session$userData$auth0_info$name,
      ) %>%
      select(`Scrape Date` = scrape_date,
             `Player Name` = player_name,
             Pos = pos,
             Team = tm,
             `Your Rank`,
             `FP Rank` = ecr,
             Z,
             SD = sd,
             Best = best,
             Worst = worst,
             fantasypros_id,
             ecr_type,
             user_id,
             user_name,
             user_nickname
             ) %>%
      df_fantasypros()

    nrow_df(nrow(df_fantasypros()))
    my_order(seq(1, nrow_df()))

    Sys.sleep(2)

    updatebs4Card("box_inputs",session = session, action = "toggle")

  })

  #### Datatable ####

  colourlist<-colorRampPalette(brewer.pal(3,'PRGn'))

  output$table <- renderDT({

    df_fantasypros() %>%
      select(-ecr_type,
             -fantasypros_id,
             -user_id,
             -user_name,
             -user_nickname) %>%
      datatable(
        rownames = FALSE,
        extensions = 'RowReorder',
        selection = 'none',
        options = list(
          rowReorder = list(selector = 'tr'),
          order = list(c(4, 'asc')),
          paging=FALSE,
          scrollX=TRUE,
          searching=FALSE
        ),
        callback = JS("table.on('row-reorder',function(e, details, all){Shiny.onInputChange('row_reorder', JSON.stringify(details));});")
      ) %>%
      formatRound(c('Z','SD'),1) %>%
      formatStyle(1:10,'Z',backgroundColor=styleInterval(quantile(range(-3,3),probs=seq(0.05,0.95,0.05),na.rm=TRUE),colourlist(20)))

  })

  observeEvent(input$row_reorder,{ # watching the "reorder" events ----

    order_info <- input$row_reorder

    if (is.null(order_info)|class(order_info) !='character'){return()}

    order_info<-read_yaml(text=order_info)

    if(length(order_info)==0){return()}

    old_order <- seq_len(nrow_df())
    new_order <- seq_len(nrow_df())

    # new_order <- my_order() # check if borked

    for (i in 1:length(order_info)) {

      j <- order_info[[i]]

      new_order[(j$oldPosition + 1)] <- j$newPosition + 1
    }

    my_order(new_order)

    new_df <- df_fantasypros() %>%
      select(-`Your Rank`) %>%
      mutate(`Your Rank`= new_order,
             Z = round((`FP Rank`-`Your Rank`)/`SD`,1)) %>%
      arrange(`Your Rank`) %>%
      select(`Scrape Date`,
             `Player Name`,
             Pos,
             Team,
             `Your Rank`,
             `FP Rank`,
             Z,
             SD,
             Best,
             Worst,
             fantasypros_id,
             ecr_type,
             user_id,
             user_name,
             user_nickname)

    df_fantasypros(new_df)

  })

  output$rankings <- renderUI({

    req(df_fantasypros())

    box_rankings(input$rank_type, input$position)

  })

}

options(shiny.port = 8080)
shinyAppAuth0(ui, server)
# shinyApp(ui, server)
