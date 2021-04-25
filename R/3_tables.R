table_historyrankings <- function(history_rankings){

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
}

rankings_datatable <- function(input_df){

  colourlist <- colorRampPalette(brewer.pal(3, "PRGn"))

  input_df %>%
    datatable(
      extensions = "RowReorder",
      selection = "none",
      options = list(
        rowReorder = list(selector = "tr"),
        order = list(c(5, "asc")),
        dom = "ftir",
        paging = FALSE,
        searching = FALSE,
        # scrollY = "50vh",
        scrollX = TRUE
      ),
      callback = JS("table.on('row-reorder',function(e, details, all){
                      Shiny.onInputChange('row_reorder', JSON.stringify(details));
                      });")
    ) %>%
    formatRound(c("Z", "SD"), 1) %>%
    formatStyle(columns = 0:11,
                valueColumns = "Z",
                backgroundColor = styleInterval(
                  quantile(range(-3, 3),
                           probs = seq(0.05, 0.95, 0.05),
                           na.rm = TRUE),
                  colourlist(20)))
}
