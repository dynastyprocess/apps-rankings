library(arrow)
library(dplyr)
library(here)
library(ffscrapr)

update_local_data <- function(){
  download.file(
    url = "https://github.com/dynastyprocess/data/raw/master/files/db_fpecr.parquet",
    destfile = here("data/db_fpecr.parquet"))

  fantasypros_raw <- read_parquet(here("data/db_fpecr.parquet")) %>%
    filter(
      scrape_date == max(scrape_date),
      ecr_type %in% c("dp",  "do",  "dsf", "drk", "rp",  "ro",  "rsf")
    ) %>%
    left_join(
      dp_playerids() %>% select(fantasypros_id,age),
      by = c("fantasypros_id")
    )

  write_parquet(fantasypros_raw, here("data/fantasypros_raw.parquet"))

  return(glue::glue("Successfully updated at {Sys.time()}"))
}

update_local_data()
