# fantasypros_raw <-
#   read.csv("https://github.com/dynastyprocess/data/raw/master/files/db_fpecr.csv") %>%
#   filter(
#     scrape_date == max(scrape_date)
#   )
#
# write_parquet(fantasypros_raw, "data/fantasypros_raw.parquet")

load_fpdata <- function(type, position){

  if(position != "Rookies"){

    type_list <- c("Dynasty Positional"= "dp",
                   "Dynasty Overall 1QB"= "do",
                   "Dynasty Overall SF"= "dsf",
                   "Redraft Positional"= "rp",
                   "Redraft Overall 1QB"= "ro",
                   "Redraft Overall SF"= "rsf")

    type <- type_list[type]
  }

  if(position == "Rookies") type <- "drk"

  fantasypros_raw <- read_parquet("data/fantasypros_raw.parquet") %>%
    filter(ecr_type == type) %>%
    mutate(sd = case_when(is.null(sd)~ecr^0.6,
                          sd == 0 ~ ecr^0.6,
                          # ecr > 32 ~ ecr^0.6,
                          TRUE ~ sd))

  if(str_detect(type, "o$|sf$|rk$")) {

    if(position == "All Offense") {
      fantasypros_raw <- fantasypros_raw %>%
        filter(pos %in% c("QB","RB","WR","TE"))
    }

    if(position == "All Defense"){
      fantasypros_raw <- fantasypros_raw %>%
        filter(pos %in% c("DL","LB","DB"))
    }

    return(fantasypros_raw)
  }

  fantasypros_raw <- fantasypros_raw %>%
    filter(pos == position)

  return(fantasypros_raw)
}
