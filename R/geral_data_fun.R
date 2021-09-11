## BASE GERAL

geral_data <- function(arquivo){
    janitor::clean_names(readr::read_csv(paste0("data/", arquivo)))
  }
