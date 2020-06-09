# Source code

# Libraries ----

install.packages("pacman")
pacman::p_load(pacman,tidyverse,stringr, readr,readxl,lubridate,stringi,zoo)
source("mung.R")

# Data Import ----

fileList <- list.files("data/", full.names = TRUE)
summary_fileList <- as.list(fileList[str_detect(fileList, "Summary of Sales")])
sheets_titles <- as.list(excel_sheets(fileList[str_detect(fileList, "Daffodils")]))


store_index <- read_excel(fileList[str_detect(fileList, "Stores")])
summaries   <- lapply(summary_fileList, summary_import, loc_index = store_index)
daffodils   <- lapply(sheets_titles, daffodils_import, fileList = fileList)

daffo_aggr  <- lapply(daffodils, daffodils_aggregator)

merged      <- lapply(summaries, merger, daffo_aggr)

# Output Views ----

view(summaries[[3]][1])
view(daffodils[[1]][[2]][1])
view(daffo_aggr[[3]][1])
view(merged[[3]][[1]])

# Export ----

write_excel_csv(merged[[3]][[1]], merged[[3]][[2]])