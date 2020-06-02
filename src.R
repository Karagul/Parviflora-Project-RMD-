# Source code

Sys.setlocale(category = "LC_ALL", locale = "Polish")

# Libraries ----

#install.packages("pacman")
pacman::p_load(pacman,tidyverse,stringr, readr,readxl,lubridate)
source("mung.R")

# Data Import ----

fileList <- list.files("data/", full.names = TRUE)

summary_fileList <- as.list(fileList[str_detect(fileList, "Summary of Sales")])

summaries <- lapply(summary_fileList, summary_import)




#summary_df <- read_csv(fileList[[4]], locale = locale(encoding = "ISO-8859-1"), col_types = cols(
#  `STORE NAME` = col_character(),
#  `STORE #`    = col_character()))
#summary_df$`STORE NAME`[str_detect(summary_df$`STORE NAME`,"PARVIFLORA \\?OM\\?A")] <- "PARVIFLORA LOMZA"
#summary_df$`STORE NAME`[str_detect(summary_df$`STORE NAME`,"\\?")]





