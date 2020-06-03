# Source code

Sys.setlocale(category = "LC_ALL", locale = "Polish")

# Libraries ----

#install.packages("pacman")
pacman::p_load(pacman,tidyverse,stringr, readr,readxl,lubridate)
source("mung.R")

# Data Import ----

fileList <- list.files("data/", full.names = TRUE)
summary_fileList <- as.list(fileList[str_detect(fileList, "Summary of Sales")])

stores_df <- read_excel(fileList[str_detect(fileList, "Stores")]) 
summaries <- lapply(summary_fileList, summary_import)




# tests ------
heh2 <- summaries[[1]][[1]]
heh <- arrange(summaries[[1]][[1]], STORE_NAME)
discrepancies <- heh[(is.na(heh$STORE_NAME) == TRUE) | (is.na(heh$`Store Name`) == TRUE), c("STORE_ID", "STORE_NAME", "Store Name")]

heh$STORE_NAME[is.na(heh$`Store Name`) == FALSE] <- heh$`Store Name`[is.na(heh$`Store Name`) == FALSE]


#heh <- summaries[[1]]
#print("LOMZA")
#summary_df <- read_csv(fileList[[4]], locale = locale(encoding = "ISO-8859-1"), col_types = cols(
#  `STORE NAME` = col_character(),
#  `STORE #`    = col_character()))
#summary_df$`STORE NAME`[str_detect(summary_df$`STORE NAME`,"PARVIFLORA \\?OM\\?A")] <- "PARVIFLORA LOMZA"
#summary_df$`STORE NAME`[str_detect(summary_df$`STORE NAME`,"\\?")]





