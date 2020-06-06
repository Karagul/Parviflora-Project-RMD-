# Source code

#Sys.setlocale(category = "LC_ALL", locale = "Polish")

# Libraries ----

#install.packages("pacman")
pacman::p_load(pacman,tidyverse,stringr, readr,readxl,lubridate,stringi)
source("mung.R")

# Data Import ----

fileList <- list.files("data/", full.names = TRUE)
summary_fileList <- as.list(fileList[str_detect(fileList, "Summary of Sales")])
sheets_titles <- as.list(excel_sheets(fileList[str_detect(fileList, "Daffodils")]))


store_index <- read_excel(fileList[str_detect(fileList, "Stores")])
summaries   <- lapply(summary_fileList, summary_import, loc_index = store_index)
daffodils   <- lapply(sheets_titles, daffodils_import, fileList = fileList)


# Views
view(daffodils[[1]][[2]][1])
view(summaries[[2]][1])








# DAFFO------

fileList <- list.files("data/", full.names = TRUE)

sheets_titles <- as.list(excel_sheets(fileList[str_detect(fileList, "Daffodils")]))

daffodils   <- read_excel(fileList[str_detect(fileList, "Daffodils")], 
                          sheet = 1,
                          col_names = FALSE,
                          skip = 6)

dateRange   <- as.character(daffodils[1,2])

daffodils   <- daffodils[-c(1:3, nrow(daffodils),nrow(daffodils)-1),]

sep_index   <- which(rowSums(is.na(daffodils)) > 4)

daffodils[1:(sep_index[1]-1), c(2,3)] <- daffodils[1:(sep_index[1]-1), c(4,5)]
daffodils[1:(sep_index[1]-1), c(4,5)] <- NA

#validation point (get it work)
#daffodils <- daffodils[,names(which(colSums(is.na(daffodils)) == nrow(daffodils)))]

daffodils <- daffodils[,-c(4,5)]
colnames(daffodils) <- c("ENTRY_CAT", 
                         "DAFFODILS_TOTAL", 
                         "DAFFODILS_COUNT")

daffodils$DAFFODILS_TOTAL <- daffodils$DAFFODILS_TOTAL %>% as.numeric
daffodils$DAFFODILS_COUNT <- daffodils$DAFFODILS_COUNT %>% as.numeric


listDaffo <- vector("list", length(sep_index))
container <- vector("list", 2)

cap <- 1
index <-1


for(i in sep_index){
  
  container[[1]] <- daffodils[cap:(i-1),]
  container[[2]] <- container[[1]][1:3,]
  container[[1]] <- container[[1]][-c(1:3),]
  container[[1]] <- drop_na(container[[1]])
  container[[1]] <- arrange(container[[1]], ENTRY_CAT)
  
  if(is.na(container[[2]][1,1]) != TRUE){
    
    container[[2]] <- "SUMMARY"
    
  }
  else if(rowSums(is.na(container[[2]][1,])) == 3){
    
    container[[2]] <- as.character(container[[2]][2,c("DAFFODILS_COUNT")])
    
  }
  
  listDaffo[[index]] <- container
  cap <- i
  index <- index + 1
  
}




##### tests-----

stri_trans_general(stores_df$`Store Name`, "Latin-ASCII")

Encoding(summary_df$`STORE NAME`[1])
iconv(summary_df$STORE_NAME[1], from = "", to = "UTF-8")

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





