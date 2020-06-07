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

daffo_aggr  <- lapply(daffodils, daffodils_aggregator)


# Views

view(summaries[[2]][1])
view(daffodils[[1]][[2]][1])
view(daffo_aggr[[3]][1])








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

## Daffo aggregator ----


title <- daffodils[[1]][[1]][[2]][[3]]

ENTRY_CAT <- c(unique(daffodils[[1]][[1]][[1]][[1]]), "Totals")

aggr <- tibble(data.frame(matrix(nrow = length(ENTRY_CAT), ncol = 2 * length(daffodils[[1]]))))
aggr <- cbind(ENTRY_CAT, aggr)
aggr$ENTRY_CAT <- as.character(aggr$ENTRY_CAT)

m <- 1

for(j in seq(2, 2*length(daffodils[[1]]),2)){
  
  for(i in 1:nrow(aggr)){
    
    id <- daffodils[[1]][[m]][[2]][1]
    colnames(aggr)[j]   <- str_c(id, "TOTAL", sep = "_")
    colnames(aggr)[j+1] <- str_c(id, "COUNT", sep = "_")
    
    tmp <- aggr$ENTRY_CAT[i]
    aggr[i,j:(j+1)] <- colSums(daffodils[[1]][[m]][[1]][daffodils[[1]][[m]][[1]][,1] == tmp, 2:3])
    
  }
  m <- m + 1
  
  aggr[aggr$ENTRY_CAT == "Totals",2:3] <- aggr[aggr$ENTRY_CAT == "Summary Totals",2:3]
  aggr <- aggr[-which(aggr$ENTRY_CAT == "Summary Totals"), ]
  
}

### daffo aggregator lapply version -----


daffodils_aggregator <- function(daffodils){
  
  title <- daffodils[[1]][[2]][[3]]
  
  ENTRY_CAT <- c(unique(daffodils[[1]][[1]][[1]]), "Totals")
  
  aggr <- tibble(data.frame(matrix(nrow = length(ENTRY_CAT), ncol = 2 * length(daffodils))))
  aggr <- cbind(ENTRY_CAT, aggr)
  aggr$ENTRY_CAT <- as.character(aggr$ENTRY_CAT)
  
  m <- 1
  
  for(j in seq(2, 2*length(daffodils),2)){
    
    for(i in 1:nrow(aggr)){
      
      id <- daffodils[[m]][[2]][1]
      colnames(aggr)[j]   <- str_c(id, "TOTAL", sep = "_")
      colnames(aggr)[j+1] <- str_c(id, "COUNT", sep = "_")
      
      tmp <- aggr$ENTRY_CAT[i]
      aggr[i,j:(j+1)] <- colSums(daffodils[[m]][[1]][daffodils[[m]][[1]][,1] == tmp, 2:3])
      
    }
    m <- m + 1
    
  }
  
  aggr[aggr$ENTRY_CAT == "Totals",2:3] <- aggr[aggr$ENTRY_CAT == "Summary Totals",2:3]
  aggr <- aggr[-which(aggr$ENTRY_CAT == "Summary Totals"), ]
  
  
  aggr$GROSS  <- 0
  aggr$RETAIL <- 0
  aggr$OTHER  <- 0
  
  for(i in 1:nrow(aggr)){
    
    if((str_detect(aggr$ENTRY_CAT[i], "Consumer Orders")) == TRUE){
      
      aggr$RETAIL[i] <- 1
      
    }
    else if((str_detect(aggr$ENTRY_CAT[i], "Small Business Orders")) == TRUE | 
            (str_detect(aggr$ENTRY_CAT[i], "Corporate Orders"))      == TRUE){
      
      aggr$GROSS[i] <- 1
      
    }
    else if((str_detect(aggr$ENTRY_CAT[i], "Totals")) == TRUE){
      
      next
      
    }
    else{
      
      aggr$OTHER[i]  <- 1
      
    }
  }
  
  
  aggr <- list(aggr, title)
  
  return(aggr)
  
}


test <- lapply(daffodils, daffodils_aggregator)


### ----

gag <- function(x){
  
  ENTRY_CAT <- x[[1]][[1]][[1]]
  return(ENTRY_CAT)
}

lel <- lapply(daffodils, gag)


lel2 <- lapply(daffodils, `[[`)






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





