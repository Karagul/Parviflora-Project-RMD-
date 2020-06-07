# Munging / Wrangling


summary_import    <- function(summary_fileList, loc_index){
  
  
  loc_index$Location <- stri_trans_general(loc_index$`Store Name`, "Latin-ASCII") %>% str_to_upper() %>% 
                        str_replace(pattern = "PARVIFLORA ", replacement = "")
  
  loc_index   <- arrange(loc_index, Location)
  
  loc_index   <- add_row(loc_index, 
                        `Store ID`   = 299,
                        `Store Name` = "Parviflora",
                         Location    = "PARVIFLORA",
                        .before = 1)
  
  
  summary_df <- read_csv(summary_fileList, locale = locale(encoding = "ISO-8859-1"), col_types = cols(
                        `STORE NAME` = col_character(),
                        `STORE #`    = col_character()))
  
  colNames <-colnames(summary_df)
  tmp1 <- 0
  
  for (i in 1:ncol(summary_df)){
    
    if((str_detect(colNames[i], "^COUNT") == TRUE) & i == 1){
      path <- 1
      break
    }
    
    else if((str_detect(colNames[i], "^COUNT") == TRUE) & i != 1){
      
      if(str_detect(colNames[i-1], '^AZALEA') == TRUE    |
         str_detect(colNames[i-1], "^BEGONIA") == TRUE   |
         str_detect(colNames[i-1], "^CARNATION") == TRUE |
         str_detect(colNames[i-1], "^DAFFODIL") == TRUE  |
         str_detect(colNames[i-1], "^TOTAL") == TRUE)    {
        
        path <- 2
        break
      }
      else if(str_detect(colNames[i+1], '^AZALEA') == TRUE    |
              str_detect(colNames[i+1], "^BEGONIA") == TRUE   |
              str_detect(colNames[i+1], "^CARNATION") == TRUE |
              str_detect(colNames[i+1], "^DAFFODIL") == TRUE  |
              str_detect(colNames[i+1], "^TOTAL") == TRUE)    {
        
        path <- 1
        break
      }
      else{ 
        stop("Column names do not meet requirments. See function description")
      }
    }
  }
  
  for (i in 1:ncol(summary_df)){
    if((str_detect(colNames[i], "^COUNT") == TRUE) & i == 1){
      
      tmp1 <- tmp1 + 1
      
      if(str_detect(colNames[i+1], "^AZALEA") == TRUE){
        colNames[i] <- "AZALEA_COUNT"
      }
      else if(str_detect(colNames[i+1], "^BEGONIA") == TRUE){
        colNames[i] <- "BEGONIA_COUNT"
      }
      else if(str_detect(colNames[i+1], "^CARNATION") == TRUE){
        colNames[i] <- "CARNATION_COUNT"
      }
      else if(str_detect(colNames[i+1], "^DAFFODIL") == TRUE){
        colNames[i] <- "DAFFODIL_COUNT"
      }
      else if(str_detect(colNames[i+1], "^TOTAL") == TRUE){
        colNames[i] <- "TOTAL_COUNT"
      }
      else{ 
        stop("Column names do not meet requirements. See function description")
      }
    }
    
    else if((str_detect(colNames[i], "^COUNT") == TRUE) & i != 1 & path == 2){
      
      tmp1 <- tmp1 + 1
      if(str_detect(colNames[i-1], '^AZALEA') == TRUE){
        colNames[i] <- "AZALEA_COUNT"
      }
      else if(str_detect(colNames[i-1], "^BEGONIA") == TRUE){
        colNames[i] <- "BEGONIA_COUNT"
      }
      else if(str_detect(colNames[i-1], "^CARNATION") == TRUE){
        colNames[i] <- "CARNATION_COUNT"
      }
      else if(str_detect(colNames[i-1], "^DAFFODIL") == TRUE){
        colNames[i] <- "DAFFODIL_COUNT"
      }
      else if(str_detect(colNames[i-1], "^TOTAL") == TRUE){
        colNames[i] <- "TOTAL_COUNT"
      }
      else{ 
        stop("Column names do not meet requirements. See function description")
      }
    }
    
    else if((str_detect(colNames[i], "^COUNT") == TRUE) & i != 1 & path == 1){
      
      tmp1 <- tmp1 + 1
      if(str_detect(colNames[i+1], '^AZALEA') == TRUE){
        colNames[i] <- "AZALEA_COUNT"
      }
      else if(str_detect(colNames[i+1], "^BEGONIA") == TRUE){
        colNames[i] <- "BEGONIA_COUNT"
      }
      else if(str_detect(colNames[i+1], "^CARNATION") == TRUE){
        colNames[i] <- "CARNATION_COUNT"
      }
      else if(str_detect(colNames[i+1], "^DAFFODIL") == TRUE){
        colNames[i] <- "DAFFODIL_COUNT"
      }
      else if(str_detect(colNames[i+1], "^TOTAL") == TRUE){
        colNames[i] <- "TOTAL_COUNT"
      }
      else{ 
        stop("Column names do not meet requirements. See function description")
      }
      
    }
  }
  
  if(tmp1 != 5){
    stop("Unexpected file structure.")
  }
  
  colNames[colNames == "STORE #"]     <- "STORE_ID"
  colNames[colNames == "STORE NAME"]  <- "STORE_NAME"
  colNames[colNames == "AZALEA"]      <- "AZALEA_TOTAL"
  colNames[colNames == "BEGONIA"]     <- "BEGONIA_TOTAL"
  colNames[colNames == "CARNATION"]   <- "CARNATION_TOTAL"
  colNames[colNames == "DAFFODIL"]    <- "DAFFODIL_TOTAL"
  colNames[colNames == "TOTAL"]       <- "TOTAL_VALUE"
  
  colnames(summary_df) <- colNames
  
  summary_df$STORE_ID <- str_sub(summary_df$STORE_ID, -3)
  
  
  # POLISH ENCODING ----
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "\\?OM\\?A")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "\\?OM\\?A")],
                pattern = "\\?OM\\?A", 
                replacement = "LOMZA")
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "\\?WIEBODZIN")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "\\?WIEBODZIN")],
                pattern = "\\?WIEBODZIN", 
                replacement = "SWIEBODZIN")
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "OSTRO\\?\\?KA")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "OSTRO\\?\\?KA")],
                pattern = "OSTRO\\?\\?KA", 
                replacement = "OSTROLEKA")
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "\\?ÓD\\?")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "\\?ÓD\\?")],
                pattern = "\\?ÓD\\?", 
                replacement = "LÓDZ")
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "GDA\\?SK")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "GDA\\?SK")],
                pattern = "GDA\\?SK", 
                replacement = "GDANSK")
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "POZNA\\?")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "POZNA\\?")],
                pattern = "POZNA\\?", 
                replacement = "POZNAN")
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "SUWA\\?KI")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "SUWA\\?KI")],
                pattern = "SUWA\\?KI", 
                replacement = "SUWALKI")
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "TORU\\?")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "TORU\\?")],
                pattern = "TORU\\?", 
                replacement = "TORUN")
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "W\\?CHOCK")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "W\\?CHOCK")],
                pattern = "W\\?CHOCK", 
                replacement = "WACHOCK")
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "BIA\\?YSTOK")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "BIA\\?YSTOK")],
                pattern = "BIA\\?YSTOK", 
                replacement = "BIALYSTOK")
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "WROC\\?AW")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "WROC\\?AW")],
                pattern = "WROC\\?AW", 
                replacement = "WROCLAW")
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "CHE\\?M")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "CHE\\?M")],
                pattern = "CHE\\?M", 
                replacement = "CHELM")
  
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "PRZEMY\\?L")] <- 
    str_replace(summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME, "PRZEMY\\?L")],
                pattern = "PRZEMY\\?L", 
                replacement = "PRZEMYSL")
  
  #----
  
  summary_df$STORE_NAME <- stri_trans_general(summary_df$STORE_NAME, "Latin-ASCII") %>% str_to_upper()
  
  summary_df  <- arrange(summary_df, STORE_NAME)
  
  summary_df$LOCATION     <- NA
  summary_df$LOCATION_ID  <- NA
  summary_df$GROSS        <- 0
  summary_df$RETAIL       <- 0
  summary_df$OTHER        <- 0
  
  for(i in 1:nrow(loc_index)){
    
    pattern <- loc_index$Location[i]
    id      <- loc_index$`Store ID`[i]
    
    for(j in 1:nrow(summary_df)){
      
      if((str_detect(summary_df$STORE_NAME[j], pattern)) == TRUE){
        
        summary_df$LOCATION[j]    <- pattern
        summary_df$LOCATION_ID[j] <- id
      }
    }
  }
  
  for(i in 1:nrow(summary_df)){
    
    if((str_detect(summary_df$STORE_NAME[i], "GROSS")) == TRUE){
      summary_df$GROSS[i] <- 1
    }
    else if((str_detect(summary_df$STORE_NAME[i], "RETAIL")) == TRUE){
      summary_df$RETAIL[i] <- 1
    }
    else if((str_detect(summary_df$STORE_NAME[i], "OTHER")) == TRUE){
      summary_df$OTHER[i] <- 1
    }
  }
  
  summary_df <- summary_df[,c("STORE_ID",
                              "STORE_NAME",
                              "LOCATION",
                              "LOCATION_ID",
                              "GROSS",
                              "RETAIL",
                              "OTHER",
                              "AZALEA_COUNT",
                              "AZALEA_TOTAL",
                              "BEGONIA_COUNT",
                              "BEGONIA_TOTAL",
                              "CARNATION_COUNT",
                              "CARNATION_TOTAL",
                              "DAFFODIL_COUNT",
                              "DAFFODIL_TOTAL",
                              "TOTAL_COUNT",
                              "TOTAL_VALUE")]
  
  summary_df    <- list(summary_df, summary_fileList)
  
  return(summary_df)
}

daffodils_import  <- function(sheets_titles, fileList){
  
  
  
  daffodils <- read_excel(fileList[str_detect(fileList, "Daffodils")], 
                          sheet = sheets_titles,
                          col_names = FALSE,
                          skip = 6)
  

  dateRange   <- as.character(daffodils[1,2])
  
  
  daffodils   <- daffodils[-c(1:3, nrow(daffodils),nrow(daffodils)-1),]
  
  sep_index   <- which(rowSums(is.na(daffodils)) > 4)
  
  daffodils[1:(sep_index[1]-1), c(2,3)] <- daffodils[1:(sep_index[1]-1), c(4,5)]
  daffodils[1:(sep_index[1]-1), c(4,5)] <- NA
  daffodils <- daffodils[,-c(4,5)]
  
  colnames(daffodils) <- c("ENTRY_CAT", 
                           "DAFFODILS_TOTAL", 
                           "DAFFODILS_COUNT")
  
  daffodils$DAFFODILS_TOTAL <- daffodils$DAFFODILS_TOTAL %>% as.numeric
  daffodils$DAFFODILS_COUNT <- daffodils$DAFFODILS_COUNT %>% as.numeric
  
  
  listDaffo <- vector("list", length(sep_index))
  container <- vector("list", 2)
  peg <- 1
  index <-1
  
  for(i in sep_index){
    
    container[[1]] <- daffodils[peg:(i-1),]
    container[[2]] <- container[[1]][1:3,]
    container[[1]] <- container[[1]][-c(1:3),]
    container[[1]] <- drop_na(container[[1]])
    container[[1]] <- arrange(container[[1]], ENTRY_CAT)
    
    if(is.na(container[[2]][1,1]) != TRUE){
      
      container[[2]] <- c("SUMMARY", dateRange, sheets_titles)
      
    }
    else if(rowSums(is.na(container[[2]][1,])) == 3){
      
      container[[2]] <- c(as.character(container[[2]][2,c("DAFFODILS_COUNT")]), dateRange, sheets_titles)
      
    }
    
    listDaffo[[index]] <- container
    peg <- i
    index <- index + 1
    
  }
  
  return(listDaffo)
  
}
  
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