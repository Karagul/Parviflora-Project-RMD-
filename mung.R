# Munging / Wrangling

#Sys.setlocale(category = "LC_ALL", locale = "Polish")

summary_import <- function(summary_fileList){
  
  fileList <- list.files("data/", full.names = TRUE)
  storeFile <- fileList[str_detect(fileList, "Stores")]
  stores_df <- read_excel(storeFile) %>% arrange(`Store ID`)
  
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
  colNames[colNames == "AZALEA"]      <- "AZALEA_REVENUE"
  colNames[colNames == "BEGONIA"]     <- "BEGONIA_REVENUE"
  colNames[colNames == "CARNATION"]   <- "CARNATION_REVENUE"
  colNames[colNames == "DAFFODIL"]    <- "DAFFODIL_REVENUE"
  colNames[colNames == "TOTAL"]       <- "TOTAL_REVENUE"
  
  colnames(summary_df) <- colNames
  
  summary_df$STORE_ID <- str_sub(summary_df$STORE_ID, -3)

  summary_df <- summary_df[,c("STORE_ID",
                              "STORE_NAME",
                              "AZALEA_COUNT",
                              "AZALEA_REVENUE",
                              "BEGONIA_COUNT",
                              "BEGONIA_REVENUE",
                              "CARNATION_COUNT",
                              "CARNATION_REVENUE",
                              "DAFFODIL_COUNT",
                              "DAFFODIL_REVENUE",
                              "TOTAL_COUNT",
                              "TOTAL_REVENUE")]
  
  
  # POLISH LETTERS ----
  
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
  
  summary_df    <- merge(summary_df, stores_df, by.x = "STORE_ID",
                       by.y = "Store ID", all = TRUE)
  
  discrepancies <- summary_df[(is.na(summary_df$STORE_NAME) == TRUE) | (is.na(summary_df$`Store Name`) == TRUE),
                              c("STORE_ID", "STORE_NAME", "Store Name")]
  
  matches       <- drop_na(summary_df)

  summary_df    <- arrange(summary_df[-(which(is.na(summary_df$STORE_NAME))), -ncol(summary_df)], STORE_NAME)
  
  
  summary_df    <- list(summary_df, 
                     discrepancies,
                     matches,
                     summary_fileList)
  
  
  rm(colNames, i, path, tmp1)
  return(summary_df)
}
  
