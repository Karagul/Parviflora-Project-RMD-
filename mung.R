# Munging / Wrangling

Sys.setlocale(category = "LC_ALL", locale = "Polish")

summary_import <- function(summary_fileList){
  
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
  summary_df$FILE_NAME <- summary_fileList
  
  summary_df <- summary_df[,c("FILE_NAME",
                              "STORE_NAME", 
                              "STORE_ID",
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
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA \\?OM\\?A")] <- "PARVIFLORA LOMZA"
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA \\?WIEBODZIN")] <- "PARVIFLORA SWIEBODZIN"
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA OSTRO\\?\\?KA")] <- "PARVIFLORA OSTROLEKA"
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA \\?ÓD\\?")] <- "PARVIFLORA LÓDZ"
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA GDA\\?SK")] <- "PARVIFLORA GDANSK"
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA POZNA\\?")] <- "PARVIFLORA POZNAN"
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA SUWA\\?KI")] <- "PARVIFLORA SUWALKI"
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA TORU\\?")] <- "PARVIFLORA TORUN"
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA W\\?CHOCK")] <- "PARVIFLORA WACHOCK"
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA BIA\\?YSTOK")] <- "PARVIFLORA BIALYSTOK"
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA WROC\\?AW")] <- "PARVIFLORA WROCLAW"
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA CHE\\?M")] <- "PARVIFLORA CHELM"
  summary_df$STORE_NAME[str_detect(summary_df$STORE_NAME,
                                   "PARVIFLORA PRZEMY\\?L")] <- "PARVIFLORA PRZEMYSL"
  
  #----
  
  rm(colNames, i, path, tmp1)
  return(summary_df)
}
  
