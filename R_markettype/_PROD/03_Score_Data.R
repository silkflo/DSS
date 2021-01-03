# ----------------------------------------------------------------------------------------
# R Script to score current period of each currency based on the newly entered data
# ----------------------------------------------------------------------------------------

# Market Periods
# 1. Bull normal, BUN
# 2. Bull volatile, BUV
# 3. Bear normal, BEN
# 4. Bear volatile, BEV
# 5. Sideways quiet, RAN
# 6. Sideways volatile, RAV


# Expected output: Table containing market type number for every of the 28 currency pairs written as files to the sandboxes of terminals
library(dplyr)
library(magrittr)
library(lubridate)
library(readr)
library(h2o)
library(lazytrade)
source("C:/DSS/Function/All_Path.R")
#path to user repo:
#!!!Change this path!!! 
path_user <- Path()$marketType

path_sbx <-  Path()$pathT2

path_sbxm <- Path()$pathT1
path_sbxd <- Path()$pathT2 #development terminal
path_sbxs <- Path()$pathT3
# Define variables for the functions to work
chart_period <- 5 #this variable will define market type period
num_cols <- 32

#absolute path to store model objects (useful when scheduling tasks)
path_model <- file.path(path_user, "_MODELS")
path_data <- file.path(path_user, "_DATA")

# check if the directory exists or create
if(!dir.exists(path_model)){dir.create(path_model)}
if(!dir.exists(path_data)){dir.create(path_data)}


# Vector of currency pairs
Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
          "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
          "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
          "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")   

# Reading the data from the Sandbox of Terminal 2 --> !!!Make sure that DataWriter robot is attached and working in Terminal 2!!!
sbx_price <- file.path(path_sbx, paste0("AI_CP",chart_period,"-300.csv"))
sbx_macd <- file.path(path_sbx, paste0("AI_BullPow", chart_period,"-300.csv"))
#price <- read_csv(sbx_price, col_names = F)
macd <- read_csv(sbx_macd, col_names = F, col_types = "cdddddddddddddddddddddddddddd")
macd$X1 <- ymd_hms(macd$X1)

# Prepare data frame with last 64 observations of all 28 pairs and remove date/time column (16 hours)
macd_100 <- macd %>% select(c(X2:X29)) %>% head(num_cols) #num_cols correspond for the 

# Rename the columns
names(macd_100) <- Pairs

# initialize the virtual machine
h2o.init(nthreads = 2)

# test for all columns
for (PAIR in Pairs) {
  # PAIR <- "EURUSD"
  # PAIR <- "GBPUSD"
  # PAIR <- "AUDCHF"
  # Extract one column with Indicator data for 1 pair (e.g. "EURUSD")
  df <- macd_100 %>% select(PAIR)

  #normalize macd value if we are dealing with JPY pairs
  if(stringr::str_detect(names(df), 'JPY')){
    df <- df/100
  }
  
  # Use function to score the data to the model
  my_market_prediction <- mt_evaluate(x = df,
                                      path_model = path_model,
                                      num_cols = num_cols,
                                      timeframe = 5) 
  # predicted value to write
  my_market <- my_market_prediction  %>% select(predict)
  
  # Join data to the predicted class and save to the temporary dataframe, only if predicted confidence is higher than 0.96
  # get predicted confidence
  my_market_conf <- my_market_prediction %>% select(-1) %>% select(which.max(.))
  # write dataframe to the temporary dataframe

  markets <- as.data.frame(c(PAIR,my_market,my_market_conf), col.names = c("Pair","M_T","Comfidence"))
  
  # all market type in a single table
  if(!exists("all_MT")) {
    all_MT <- markets
  } else {
    all_MT <- bind_rows(all_MT,markets, id = NULL)
    }
  
   if(!exists("df_temp") && my_market_conf[1,1]> 0.96){
    # join data to predicted class and write to the new object df_temp
    df_row <- t(df) %>% as.data.frame(row.names = 1)
    colnames(df_row) <- c(paste("X",1:num_cols,sep=""))
    df_temp <- df_row %>% transform(M_T = my_market[1,1]) %>% mutate_at("M_T", as.character)
    
  } else if(exists("df_temp") && my_market_conf[1,1]> 0.96) { 
    # in case df_temp is already exists
    # join data to predicted class and write to the new object df_temp
    df_row <- t(df) %>% as.data.frame(row.names = 1)
    colnames(df_row) <- c(paste("X",1:num_cols,sep=""))
    df_temp <-  df_row %>% transform(M_T = as.character(my_market[1,1])) %>% mutate_at("M_T", as.character) %>% bind_rows(df_temp)
    }
  
  # Return the name of the output
  names(my_market) <- PAIR
  # Add prediction confidence for diagnostics / robot logic purposes
  my_market <- my_market %>% bind_cols(my_market_conf)
  # Write obtained result to the sandboxes
  write_csv(my_market, file.path(path_sbx, paste0("AI_MarketType_", PAIR, chart_period, ".csv")))
  write_csv(my_market, file.path(path_sbxm,  paste0("AI_MarketType_", PAIR, chart_period, ".csv")))
  write_csv(my_market, file.path(path_sbxd,  paste0("AI_MarketType_", PAIR, chart_period, ".csv")))
  write_csv(my_market, file.path(path_sbxs,  paste0("AI_MarketType_", PAIR, chart_period, ".csv")))
 
}

all_MT <- all_MT[order(all_MT$Pair),]
write_csv(all_MT, file.path(path_sbx, paste0("AI_All_MarketType_", chart_period, ".csv")))


# retrieve already recorded data >> add temporary dataframe >> write to the data_update folder
# check if there is a rds file in the data_update folder and add the df_temp there
  if(exists("df_temp") && !file.exists(file.path(path_data, paste0("macd_ai_classified_",chart_period,"M",num_cols,".rds"))))
    {
      # write file first time
      write_rds(df_temp, file.path(path_data, paste0("macd_ai_classified_",chart_period,"M",num_cols,".rds")))
  } else if(exists("df_temp")) {
      # read previous file
        read_rds(file.path(path_data,  paste0("macd_ai_classified_",chart_period,"M",num_cols,".rds"))) %>% 
        # join obtained data
        bind_rows(df_temp) %>% 
        # write data back
        write_rds(file.path(path_data, paste0("macd_ai_classified_",chart_period,"M",num_cols,".rds")))
        #verify generated data
        # x1 <- read_rds(file.path(data_update_path, "macd_ai_classified.rds"))
    }
# 
#
# shutdown  the virtual machine
h2o.shutdown(prompt = F)

