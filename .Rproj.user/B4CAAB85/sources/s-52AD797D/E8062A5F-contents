# Copyright (C) 2019 Vladimir Zhbanko
# PURPOSE: Analyse trade results in Terminal 1 and Trigger or Stop Trades in Terminal 3
# DETAILS: Trades are analysed and RL model is created for each single Expert Advisor
#        : Q states function is calculated, whenever Action 'ON' is > than 'OFF' trade trigger will be active   
#        : Results are written to the file of the MT4 Trading Terminal
#        : Reinforcement Learning Models are created for each specific system considering 6 Market Types
#        : RL would learn to select the best Market Types to switch ON/OFF Trading Systems

# packages used *** make sure to install these packages
library(readr)
library(stringr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ReinforcementLearning) 
library(magrittr)
library(lazytrade)

source("C:/DSS/Function/All_Path.R")
source("C:/DSS/Function/import_data_mt.R")

# ----------- Main Steps -----------------
# -- Read trading results from Terminal 1
# -- Rearrange data for RL
# -- Perform Reinforcement Learning or Update Model with New Data
# -- Start/Stop trades in Terminal 3 based on New Policy
# -- Start/Stop trades on Terminals at MacroEconomic news releases (will be covered in Course #5)

timeFrame <- 5

# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 2 path *** make sure to customize this path
path_T2 <- Path()$pathT2

# terminal 3 path *** make sure to customize this path
path_T3 <- Path()$pathT3


path_user <- Path()$tradeControl

# path with folder containing control parameters
path_control_files = Path()$controlFile

# -------------------------
# read data from trades in terminal 2
# -------------------------

DFT2 <- try(import_data(path_T2, "OrdersResultsT2.csv"), silent = TRUE)
# -------------------------
# read data from trades in terminal 3
# -------------------------
DFT3 <- try(import_data(path_T3, "OrdersResultsT3.csv"), silent = TRUE)

# check if we have no errors importing trading results
if(!class(DFT2)[1]=="try-error"){
  
  # Vector with unique Trading Systems
  vector_systems <- DFT2 %$% MagicNumber %>% unique() %>% sort()
  
  # For debugging: summarise number of trades to see desired number of trades was achieved
  DFT2_sum <- DFT2 %>% 
    group_by(MagicNumber) %>% 
    summarise(Num_Trades = n(),
              Mean_profit = sum(Profit)) %>% 
    arrange(desc(Num_Trades))
  
  time_start <- Sys.time()
  ### ============== FOR EVERY TRADING SYSTEM ###
  for (i in 1:length(vector_systems)) {
    # tryCatch() function will not abort the entire for loop in case of the error in one iteration
    tryCatch({
      # execute this code below for debugging:
      # i <- 1
      
      # extract current magic number id
      trading_system <- vector_systems[i]
      # get trading summary data only for one system 
      trading_systemDF <- DFT2 %>% filter(MagicNumber == trading_system)
      # try to extract market type information for that system, filter rows where MarketType was not logged!
      DFT2_MT <- try(import_data_mt(path_terminal = path_T2, 
                                    system_number =  trading_system),
                     silent = TRUE) %>%
        filter(TicketNumber != -1)
      # go to the next i if there is no data
      if(class(DFT2_MT)[1]=="try-error") { next }
      # joining the data with market type info
      trading_systemDF <- inner_join(trading_systemDF, DFT2_MT, by = "TicketNumber") 
      # write this data for further debugging or tests
      # write_rds(trading_systemDF,path = "test_data/data_trades_markettype.rds")
      
      #==============================================================================
      # Define state and action sets for Reinforcement Learning
      states <- c("BUN", "BUV", "BEN", "BEV", "RAN", "RAV")
      actions <- c("ON", "OFF") # 'ON' and 'OFF' are referring to decision to trade with Slave system
      
      # Define reinforcement learning parameters (see explanation below or in vignette)
      # -----
      # alpha - learning rate      0.1 <- slow       | fast        -> 0.9
      # gamma - reward rate        0.1 <- short term | long term   -> 0.9
      # epsilon - sampling rate    0.1 <- high sample| low sample  -> 0.9
      # iter 
      # ----- 
      # to uncomment desired learning parameters:
      #control <- list(alpha = 0.5, gamma = 0.5, epsilon = 0.5)
      #control <- list(alpha = 0.9, gamma = 0.9, epsilon = 0.9)
      #control <- list(alpha = 0.8, gamma = 0.3, epsilon = 0.5)
      #control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1)
      # check existence of the file with control parameters, go to next if not exists
      if(!file.exists(paste0(path_control_files,"/", trading_system, ".rds"))) { next }
      # Use optimal control parameters found by auxiliary function
      control <- read_rds(paste0(path_control_files,"/", trading_system, ".rds"))
      #control <- read_rds(paste0(path_control_files,"/", 8118102, ".rds"))
      # -----
      #==============================================================================
      
      
      # perform reinforcement learning and return policy
      policy_tr_systDF <- rl_generate_policy_mt(x = trading_systemDF,
                                                states = states,
                                                actions = actions,
                                                control = control)
      
      # # summarize results by Market Type
      # trading_systemDF %>% group_by(MarketType) %>% summarise(ProfitMT = sum(Profit))
      
      # record policy to the sandbox of Terminal 3, this should be analysed by EA
      rl_record_policy_mt(x = policy_tr_systDF, 
                          trading_system = trading_system,
                          path_terminal = path_T3,
                          fileName = "SystemControlMT")
      
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    
  }
  ### ============== END of FOR EVERY TRADING SYSTEM ###
  
} # ============== END of condition to check  if(!class(DFT1_MT)[1]=="try-error")

time_end <- Sys.time()
#calculate total time difference in seconds
time_total <- difftime(time_end,time_start,units="sec")
#convert to numeric
as.double(time_total)


logs <- data.frame(dtm = Sys.time(), time2run = time_total,file = "MT_TradeTriggerRL.R")

#read existing log (if exists) and add there a new log data
if(!file.exists(file.path(Path()$pathLog, paste0('time_executeM',timeFrame,'.rds')))){
  write_rds(logs, file.path(Path()$pathLog, paste0('time_executeM',timeFrame,'.rds')))
} else {
  read_rds(file.path(Path()$pathLog, paste0('time_executeM',timeFrame,'.rds'))) %>% 
    bind_rows(logs) %>% 
    write_rds(file.path(Path()$pathLog, paste0('time_executeM',timeFrame,'.rds')))
}

