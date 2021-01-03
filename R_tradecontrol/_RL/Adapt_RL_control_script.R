# PURPOSE: Adapt RL control parameters and write them to the file

library(tidyverse) 
library(lubridate) 
library(ReinforcementLearning) 
library(magrittr)

  
# ----------- Main Steps -----------------
# -- Read trading results from Terminal 1
# -- Use function find control parameters to write best RL control parameters for every trading robot

# *** make sure to customize this path
source("C:/DSS/Function/import_data.R") 
source("C:/DSS/Function/write_control_parameters.R")
source("C:/DSS/Function/log_RL_progress.R")
source("C:/DSS/Function/All_Path.R")

timeFrame <- 5 
path_control_files <- Path()$pathControlFile
DFT2 <- try(import_data(trade_log_file = Path()$orderResultsT2,demo_mode = T),silent = TRUE)

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
    # i <- 1 #policy off
    # i <- 2 #policy on
    # i <- 3
    
    # extract current magic number id
  trading_system <- vector_systems[i]
  # get trading summary data only for one system 
  trading_systemDF <- DFT2 %>% filter(MagicNumber == trading_system)
  
  ## -- Go to the next Loop iteration if there is too little trades! -- ##
  if(nrow(trading_systemDF) < 5) { next }
  
  #==============================================================================
  # Define state and action sets for Reinforcement Learning
  states <- c("tradewin", "tradeloss")
  actions <- c("ON", "OFF") # 'ON' and 'OFF' are referring to decision to trade with Slave system
  
  # Define reinforcement learning parameters (see explanation below or in vignette)
  # -----
  # alpha - learning rate      0.1 <- slow       | fast        -> 0.9
  # gamma - reward rate        0.1 <- short term | long term   -> 0.9
  # epsilon - sampling rate    0.1 <- high sample| low sample  -> 0.9
  # iter 
  # ----- 
  # to uncomment desired learning parameters:
  # NOTE: more research is required to find best parameters TDL TDL TDL
  #control <- list(alpha = 0.5, gamma = 0.5, epsilon = 0.5)
  #control <- list(alpha = 0.9, gamma = 0.9, epsilon = 0.9)
  #control <- list(alpha = 0.7, gamma = 0.5, epsilon = 0.9)
  #control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1) 
  
  # or to use optimal control parameters found by auxiliary function
  write_control_parameters(trading_systemDF, 
                           path_control_files = path_control_files)
  #cntrl <- read_rds(paste0(path_control_files, "/", trading_system, ".rds"))
  #cntrl <- read_rds(paste0(path_control_files, "/", 8139106, ".rds"))
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}

time_end <- Sys.time()
#calculate total time difference in seconds
time_total <- difftime(time_end,time_start,units="sec")
#convert to numeric
as.double(time_total)


logs <- data.frame(dtm = Sys.time(), time2run = time_total,file = "Adapt_RL_control_script.R")

#read existing log (if exists) and add there a new log data
if(!file.exists(file.path(Path()$pathLog, paste0('time_executeM',timeFrame,'.rds')))){
  write_rds(logs, file.path(Path()$pathLog, paste0('time_executeM',timeFrame,'.rds')))
} else {
  read_rds(file.path(Path()$pathLog, paste0('time_executeM',timeFrame,'.rds'))) %>% 
    bind_rows(logs) %>% 
    write_rds(file.path(Path()$pathLog, paste0('time_executeM',timeFrame,'.rds')))
}


# uncomment to read the rds file created
# y <- read_rds(paste0("E:/trading/Git/R_tradecontrol/_RL2/control/8118201.rds"))

