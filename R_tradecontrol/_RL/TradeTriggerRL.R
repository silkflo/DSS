# packages used *** make sure to install these packages
library(tidyverse) 
library(lubridate) 
library(ReinforcementLearning) 
library(magrittr)

# ----------- Main Steps -----------------
# -- Read trading results from Terminal 1
# -- Rearrange data for RL
# -- Perform Reinforcement Learning or Update Model with New Data
# -- Start/Stop trades in Terminal 3 based on New Policy
# -- Start/Stop trades on Terminals at MacroEconomic news releases

# ----------------
# Used Functions (to make code more compact). See detail of each function in the repository
#-----------------
# *** make sure to customize this path
source("C:/DSS/Function/import_data.R") 
source("C:/DSS/Function/generate_RL_policy.R")
source("C:/DSS/Function/record_policy.R")
source("C:/DSS/Function/writeCommandViaCSV.R")
source("C:/DSS/Function/Adapt_RL_control.R")
source("C:/DSS/Function/All_Path.R")
 
# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path *** make sure to customize this path
path_T2 <- Path()$pathT2

# terminal 3 path *** make sure to customize this path
path_T3 <- Path()$pathT3

# path where to read control parameters from
path_control_files <- Path()$controlFile

timeFrame <- 5

# -------------------------
# read data from trades in terminal 2
# -------------------------
DFT2 <- try(import_data(path_T2, "OrdersResultsT2.csv"), silent = TRUE)
# -------------------------
# read data from trades in terminal 3
# -------------------------
DFT3 <- try(import_data(path_T3, "OrdersResultsT3.csv"), silent = TRUE)

# Vector with unique Trading Systems
vector_systems <- DFT2 %$% MagicNumber %>% unique() %>% sort()

# For debugging: summarize number of trades to see desired number of trades was achieved
DFT2_sum <- DFT2 %>% 
  group_by(MagicNumber) %>% 
  summarise(Num_Trades = n(),
            Mean_profit = sum(Profit)) %>% 
  arrange(desc(Num_Trades))


time_start <- Sys.time()
# Is searching for the best parameter using brut force
#------------------------------------------------
j <- 0
for (i in 1:length(vector_systems)) {
  tryCatch({
   #i<-5
    trading_system <- vector_systems[i]
  })
    if(!file.exists(file.path(Path()$controlFile,paste0(trading_system,".rds")))){
      j <- j+1
     }
}

if(j>0){
Adapt_RL_control(DFT2,path_T2,path_control_files)
}
#--------------------------------------------------

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
  trading_systemDF <- filter(trading_systemDF,trading_systemDF$Profit != 0)
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
  # control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1)
  # Use optimal control parameters found by auxiliary function
 
  control <- read_rds(paste0(path_control_files,"/", trading_system, ".rds"))
  #control <- read_rds(paste0(path_control_files,"/", 8118102, ".rds"))
  
  # perform reinforcement learning and return policy
  policy_tr_systDF <- generate_RL_policy(trading_systemDF, states = states,actions = actions,
                                         control = control)
  # get the latest trade of that system (will be used to match with policy of RL)
  latest_trade <- trading_systemDF %>% 
    arrange(desc(OrderCloseTime)) %>% 
    mutate(NextState = ifelse(Profit>0, "tradewin",
                              ifelse(Profit<0, "tradeloss", NA)),
           Reward =  Profit,
           State = NextState) %>% head(1) %$% NextState
  
  # record policy to the sandbox of Terminal 3, this should be analysed by EA
  record_policy(x = policy_tr_systDF, last_result = latest_trade, trading_system = trading_system, path_sandbox = path_T3)
 
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}
### ============== END of FOR EVERY TRADING SYSTEM ###
time_end <- Sys.time()
#calculate total time difference in seconds
time_total <- difftime(time_end,time_start,units="sec")
#convert to numeric
as.double(time_total)


logs <- data.frame(dtm = Sys.time(), time2run = time_total,file = "TradeTriggerRL.R")

#read existing log (if exists) and add there a new log data
if(!file.exists(file.path(Path()$pathLog, paste0('time_executeM',timeFrame,'.rds')))){
  write_rds(logs, file.path(Path()$pathLog, paste0('time_executeM',timeFrame,'.rds')))
} else {
  read_rds(file.path(Path()$pathLog, paste0('time_executeM',timeFrame,'.rds'))) %>% 
    bind_rows(logs) %>% 
    write_rds(file.path(Path()$pathLog, paste0('time_executeM',timeFrame,'.rds')))
}

##========================================
# -------------------------
# stopping all systems when macroeconomic event is present
# this will be covered in the Course #5 of the Lazy Trading Series!
# -------------------------

#flag 1 means can not trade
if(file.exists(file.path(path_T2, "01_MacroeconomicEvent.csv"))){
  DF_NT <- (read_csv(file= file.path(path_T2, "01_MacroeconomicEvent.csv"), col_types = "ici"))
  DF_NT  <- as.data.frame(DF_NT) %>% filter(DF_NT$Flag == 1)
  
  # disable trades

  if(!class(DFT3)[1]=='try-error' && !class(DF_NT)[1]=='try-error'){
    DF_NT %>%
      group_by(MagicNumber) %>%
      select(MagicNumber) %>% 
      mutate(IsEnabled = 0) %>% 
      # write commands to disable systems
      writeCommandViaCSV(path_T3)
    }
  
#   DF_NT <- read_csv(file= file.path(path_T2, "01_MacroeconomicEvent.csv"), col_types = "ici") 
#   DF_NT  <- as.data.frame(DF_NT) %>% filter(DF_NT$Flag == 0)
#   
#   MN <- DFT3 %>%
#         group_by(MagicNumber) %>%
#         select("MagicNumber") %>% unique()
#   
#   # delete rows from DF_NT if systemcontrol don't allow trade
#   for(i in nrow(MN))
#   {
#     # i <-1
#     magicNumber <- toString(MN[i,])
#     
#     SC <- read_csv(file= file.path(path_T3, paste0("SystemControl",magicNumber,".csv")),col_names =  TRUE, col_types = "ii") 
#     SC <- as.data.frame(SC)
#     
#     if(SC$IsEnabled == 0){
#       DF_NT <- as.data.frame(DF_NT) %>%
#               filter(DF_NT$MagicNumber != magicNumber)
#     }
#   }
#   
#   
#   # enable trades
# 
#   # in this algorithm SystemControl file must be enabled in case there are no MacroEconomic Event
#   if(!class(DFT3)[1]=='try-error' && !class(DF_NT)[1]=='try-error'){
#     DF_NT %>%
#       group_by(MagicNumber) %>% 
#       select(MagicNumber) %>% 
#       mutate(IsEnabled = 1) %>% 
#       # write commands to disable systems
#       writeCommandViaCSV(path_T3)}
#   
# }
# 