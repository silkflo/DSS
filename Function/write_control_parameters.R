#' Function to find and write the best control parameters. This function is supposed to run on a weekly basis
#'
#' @param x - dataset containing the trading results for one trading robot
#' @param record_actual - if TRUE function will record actual results to the rds file; 
#'                        if FALSE function will directly return best control parameters (very slow execution)
#' @param path_control_files - path where control parameters will be saved
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' library(tidyverse) 
#' library(lubridate) 
#' library(ReinforcementLearning) 
#' library(magrittr)
#' source("C:/DSS/Function/All_Path.R")
#' source("C:/DSS/Function/log_RL_progress.R")
#' source("C:/DSS/Function/import_data.R") 
#' x <- try(import_data(trade_log_file = Path()$orderResultsT2,demo_mode = T),silent = TRUE)
#' path_control_files = Path()$controlFile

#' 
#' 
write_control_parameters <- function(x, path_control_files){


  
require(tidyverse)
require(ReinforcementLearning)
require(magrittr)

# delete DF_RES if it is exist
if(exists("DF_RES")){rm(DF_RES)}

# perform set of operations for every trading system

# sum(x$Profit)
states <- c("tradewin", "tradeloss")
actions <- c("ON", "OFF") # 'ON' and 'OFF' are referring to decision to trade with Slave system
# alpha - learning rate      0.1 <- slow       | fast        -> 0.9
# gamma - reward rate        0.1 <- short term | long term   -> 0.9
# epsilon - sampling rate    0.1 <- high sample| low sample  -> 0.9
#control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1) 

# Create RL models for all the space
Alpha <- seq(from = 0.1, to = 0.9, by = 0.2)
Gamma <- seq(from = 0.1, to = 0.9, by = 0.2)
Epsyl <- seq(from = 0.1, to = 0.9, by = 0.2)

for (ALF in Alpha) {
  for (GAM in Gamma) {
    for (EPS in Epsyl) {
      # assign values to test or debug
      # ALF <- 0.1
      # GAM <- 0.1
      # EPS <- 0.1
      control <- list(alpha = ALF, gamma = GAM, epsilon = EPS)
      # retrieve RL model Q values progress
      DF_RESEARCH <- log_RL_progress(x = x,states = states, actions = actions, control = control)
      # create object where all data can be aggregated
      if(!exists("DF_RES")){DF_RES <- DF_RESEARCH} else {
        DF_RES <- bind_rows(DF_RES, DF_RESEARCH) }
      
    }
  }
}

# object DF_RES is generated, next goal is to simulate how the trading would go on Terminal 3. 
# which means that separate column with trading result will be generated. 
# this column will simulate trades commanded by RL policy in Terminal 3

# generate simulated trades in T3
DF_RES1 <- DF_RES %>% 
  # create column with policy decision (1 - command on to trade in T3, 0 - no trade)
  mutate(T3_trigger = ifelse(rewardseq.ON > rewardseq.OFF & rewardseq.ON > 0, 1, 0)) %>% 
  # create shifted column with reward. This is to simulate future trades decisions of RL model
  mutate(Prev_result = lag(totreward, 2)) %>% 
  # TDL generate column with PNL in T3, this is based on trades executed by RL trigger
  mutate(T3_PnL = ifelse(T3_trigger == 1 & Prev_result > 0 & trstate == "tradewin", totreward, 
                         ifelse(T3_trigger == 1 & Prev_result < 1 & trstate == "tradeloss", totreward,0))) %>% 
  # group by all parameters and generate sum of column T3_PnL
  group_by(alpha,gamma,epsilon) %>% 
  #summarise
  summarise(Tot_pnlT3 = sum(T3_PnL)) %>% 
  # find rows where Tot_pnlT3 column has a highest values
  #slice(which.max(Tot_pnlT3)) %>% 
  # order
  arrange(desc(Tot_pnlT3)) %>% 
  # get the first one
  head(1) %>% 
  # select only control parameters
  select(alpha, gamma, epsilon) %>% 
  # convert to list
  as.list()

## We have the best control parameters

# record to file
# recording control parameters for this system (this function is intended to run 1x week)

  # create directory where to write files if not exists yet
  if(!dir.exists(path_control_files)){
    dir.create(path_control_files)
  }
  # write the best control parameters, use magic number as name
  # extract current magic number to be used as a file name
  m_number <- x %$% MagicNumber %>% head(1)
  # write rds file with control parameters
  write_rds(DF_RES1, paste0(path_control_files,"/", m_number, ".rds"))
  
  # uncomment to read the rds file created
  # y <- read_rds(paste0("E:/trading/Git/R_tradecontrol/_RL2/control/",m_number,".rds"))

  
}

