## This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis of Trades
# Copyright (C) 2018 Vladimir Zhbanko

# PURPOSE: Analyse trade results in Terminal 1. Indicate when to optimize non performing systems
# NOTE:    Results are written in the Trading System Version Control Repository. 
#          Important: Trading System version control must contain a list of trading systems in use
#                     inside the file Setup.csv

# load packages. 
library(tidyverse)
library(lubridate)

# ----------- Main Steps -----------------
# -- Read trading results from Terminal 1
# -- Analyse results and filter systems where profit factor < 0.7
# -- Write command 'to optimize' to the file

# ----------- TESTS -----------------
# -- Select entire content of the script and execute
# -- Pass: object DFT1 is dataframe
# -- Pass: file 'Date-Re-Train.csv' is written into the trading robot folder
# -- Fail: object DFT1 has class 'try-error'


# =============================================
# *************Used Functions******************
# =============================================
# *** make sure to customize this path
source("E:/trading/Git/R_tradecontrol/get_profit_factorDF.R")
source("E:/trading/Git/R_tradecontrol/import_data.R")
source("E:/trading/Git/R_tradecontrol/check_if_optimize.R")

# =============================================
# ************End of Used Functions************
# =============================================

# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path
path_T2 <- "C:/Program Files (x86)/AM MT4 - Terminal 2/tester/Files/"

# trading system project folder
path_PRJCT_1 <- "E:/trading/Git/FALCON_B/"
path_PRJCT_2 <- "E:/trading/Git/FALCON_D/"
path_PRJCT_3 <- "E:/trading/Git/POP/"

# -------------------------
# read data from trades in terminal 2
# -------------------------
# # # uncomment code below to test functionality without MT4 platform installed
# DFT1 <- try(import_data(trade_log_file = "_TEST_DATA/OrdersResultsT1.csv",
#                         demo_mode = T),
#             silent = TRUE)
DFT2 <- try(import_data(path_T2, "OrdersResultsT2.csv"), silent = TRUE)

# -------------------------
# data frame T2 analysis and manipulation
# -------------------------

#### SORTING AND DECIDE IF SYSTEM NEEDS TO BE RE-TRAINED/RE-OPTIMIZED #### -----------------------------
# 4. Function that uses last 20 orders on DEMO && pr.fact < 0.7
#
### DEMO MODE 
# Uncomment code chunk below
# DFT1 %>% check_if_optimize(num_trades_to_consider = 10,
#                            profit_factor_limit = 0.7,
#                            demo_mode = T)

#
### PROJECT 1
#
DFT2 %>% check_if_optimize(path_trading_robot = path_PRJCT_1,
                           num_trades_to_consider = 20,
                           profit_factor_limit = 0.7)
#
### PROJECT 2
#
DFT2 %>% check_if_optimize(path_trading_robot = path_PRJCT_2,
                           num_trades_to_consider = 20,
                           profit_factor_limit = 0.7)


#
### PROJECT 3
#
DFT2 %>% check_if_optimize(path_trading_robot = path_PRJCT_3,
                           num_trades_to_consider = 20,
                           profit_factor_limit = 0.7)


##======================================== end of script
