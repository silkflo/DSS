#' Import Market Type related Data to R from the Sandbox
#'
#' @description Function imports file from the MetaTrader sandbox. Function performs necessary cleansing of the data column types
#'
#' @param path_sbxm - String, Path to the sandbox with the log file (master terminal)
#' @param system_number - magic number id of the trading system
#'
#' @return function returns the data frame with 5 columns including market type code
#' @export
#'
#' @author (C) 2020 Vladimir Zhbanko
#'
#' @examples
#'
#'
#' library(dplyr)
#' library(readr)
#' library(lazytrade)
#' 
#' source("C:/DSS/Function/All_Path.R")
#' path_terminal <- Path()$pathT2
#' system_number <- 1201204
#'
import_data_mt <- function(path_terminal, system_number){
  # path_terminal <- "C:/Program Files (x86)/AM MT4 - Terminal 2/MQL4/Files/"
  # system_number <- 1201214
  require(tidyverse)
  trade_log_file <- paste0("MarketTypeLog", system_number, ".csv")
  DFT1 <- try(read_csv(file = file.path(path_terminal, trade_log_file), 
                       col_names = c("MagicNumber", "TicketNumber", "MarketType"),
                       col_types = "iic"), 
              silent = TRUE)
  if(class(DFT1)[1] == "try-error") {stop("Error reading file. File with trades may not exist yet!",
                                          call. = FALSE)}
  if(!nrow(DFT1)==0){
    # data frame preparation
    DFT1$MarketType <- as.factor(DFT1$MarketType)
    # removes duplicates
    DFT1 <- unique(DFT1)
    
    return(DFT1)
  } else {
    stop("No trades executed so far. Trade data log is empty!",
         call. = FALSE)
  }
  
}