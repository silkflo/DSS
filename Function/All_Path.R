

Path <- function(){
  
  calendarPath <- "C:/DSS/R_NewsReading/ff_calendar_thisweek.csv"
  orderResultsT2 <- "C:/Program Files (x86)/MT4 - Terminal 2/MQL4/files/OrdersResultsT2.csv"
  pathT1 <- "C:/Program Files (x86)/MT4 - Terminal 1/MQL4/Files/"
  pathT2 <- "C:/Program Files (x86)/MT4 - Terminal 2/MQL4/Files/"
  pathT3 <- "C:/Program Files (x86)/MT4 - Terminal 3/MQL4/Files/"
  pathT4 <- "C:/Program Files (x86)/MT4 - Terminal 4/MQL4/Files/"
  pathT5 <- "C:/Program Files (x86)/MT4 - Terminal 5/MQL4/Files/"
  controlFile <- "C:/DSS/R_tradecontrol/control/"
  pathLog <- "C:/DSS/Log/"
  marketTypeData <- "C:/DSS/R_markettype/_DATA/"
  marketType <- "C:/DSS/R_markettype/"
  selfLearning <- "C:/DSS/R_selflearning/"
  tradeControl <- "C/DSS/R_tradecontrol/"
  data <- "C:/DSS/_DATA/"
  
  path <- list("calendarPath" = calendarPath,
               "orderResultsT2" = orderResultsT2,
               "pathT1" = pathT1,
               "pathT2" = pathT2,
               "pathT3" = pathT3,
               "pathT4" = pathT4,
               "pathT5" = pathT5,
               "controlFile" = controlFile,
               "pathLog" = pathLog,
               "marketTypeData" = marketTypeData,
               "marketType" = marketType,
               "selfLearning" = selfLearning,
               "tradeControl" = tradeControl,
               "data" = data)
          
  
  return(path)
  
}