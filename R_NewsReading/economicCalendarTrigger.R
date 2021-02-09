# run script every hour


library(rvest)
library(lubridate)
library(tidyverse)
library(stringr)
library(magrittr)
library(tidyverse)
library(mailR)

source("C:/DSS/Function/All_Path.R")
source("C:/DSS/Function/writeCommandViaCSV.R")
#--------------DATA IMPORT-----------------#
useRL <- FALSE
TimeBeforeEventStopTrade <- 1  #unit = hour
TimeAfterEventAllowTrade <- 2  #unit = hour
path_T1 <- Path()$pathT1
path_T2 <- Path()$pathT2
path_T3 <- Path()$pathT3
path_T4 <- Path()$pathT4
path_T5 <- Path()$pathT5

#----calendar 1-------


calendarPath <- paste0(Path()$data,"ff_calendar_thisweek.csv")
#make sure the file ff_calendar_thisweek.csv already exist in _DATA folder
if(file.exists(calendarPath)){
  calendarDate <- as.Date(file.mtime(calendarPath))
  
  if(calendarDate != Sys.Date())
  {
    path_calendar <- "https://cdn-nfs.faireconomy.media/ff_calendar_thisweek.csv"
    destfile <- paste0(Path()$data,"ff_calendar_thisweek.csv")
    download.file(path_calendar, destfile)
    print("calendar updated")
  }
}else{
  path_calendar <- "https://cdn-nfs.faireconomy.media/ff_calendar_thisweek.csv"
  destfile <- paste0(Path()$data,"ff_calendar_thisweek.csv")
  download.file(path_calendar, destfile)
  print("calendar missing downloaded")
}

cal_df <- try(readr::read_csv(paste0(Path()$data,"ff_calendar_thisweek.csv")),silent = TRUE)
cal_df$Start <- paste(cal_df$Date," ",cal_df$Time)
cal_df_Result <- data.frame(Start = cal_df$Start,
                            Name = cal_df$Title,
                            Impact = cal_df$Impact,
                            Currency = cal_df$Country) %>%
  filter( cal_df$Impact == "HIGH")

#---------calendar 2----------
calendarPath <- Path()$data
if(!file.exists(paste0(calendarPath,"calendar-event-list.csv"))){
  print("please download the calendar into the _DATA folder : https://www.fxstreet.com/economic-calendar")
}else{
  DFevent <- try(read.csv(paste0(calendarPath,"calendar-event-list.csv")),silent = TRUE)
  
  DFeventResult <- data.frame(Start = as.POSIXct( DFevent$Start, format="%m/%d/%Y %H:%M:%S" , tz = "GMT"),
                              Name = DFevent$Name,
                              Impact = DFevent$Impact,
                              Currency = DFevent$Currency)  %>%
    filter(DFevent$Impact == "HIGH")
}
#----mix calendar------
fullCalendarResult <- rbind(cal_df_Result,DFeventResult)


DFT2 <- try(read_csv(Path()$orderResultsT2,
                     col_names = c("MagicNumber", "TicketNumber", "OrderStartTime", "OrderCloseTime", "Profit", "Symbol", "OrderType"),
                     col_types = "iiccdci"),silent = TRUE)

# get Magic Number and symbol
DFT2MN <- DFT2 %>% group_by(MagicNumber) %>% select("MagicNumber","Symbol") %>% unique()

#----------------TABLE EDITION--------------#


# change timezone to local time
# tz list : https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
fullCalendarResult$Start <- format(fullCalendarResult$Start, tz="Africa/Cairo",usetz=TRUE)

#remove old events (can be adjusted according how long after the event have started you want allow trade)
fullCalendarResult <- filter(fullCalendarResult, fullCalendarResult$Start > Sys.time() - (TimeAfterEventAllowTrade *60*60))


#-------FLAG RESULT--------------#
# use for loop to compare all event time with current time to send us the signal
flag  <- vector("numeric", nrow(DFT2MN))

# the 1st for loop will check any magic numbers
for(i in 1:nrow(DFT2MN)){
  # i <-1
  if(nrow(fullCalendarResult>0)){
    for(j in 1:nrow(fullCalendarResult)){
      # j<-1
      # 1 can be adjusted according how long before the event you want the signal stop the trading
      if(as.double(difftime(fullCalendarResult$Start[j],Sys.time(), units = "hours")) < 1 &&
         (substr(DFT2MN$Symbol[i],1,3) == fullCalendarResult$Currency[j] || substr(DFT2MN$Symbol[i],4,6) == fullCalendarResult$Currency[j] )){
        flag[i] <- 1 
        break
      } else{
        flag[i] <- 0
      }
    }
  } else
  {
    flag[i] <- 0
  }
  
}

# Flag = 1 <=> incoming economic event can't trade
# add new column telling us which magic number shouldn't trade
DFT2MN$Flag = flag

# filter the table with magic number not allow to trade only
#DFT2MN <-as.data.frame(DFT2MN) %>% filter(DFT2MN$Flag == 1)


#----------SEND SIGNAL IN A CSV FILE-------------#

# write flag obtained to all terminals!

#Terminal 2
write.csv(DFT2MN, paste0(Path()$pathT2,"01_MacroeconomicEvent.csv"), row.names = F)
#Terminal 1
DFT1MN <- data.frame(DFT2MN$MagicNumber - 100, DFT2MN$Symbol,DFT2MN$Flag )
write.csv(DFT1MN, paste0(Path()$pathT1,"01_MacroeconomicEvent.csv"), row.names = F)
#Terminal 3
DFT3MN <- data.frame(DFT2MN$MagicNumber + 100, DFT2MN$Symbol,DFT2MN$Flag )
write.csv(DFT3MN, paste0(Path()$pathT3,"01_MacroeconomicEvent.csv"), row.names = F)
#Terminal 4
DFT4MN <- data.frame(DFT2MN$MagicNumber + 200, DFT2MN$Symbol,DFT2MN$Flag )
write.csv(DFT4MN, paste0(Path()$pathT4,"01_MacroeconomicEvent.csv"), row.names = F)
#Terminal 5
DFT5MN <- data.frame(DFT2MN$MagicNumber + 300, DFT2MN$Symbol,DFT2MN$Flag )
write.csv(DFT4MN, paste0(Path()$pathT4,"01_MacroeconomicEvent.csv"), row.names = F)




#  DF_NT <- (read_csv(file= file.path(path_T2, "01_MacroeconomicEvent.csv"), col_types = "ici"))
DFT2MN  <- as.data.frame(DFT2MN) %>% filter(DFT2MN$Flag == 1)

# disable trades

if(!class(DFT2)[1]=='try-error' && !class(DFT2MN)[1]=='try-error'){
  
  DFT2MN <- DFT2MN %>%
    group_by(MagicNumber) %>%
    select(MagicNumber) %>% 
    mutate(IsEnabled = 0)
  # write commands to disable systems
  DFT1MN <- data.frame(DFT2MN$MagicNumber - 100, DFT2MN$IsEnabled)
  colnames(DFT1MN) <- c("MagicNumber","isEnabled")
  writeCommandViaCSV(DFT1MN,path_T1)
  
  writeCommandViaCSV(DFT2MN,path_T2)
  
  DFT3MN <- data.frame(DFT2MN$MagicNumber + 100, DFT2MN$IsEnabled)
  colnames(DFT3MN) <- c("MagicNumber","isEnabled")
  writeCommandViaCSV(DFT3MN,path_T3)
  
  DFT4MN <- data.frame(DFT2MN$MagicNumber + 200, DFT2MN$IsEnabled)
  colnames(DFT4MN) <- c("MagicNumber","isEnabled")
  writeCommandViaCSV(DFT4MN,path_T4)
  
  DFT5MN <- data.frame(DFT2MN$MagicNumber + 300, DFT2MN$IsEnabled)
  colnames(DFT5MN) <- c("MagicNumber","isEnabled")
  writeCommandViaCSV(DFT5MN,path_T5)
}

DFT2MN <- read_csv(file= file.path(path_T2, "01_MacroeconomicEvent.csv"), col_types = "ici") 
DFT2MN  <- as.data.frame(DFT2MN) %>% filter(DFT2MN$Flag == 0)

MN <- DFT2MN %>%
  group_by(MagicNumber) %>%
  select("MagicNumber") %>% unique()

if(useRL == TRUE){
  # delete rows from DF_NT if systemcontrol don't allow trade
  for(i in nrow(MN))
  {
    # i <-1
    magicNumber <- toString(MN[i,])
    
    SC <- read_csv(file= file.path(path_T3, paste0("SystemControl",magicNumber,".csv")),col_names =  TRUE, col_types = "ii") 
    SC <- as.data.frame(SC)
    
    if(SC$IsEnabled == 0){
      DFT2MN <- as.data.frame(DFT2MN) %>%
        filter(DFT2MN$MagicNumber != magicNumber)
    }
  }
}

# enable trades

# in this algorithm SystemControl file must be enabled in case there are no MacroEconomic Event
if(!class(DFT2)[1]=='try-error' && !class(DFT2MN)[1]=='try-error'){
  DFT2MN <- DFT2MN %>%
    group_by(MagicNumber) %>% 
    select(MagicNumber) %>% 
    mutate(IsEnabled = 1)  
  # write commands to disable systems
  DFT1MN <- data.frame(DFT2MN$MagicNumber - 100, DFT2MN$IsEnabled)
  colnames(DFT1MN) <- c("MagicNumber","isEnabled")
  writeCommandViaCSV(DFT1MN,path_T1)
  
  writeCommandViaCSV(DFT2MN,path_T2)
  
  DFT3MN <- data.frame(DFT2MN$MagicNumber + 100, DFT2MN$IsEnabled)
  colnames(DFT3MN) <- c("MagicNumber","isEnabled")
  writeCommandViaCSV(DFT3MN,path_T3)
  
  DFT4MN <- data.frame(DFT2MN$MagicNumber + 200, DFT2MN$IsEnabled)
  colnames(DFT4MN) <- c("MagicNumber","isEnabled")
  writeCommandViaCSV(DFT4MN,path_T4)
  
  DFT5MN <- data.frame(DFT2MN$MagicNumber + 300, DFT2MN$IsEnabled)
  colnames(DFT5MN) <- c("MagicNumber","isEnabled")
  writeCommandViaCSV(DFT5MN,path_T5)
}
