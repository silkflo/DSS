#global.r
#read dataset
library(readr)
#path to user repo:
#!!!Change this path!!! 
#path_user <- "C:/DSS_Bot/DSS_R"
path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')

path_data <- file.path(path_user, "_DATA")

#function to get data for this App to work
#get_data <- function(){
  macd_ai <- readr::read_rds(file.path(path_data, 'macd_ai_classified.rds'))
#  return(macd_ai)}
#use this function
# macd_ai <- get_data()
#function to write data
write_data <- function(x){
  readr::write_rds(x, file.path(path_data, 'macd_ai_classified.rds'))
}

#output data from this app
file_checked <- file.path(path_data, "macd_checked_60M.rds")

# function that writes data to rds file 
storeData <- function(data, fileName) {
  
  # store store gathered unique records
  # non duplicates
  nonDuplicate <- data[!duplicated(data), ]
  
  # read existing file, if that exists...
  if(file.exists(fileName)){
    ex_data <- readr::read_rds(fileName)
    # append...
    agr_data <- dplyr::bind_rows(ex_data, nonDuplicate)
    # Write the file to the local system
    readr::write_rds(x = agr_data, file = fileName)
    # write data first time...
  } else {
    # Write the file to the local system
    readr::write_rds(x = nonDuplicate, file = fileName)
  }
  
  
  
}