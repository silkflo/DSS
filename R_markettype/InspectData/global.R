#global.r
#read dataset
library(readr)
#path to user repo:
#!!!Change this path!!! 
# global variable creation
path_user <- "E:/trading/Git/R_markettype"
path_data <- file.path(path_user, "_DATA")
file_checked <- file.path(path_data, "macd_checked.rds")

macd_ai <- readr::read_rds(file.path(path_data, 'macd_ai_classified_5M32.rds'))

