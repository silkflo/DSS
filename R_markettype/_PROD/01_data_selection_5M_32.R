# ----------------------------------------------------------------------------------------
# R Script to select and train the Deep Learning model on Financial Asset Time Series Data
# ----------------------------------------------------------------------------------------
# ## Manually select data into big matrix with label of 6 classes
# ----------------------------------------------------------------------------------------
#
# Supervised Deep Learning Classification Modelling
#
# load libraries to use and custom functions

# "EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
# "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
# "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
# "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF"



library(tidyverse)
library(h2o)
library(lubridate)
library(plotly)
library(lazytrade)
source("C:/DSS/Function/All_Path.R")
#used to name the columns
# Pairs = c("DATE","EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
#          "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
#          "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
#          "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")   
#
#### Read asset prices and indicators ==========================================
# load prices of 28 currencies
# this is the files source taken in consideration for the model
prices <- read_csv(file.path(Path()$marketTypeData, "AI_CP5-12000.csv"), col_names = F)
prices$X1 <- ymd_hms(prices$X1)
#names(prices) <- Pairs

# load macd indicator of 28 currencies
macd <- read_csv(file.path(Path()$marketTypeData, "AI_BullPow5-12000.csv"), col_names = F)
macd$X1 <- ymd_hms(macd$X1)



#names(macd) <- Pairs

#### Manually Selecting data... =================================================
# Market Periods
# 1. Bull normal, BUN
# 2. Bull volatile, BUV
# 3. Bear normal, BEN
# 4. Bear volatile, BEV
# 5. Sideways quiet, RAN
# 6. Sideways volatile, RAV

##########################################################################
## ---------- # 1. Bull normal, BUN ---------------
##########################################################################
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/

#USDNZD
ggplot(prices, aes(X1, X5))+geom_line()

# Extract approximate date and choose only relevant columns
# time on df is decalate of - 7 hours and the df is decalate +2 candles from the market...
#end +02:40
price_df <- prices %>% filter(X1 > "2020-10-08 12:05:00", X1 < "2020-10-08 14:50:00") %>% select(X1, X5)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X5))+geom_line()

# Extract corresponding piece of indicator dataframe:
macd_df <- macd %>% select(X1, X5) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X5.y, col = X5.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bun1 <- macd_df %>% select(X5.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#USDCHF
ggplot(prices, aes(X1, X7))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-10-22 02:40:00", X1 < "2020-10-22 05:25:00") %>% select(X1, X7)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X7))+geom_line()

# Extract corresponding piece of indicator dataframe:
macd_df <- macd %>% select(X1, X7) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X7.y, col = X7.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bun2 <- macd_df %>% select(X7.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#EURUSD
ggplot(prices, aes(X1, X2))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-12-01 07:45:00", X1 < "2020-12-01 10:30:00") %>% select(X1, X2)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X2))+geom_line()

# Extract corresponding piece of indicator dataframe:
macd_df <- macd %>% select(X1, X2) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X2.y, col = X2.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bun3 <- macd_df %>% select(X2.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#EURGBP
ggplot(prices, aes(X1, X9))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-10-30 09:10:00", X1 < "2020-10-30 11:55:00") %>% select(X1, X9)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X9))+geom_line()

# Extract corresponding piece of indicator dataframe:
macd_df <- macd %>% select(X1, X9) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X9.y, col = X9.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bun4 <- macd_df %>% select(X9.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#CADJPY
ggplot(prices, aes(X1, X24))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-19 13:30:00", X1 < "2020-11-19 16:15:00") %>% select(X1, X24)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X24))+geom_line()

# Extract corresponding piece of indicator dataframe:
macd_df <- macd %>% select(X1, X24) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X24.y, col = X24.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bun5 <- macd_df %>% select(X24.x) %>% to_m(32)
# --------------------

#########################################################################
# add new column to this matrix with value BUN
macd_m_bun1 <- transform(macd_m_bun1, M_T = "BUN")
macd_m_bun2 <- transform(macd_m_bun2, M_T = "BUN")
macd_m_bun3 <- transform(macd_m_bun3, M_T = "BUN")
macd_m_bun4 <- transform(macd_m_bun4, M_T = "BUN")
macd_m_bun5 <- transform(macd_m_bun5, M_T = "BUN")
##########################################################################


##########################################################################
## ---------- # 2. Bull volatile, BUV ---------------
##########################################################################
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# USDCAD
ggplot(prices, aes(X1, X6))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-20 23:45:00", X1 < "2020-11-21 02:30:00") %>% select(X1, X6)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X6))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X6) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X6.y, col = X6.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv1 <- macd_df %>% select(X6.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#USDCHF
ggplot(prices, aes(X1, X7))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-27 15:40:00", X1 < "2020-11-27 18:25:00") %>% select(X1, X7)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X7))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X7) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X7.y, col = X7.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv2 <- macd_df %>% select(X7.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# EURGBP
ggplot(prices, aes(X1, X9))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-26 21:50:00", X1 < "2020-11-27 00:35:00") %>% select(X1, X9)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X9))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X9) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X9.y, col = X9.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv3 <- macd_df %>% select(X9.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# EURCHF
ggplot(prices, aes(X1, X11))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-12 18:15:00", X1 < "2020-11-12 21:00:00") %>% select(X1, X11)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X11))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X11) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X11.y, col = X11.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv4 <- macd_df %>% select(X11.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#GBPCAD
ggplot(prices, aes(X1, X16))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-26 23:10:00", X1 < "2020-11-27 01:55:00") %>% select(X1, X16)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X16))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X16) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X16.y, col = X16.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv5 <- macd_df %>% select(X16.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# EURAUD
ggplot(prices, aes(X1, X14))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-27 18:50:00", X1 < "2020-11-27 21:35:00") %>% select(X1, X14)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X14))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X14) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X14.y, col = X14.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv6 <- macd_df %>% select(X14.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# EURAUD
ggplot(prices, aes(X1, X14))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-19 10:00:00", X1 < "2020-11-19 12:45:00") %>% select(X1, X14)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X14))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X14) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X14.y, col = X14.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv7 <- macd_df %>% select(X14.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# GBPAUD
ggplot(prices, aes(X1, X15))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-19 16:55:00", X1 < "2020-11-19 19:40:00") %>% select(X1, X15)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X15))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X15) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X15.y, col = X15.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv8 <- macd_df %>% select(X15.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#NZDCHF
ggplot(prices, aes(X1, X28))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-16 21:10:00", X1 < "2020-11-16 23:55:00") %>% select(X1, X28)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X28))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X28) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X28.y, col = X28.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv9 <- macd_df %>% select(X28.x) %>% to_m(32)
# --------------------

#########################################################################
macd_m_buv1 <- transform(macd_m_buv1, M_T = "BUV") 
macd_m_buv2 <- transform(macd_m_buv2, M_T = "BUV")
macd_m_buv3 <- transform(macd_m_buv3, M_T = "BUV") 
macd_m_buv4 <- transform(macd_m_buv4, M_T = "BUV") 
macd_m_buv5 <- transform(macd_m_buv5, M_T = "BUV")
macd_m_buv6 <- transform(macd_m_buv6, M_T = "BUV")
macd_m_buv7 <- transform(macd_m_buv7, M_T = "BUV")
macd_m_buv8 <- transform(macd_m_buv8, M_T = "BUV")
macd_m_buv9 <- transform(macd_m_buv9, M_T = "BUV") 
#########################################################################


##########################################################################
## ---------- # 3. Bear normal, BEN ---------------
##########################################################################
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#GPBUSD
ggplot(prices, aes(X1, X3))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-10-29 18:25:00", X1 < "2020-10-29 21:10:00") %>% select(X1, X3)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X3))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X3) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X3.y, col = X3.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ben1 <- macd_df %>% select(X3.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#NZDUSD
ggplot(prices, aes(X1, X5))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-12 10:25:00", X1 < "2020-11-12 13:10:00") %>% select(X1, X5)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X5))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X5) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X5.y, col = X5.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ben2 <- macd_df %>% select(X5.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# EURJPY
ggplot(prices, aes(X1, X10))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-10-21 16:55:00", X1 < "2020-10-21 19:40:00") %>% select(X1, X10)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X10))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X10) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X10.y, col = X10.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ben3 <- macd_df %>% select(X10.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#  EURCHF
ggplot(prices, aes(X1, X11))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-10-23 09:00:00", X1 < "2020-10-23 11:45:00") %>% select(X1, X11)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X11))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X11) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X11.y, col = X11.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ben4 <- macd_df %>% select(X11.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#  GBPCHF
ggplot(prices, aes(X1, X17))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-12-01 17:55:00", X1 < "2020-12-01 20:40:00") %>% select(X1, X17)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X17))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X17) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X17.y, col = X17.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ben5 <- macd_df %>% select(X17.x) %>% to_m(32)
# --------------------

#########################################################################
macd_m_ben1 <- transform(macd_m_ben1, M_T = "BEN")
macd_m_ben2 <- transform(macd_m_ben2, M_T = "BEN")
macd_m_ben3 <- transform(macd_m_ben3, M_T = "BEN")
macd_m_ben4 <- transform(macd_m_ben4, M_T = "BEN")
macd_m_ben5 <- transform(macd_m_ben5, M_T = "BEN")

#########################################################################


##########################################################################
## ---------- # 4. Bear volatile, BEV ---------------
##########################################################################
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# EURUSD
ggplot(prices, aes(X1, X2))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-26 16:50:00", X1 < "2020-11-26 19:35:00") %>% select(X1, X2)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X2))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X2) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X2.y, col = X2.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev1 <- macd_df %>% select(X2.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# GBPUSD
ggplot(prices, aes(X1, X3))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-25 17:10:00", X1 < "2020-11-25 19:55:00") %>% select(X1, X3)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X3))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X3) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X3.y, col = X3.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev2 <- macd_df %>% select(X3.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# EURCHF
ggplot(prices, aes(X1, X11))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-25 22:30:00", X1 < "2020-11-26 01:15:00") %>% select(X1, X11)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X11))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X11) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X11.y, col = X11.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev3 <- macd_df %>% select(X11.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#  EURNZD
ggplot(prices, aes(X1, X12))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-25 22:35:00", X1 < "2020-11-26 01:20:00") %>% select(X1, X12)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X12))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X12) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X12.y, col = X12.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev4 <- macd_df %>% select(X12.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# EURCAD
ggplot(prices, aes(X1, X13))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-04 20:30:00", X1 < "2020-11-04 23:15:00") %>% select(X1, X13)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X13))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X13) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X13.y, col = X13.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev5 <- macd_df %>% select(X13.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# GBPCAD
ggplot(prices, aes(X1, X16))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-10-23 22:50:00", X1 < "2020-10-24 01:35:00") %>% select(X1, X16)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X16))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X16) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X16.y, col = X16.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev6 <- macd_df %>% select(X16.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# CADJPY
ggplot(prices, aes(X1, X24))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-20 23:35:00", X1 < "2020-11-21 02:20:00") %>% select(X1, X24)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X24))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X24) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X24.y, col = X24.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev7 <- macd_df %>% select(X24.x) %>% to_m(32)
# --------------------
#########################################################################
macd_m_bev1 <- transform(macd_m_bev1, M_T = "BEV")
macd_m_bev2 <- transform(macd_m_bev2, M_T = "BEV")
macd_m_bev3 <- transform(macd_m_bev3, M_T = "BEV")
macd_m_bev4 <- transform(macd_m_bev4, M_T = "BEV")
macd_m_bev5 <- transform(macd_m_bev5, M_T = "BEV")
macd_m_bev6 <- transform(macd_m_bev6, M_T = "BEV")
macd_m_bev7 <- transform(macd_m_bev7, M_T = "BEV")
#########################################################################


##########################################################################
## ---------- # 5. Sideways quiet, RAN ---------------
##########################################################################
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#EURUSD
ggplot(prices, aes(X1, X2))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-18 05:55:00", X1 < "2020-11-18 08:45:00") %>% select(X1, X2)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X2))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X2) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X2.y, col = X2.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ran1 <- macd_df %>% select(X2.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# AUDUSD
ggplot(prices, aes(X1, X4))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-11 06:35:00", X1 < "2020-11-11 09:25:00") %>% select(X1, X4)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X4))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X4) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X4.y, col = X4.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ran2 <- macd_df %>% select(X4.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# EURGBP
ggplot(prices, aes(X1, X9))+geom_line()

  # Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-11 12:10:00", X1 < "2020-11-11 14:55:00") %>% select(X1, X9)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X9))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X9) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X9.y, col = X9.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ran3 <- macd_df %>% select(X9.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# GBPCHF
ggplot(prices, aes(X1, X17))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-12-03 12:15:00", X1 < "2020-12-03 15:00:00") %>% select(X1, X17)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X17))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X17) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X17.y, col = X17.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ran4 <- macd_df %>% select(X17.x) %>% to_m(32)
# --------------------
# --------------------



# Choose the asset corresponding to this period /find by replacing 'y' argument/
# EURNZD
ggplot(prices, aes(X1, X12))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-12-02 01:35:00", X1 < "2020-12-02 04:20:00") %>% select(X1, X12)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X12))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X12) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X12.y, col = X12.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ran5 <- macd_df %>% select(X12.x) %>% to_m(32)
# --------------------
#########################################################################
macd_m_ran1 <- transform(macd_m_ran1, M_T = "RAN") 
macd_m_ran2 <- transform(macd_m_ran2, M_T = "RAN") 
macd_m_ran3 <- transform(macd_m_ran3, M_T = "RAN") 
macd_m_ran4 <- transform(macd_m_ran4, M_T = "RAN") 
macd_m_ran5 <- transform(macd_m_ran5, M_T = "RAN") 
#########################################################################

##########################################################################
## ---------- # 6. Sideways volatile, RAV ---------------
##########################################################################
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
#EURUSD
ggplot(prices, aes(X1, X2))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-18 16:55:00", X1 < "2020-11-18 19:40:00") %>% select(X1, X2)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X2))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X2) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X2.y, col = X2.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_rav1 <- macd_df %>% select(X2.x) %>% to_m(32)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
# EURGBP
ggplot(prices, aes(X1, X9))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-13 19:30:00", X1 < "2020-11-13 22:15:00") %>% select(X1, X9)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X9))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X9) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X9.y, col = X9.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_rav2 <- macd_df %>% select(X9.x) %>% to_m(32)
# --------------------

# Choose the asset corresponding to this period /find by replacing 'y' argument/
#  GBPJPY
ggplot(prices, aes(X1, X18))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-13 06:45:00", X1 < "2020-11-13 09:35:00") %>% select(X1, X18)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X18))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X18) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X18.y, col = X18.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_rav3 <- macd_df %>% select(X18.x) %>% to_m(32)
# --------------------


# Choose the asset corresponding to this period /find by replacing 'y' argument/
#  USDJPY
ggplot(prices, aes(X1, X8))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-14 03:20:00", X1 < "2020-11-14 06:05:00") %>% select(X1, X8)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X8))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X8) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X8.y, col = X8.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_rav4 <- macd_df %>% select(X8.x) %>% to_m(32)
# --------------------


# Choose the asset corresponding to this period /find by replacing 'y' argument/
# AUDUSD
ggplot(prices, aes(X1, X4))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2020-11-18 03:15:00", X1 < "2020-11-18 06:00:00") %>% select(X1, X4)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X4))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X4) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X4.y, col = X4.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_rav5 <- macd_df %>% select(X4.x) %>% to_m(32)
# --------------------







#########################################################################
macd_m_rav1 <- transform(macd_m_rav1, M_T = "RAV")
macd_m_rav2 <- transform(macd_m_rav2, M_T = "RAV")
macd_m_rav3 <- transform(macd_m_rav3, M_T = "RAV")
macd_m_rav4 <- transform(macd_m_rav4, M_T = "RAV")
macd_m_rav5 <- transform(macd_m_rav5, M_T = "RAV")
#########################################################################
#########################################################################
#########################################################################

# Combine all of that :)
macd_ML2 <- rbind(macd_m_bun1,macd_m_bun2,macd_m_bun3,macd_m_bun4,macd_m_bun5,
                  macd_m_buv1,macd_m_buv2,macd_m_buv3,macd_m_buv4,macd_m_buv5,macd_m_buv6,macd_m_buv7,macd_m_buv8,macd_m_buv9,
                  macd_m_ben1,macd_m_ben2,macd_m_ben3,macd_m_ben4,macd_m_ben5,
                  macd_m_bev1,macd_m_bev2,macd_m_bev3,macd_m_bev4,macd_m_bev5,macd_m_bev6,macd_m_bev7,
                  macd_m_ran1,macd_m_ran2,macd_m_ran3,macd_m_ran4,macd_m_ran5,
                  macd_m_rav1,macd_m_rav2,macd_m_rav3,macd_m_rav4,macd_m_rav5)

### NOTE Number of rows Matrices needs to be roughly equal

# Optionally record data into the folder
if(!dir.exists(file.path(getwd(),"_DATA"))){
  dir.create(file.path(getwd(),"_DATA"))
  write_rds(macd_ML2, file.path(getwd(),"_DATA", "macd_ML5M.rds"))
} else
{
  write_rds(macd_ML2, file.path(getwd(),"_DATA", "macd_ML5M.rds"))
  
}
  

## Visualize new matrix in 3D
plot_ly(z = as.matrix(macd_ML2[,1:32]), type = "surface")

#### End