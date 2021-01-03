#ini files for production terminals

library(lazytrade)

## TERMINAL 1
# test file for MT4 start with specific parameters
write_ini_file(mt4_Profile = "Florian, Louis, Salomon Assous",
               mt4_Login = "8000766",
               mt4_Password = "NGzZm1Rr", 
               mt4_Server = "OANDA-OGM Live-1",
               dss_inifilepath = "C:/Program Files (x86)/MT4 - Terminal 1",
               dss_inifilename = "prod_T1.ini",
               dss_mode = "prod")

## TERMINAL 2
# test file for MT4 start with specific parameters
write_ini_file(mt4_Profile = "Florian Louis Salomon Assous",
               mt4_Login = "24258755",
               mt4_Password = "6RqJo1zN0SHs", 
               mt4_Server = "AdmiralMarkets-Demo",
               dss_inifilepath = "C:/Program Files (x86)/MT4 - Terminal 2",
               dss_inifilename = "prod_T2.ini",
               dss_mode = "prod")

## TERMINAL 3
# test file for MT4 start with specific parameters
write_ini_file(mt4_Profile = "Florian Louis Salomon Assous",
               mt4_Login = "24258758",
               mt4_Password = "RF63koVJ6i9Y", 
               mt4_Server = "AdmiralMarkets-Demo",
               dss_inifilepath = "C:/Program Files (x86)/MT4 - Terminal 3",
               dss_inifilename = "prod_T3.ini",
               dss_mode = "prod")

## TERMINAL 4
# test file for MT4 start with specific parameters
# write_ini_file(mt4_Profile = "Profile_Name",
#                mt4_Login = "1234567",
#                mt4_Password = "xxyyzzyy", 
#                mt4_Server = "Broker Server Name",
#                dss_inifilepath = "C:/Program Files (x86)/MT4 - Terminal 4Ω",
#                dss_inifilename = "prod_T4.ini",
#                dss_mode = "prod")
# 