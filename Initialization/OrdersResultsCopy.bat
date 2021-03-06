
rem Script to Sync Files from Development Terminal to Version Control

@echo off
setlocal enabledelayedexpansion

:: Source Directory where OrderResults is located
set SOURCE_DIR1="C:\Program Files (x86)\MT4 - Terminal 1\MQL4\Files"
set SOURCE_DIR2="C:\Program Files (x86)\MT4 - Terminal 2\MQL4\Files"
set SOURCE_DIR3="C:\Program Files (x86)\MT4 - Terminal 3\MQL4\Files"
set SOURCE_DIR4="C:\Program Files (x86)\MT4 - Terminal 4\MQL4\Files"
set SOURCE_DIR5="C:\Program Files (x86)\MT4 - Terminal 5\MQL4\Files"
:: Destination Directory where Version Control Repository is located
set DEST_DIR="C:\DSS\_DATA" 


:: Copy only files with *.csv extension
ROBOCOPY %SOURCE_DIR1% %DEST_DIR% *OrdersResultsT1.csv
ROBOCOPY %SOURCE_DIR2% %DEST_DIR% *OrdersResultsT2.csv
ROBOCOPY %SOURCE_DIR3% %DEST_DIR% *OrdersResultsT3.csv
ROBOCOPY %SOURCE_DIR4% %DEST_DIR% *OrdersResultsT4.csv
ROBOCOPY %SOURCE_DIR5% %DEST_DIR% *OrdersResultsT5.csv

ROBOCOPY %SOURCE_DIR1% %DEST_DIR% *AccountResultsT1.csv
ROBOCOPY %SOURCE_DIR2% %DEST_DIR% *AccountResultsT2.csv
ROBOCOPY %SOURCE_DIR3% %DEST_DIR% *AccountResultsT3.csv
ROBOCOPY %SOURCE_DIR4% %DEST_DIR% *AccountResultsT4.csv
ROBOCOPY %SOURCE_DIR5% %DEST_DIR% *AccountResultsT5.csv
