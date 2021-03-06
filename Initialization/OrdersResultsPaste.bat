
rem Script to Sync Files from Development Terminal to Version Control

@echo off
setlocal enabledelayedexpansion

:: Source Directory where OrderResults is located
set SOURCE_DIR="C:\DSS\_DATA" 
:: Destination Directory where Version Control Repository is located
set DEST_DIR1="C:\Program Files (x86)\AM MT4 - Terminal 1\MQL4\Files"
set DEST_DIR2="C:\Program Files (x86)\AM MT4 - Terminal 2\MQL4\Files"
set DEST_DIR3="C:\Program Files (x86)\AM MT4 - Terminal 3\MQL4\Files"
set DEST_DIR4="C:\Program Files (x86)\AM MT4 - Terminal 4\MQL4\Files"
::set DEST_DIR5="C:\Program Files (x86)\MT4 - Terminal 5\MQL4\Files"



:: Copy only files with *.csv extension
ROBOCOPY %SOURCE_DIR% %DEST_DIR1% *OrdersResultsT1.csv
ROBOCOPY %SOURCE_DIR% %DEST_DIR2% *OrdersResultsT2.csv
ROBOCOPY %SOURCE_DIR% %DEST_DIR3% *OrdersResultsT3.csv
ROBOCOPY %SOURCE_DIR% %DEST_DIR4% *OrdersResultsT4.csv
::ROBOCOPY %SOURCE_DIR% %DEST_DIR% *OrdersResultsT5.csv

:: Copy only files with *.csv extension
ROBOCOPY %SOURCE_DIR% %DEST_DIR1% *AccountResultsT1.csv
ROBOCOPY %SOURCE_DIR% %DEST_DIR2% *AccountResultsT2.csv
ROBOCOPY %SOURCE_DIR% %DEST_DIR3% *AccountResultsT3.csv
ROBOCOPY %SOURCE_DIR% %DEST_DIR4% *AccountResultsT4.csv
::ROBOCOPY %SOURCE_DIR% %DEST_DIR% *AccountResultsT5.csv
