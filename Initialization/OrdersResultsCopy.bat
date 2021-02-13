
rem Script to Sync Files from Development Terminal to Version Control

@echo off
setlocal enabledelayedexpansion

:: Source Directory where OrderResults is located
:: set SOURCE_DIR1="C:\Program Files (x86)\MT4 - Terminal 1\MQL4\Files\OrdersResultsT1"
set SOURCE_DIR2="C:\Program Files (x86)\MT4 - Terminal 2\MQL4\Files\OrdersResultsT2"
:: Destination Directory where Version Control Repository is located
:: set DEST_DIR1="C:\DSS\_DATA\OrdersResultsT1" 
set DEST_DIR2="C:\DSS\_DATA\OrdersResultsT2"

:: Copy only files with *.csv extension
:: ROBOCOPY %SOURCE_DIR1% %DEST_DIR1% *.csv
ROBOCOPY %SOURCE_DIR2% %DEST_DIR2% *.csv
