rem Script to Deploy files from Version Control repository to Development Terminal
rem Use in case some content needs to be replaced (reverted from Version Control History)

@echo off
setlocal enabledelayedexpansion

:: Source Directory where Version Control Repository is located
set SOURCE_DIR="E:\trading\Git\Include"
:: Destination Directory where Expert Advisor is located
set DEST_DIR="C:\Program Files (x86)\AM MT4 - Terminal 2\MQL4\Include"

ROBOCOPY %SOURCE_DIR% %DEST_DIR% *.mq4

