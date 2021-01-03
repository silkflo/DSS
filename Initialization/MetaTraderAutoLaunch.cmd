rem *************************************************
rem *** This starts the terminals after waiting 30 seconds ***
rem *************************************************

ping localhost -n 30

rem Starting terminals with parameters using *.ini files:
start "1" "C:\Program Files (x86)\MT4 - Terminal 1\terminal.exe" /portable "C:\Program Files (x86)\MT4 - Terminal 1\prod_T1.ini"
start "2" "C:\Program Files (x86)\MT4 - Terminal 2\terminal.exe" /portable "C:\Program Files (x86)\MT4 - Terminal 2\prod_T2.ini"
start "3" "C:\Program Files (x86)\MT4 - Terminal 3\terminal.exe" /portable "C:\Program Files (x86)\MT4 - Terminal 3\prod_T3.ini"
start "4" "C:\Program Files (x86)\MT4 - Terminal 4\terminal.exe" /portable "C:\Program Files (x86)\MT4 - Terminal 4\prod_T4.ini"
start "5" "C:\Program Files\Google\Chrome\Application\chrome.exe"

exit