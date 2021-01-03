@echo off
:: Use this line of code for debugging purposes *** Adapt the paths for R installation and R script!!!
::"C:\Program Files\R\R-4.0.3\bin\R.exe" CMD BATCH "C:\DSS\R_tradecontrol\_RL\TradeTriggerRL.R"
:: Use this code in 'production'
"C:\Program Files\R\R-4.0.3\bin\Rscript.exe" "C:\DSS\R_tradecontrol\_RL\TradeTriggerRL.R"