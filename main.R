library("IBrokers")
library("tidyverse")
library("ichimoku")
library("zoo")

# Source ibkrUtilities functions
source('utilities/ibkrUtilities.R')
source('utilities/dataMiningUtilities.R')
source('utilities/dataGenerationUtilities/generalGenerationUtilities.R')
source('simulations/simulationUtilities.R')
source('strategies/60PeriodSMA.R')

#Connect to IBKR
# tws <- twsConnect(port = 7496)
tws <- twsConnect(port = 4000, clientId = 2)
isConnected(tws)
# retrieve_Base_Data(ticker, barsize = barsize, duration = duration)
strategy = "5PeriodSmaTest"
run_Strategy_On_List_Of_Tickers(tws, strategy, sampleSMABuySellStrategy, 
                                        generate_Additional_Data_Function = generate5PeriodSMA, 
                                        barsize = "1 day", duration = "3 M", strategy_period_offset=1)
