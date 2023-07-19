library("IBrokers")
library("tidyverse")
library("ichimoku")
library("zoo")

# Source ibkrUtilities functions
source('utilities/ibkrUtilities.R')
source('utilities/dataMiningUtilities.R')
source('MACFunctions/MACSimulations.R')
source('MACFunctions/MACFunctions.R')
source('BollingerBands/BollingerSimulation.R')


#Connect to IBKR
# tws <- twsConnect(port = 7496)
tws <- twsConnect(port = 4000)
isConnected(tws)

ticker = "XOM"
barsize = "5 secs"
duration = "1 D"
# retrieve_Base_Data(ticker, barsize = barsize, duration = duration)

contract <- twsEquity(ticker,"SMART","SMART")
# reqMktData(tws, contract)
fh <- file('out.csv',open='a')
reqMktData(tws, contract, file=fh)
close(fh)
cancelMktData(tws,ticker)

data <- read.table("out.csv")

