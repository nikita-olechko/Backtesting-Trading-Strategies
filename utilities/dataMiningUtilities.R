write_All_Historical_NYSE_Equity_Data <- function(barsize = "1 day", duration = "3 Y", whatToShow = "TRADES"){
  for (ticker in nyseStocks) {
    retrieve_Base_Data(ticker, barsize = barsize, duration = duration)
  }
}

write_Largest_Market_Cap_Stocks_Historical_Equity_Data <- function(barsize = "1 day", duration = "3 Y", whatToShow = "TRADES"){
  largeMarketCapStocks <- read.csv("largest-market-cap.csv")
  largeMarketCapStocks <- largeMarketCapStocks$Symbol
  for (ticker in largeMarketCapStocks) {
    retrieve_Base_Data(ticker, barsize = barsize, duration = duration)
  } 
}


