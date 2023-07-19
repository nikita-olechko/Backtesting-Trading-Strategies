# Functions related to modularized and reusable strategy simulation


#' Generic function to run a strategy on NYSE tickers in database
#' Can potentially speed up function by employing list of tickers to ignore in errored tickers as a set
run_Strategy_On_NYSE_Stocks <- function(tws, strategy, strategy_Buy_Or_Sell_Condition_Function, generate_Additional_Data_Function = FALSE, barsize = "1 day", duration = "3 Y", whatToShow = "TRADES"){
  
  nyseStocks <- read.csv("nyse-listed.csv")
  nyseStocks <- nyseStocks$ACT.Symbol
  erredTickers <- unique(read.csv("data/ErroredTickers/ErroredTickers-NikitasM16.csv"))
  erredTickers <- erredTickers$AA.B
  
  #Create data directory for results
  create_Data_Results_Directory(barsize, duration, strategy = strategy)

  folderName <- create_Folder_Name(barsize, duration, strategy = strategy)
  folderPath <- paste0(get_Working_Directory_As_Path(), "\\data\\", folderName, "\\")
  
  for (ticker in nyseStocks) {
    # Skip tickers that previously erred
    if (ticker %in% erredTickers){
      print(paste("Skip Previously Errored Ticker:", ticker))
      next()
    }
    #run garbage collector
    gc()
    filename <- createFileName(ticker, barsize, duration, strategy)
    # if this data does not already exist
    if (!file_Exists_In_Folder(filename, folderPath = folderPath)){
      stk_data <- retrieve_Base_Data(ticker)
      if (!is.null(stk_data)){
      stk_data <- simulate_Trading_On_Strategy(stk_data, ticker, strategy_Buy_Or_Sell_Condition_Function, generate_Additional_Data_Function = generate_Additional_Data_Function)
      }
    }
    # if the data already exists (the program was restarted or something)
    else{ 
      next()
    }
  }
}

#' A function that retrieves base stock_data either locally or from IBKR
retrieve_Base_Data <- function(ticker, exchange = "SMART", barsize = "1 day", duration = "3 Y", whatToShow = "TRADES"){
  security <- twsSTK(ticker, exchange)
  stk_data <- get_Stock_Data(tws, security, barsize = barsize, duration = duration)
  if (is.null(stk_data)){
    add_Errored_Tickers_To_Log(ticker)
    return(NULL)
  }
  return (stk_data)
}


#' A function to write stock simulation data to a custom folder 
write_Updated_Data <- function(data_to_write ,ticker, barsize, strategy){
  fileName <- createFileName(ticker, barsize, duration, strategy = strategy)
  folderName <- create_Folder_Name(barsize, duration, strategy = strategy)
  folderPath <- paste0(get_Working_Directory_As_Path(), "\\data\\", folderName, "\\")
  write_Zoo_Data_To_Folder(data_to_write, fileName, folderPath)
}



#' A function that simulates trading on a stock, based on a given strategy.
#' @param stk_data an xts dataframe containing stock data
#' @param ticker a string containing the name of a stock
#' @param strategy_Buy_Or_Sell_Condition_Function a function that returns "Buy", "Sell", or 0 based on some market conditions
#' @param generate_Additional_Data_Function a function that generates additional columns in stk_data if necessary for the strategy
simulate_Trading_On_Strategy <- function(stk_data, ticker, strategy_Buy_Or_Sell_Condition_Function, generate_Additional_Data_Function = FALSE) {
  tickerWAP <- paste0(ticker, ".WAP")
  if (generate_Additional_Data_Function != FALSE){
    stk_data <- generate_Additional_Data_Function(stk_data, tickerWAP)
  }
  last_order_index <- 1
  #Create Buy and Sell Variables
  for (index in 1:nrow(stk_data)) {
    #ignore NAs in sma until we have enough data
    if (is.na(stk_data$sma[index])) {
      next()
    }
    # Get order based on strategy conditions
    order <- strategy_Buy_Or_Sell_Condition_Function(stk_data, tickerWAP, index, last_order_index)
    
    # If this is the first order, establish the position
    if (last_order_index == 1){
      if(order == "Buy"){
        stk_data$Orders[index] <- "Buy"
        stk_data$Position[index] <- - as.numeric(stk_data[, tickerWAP][index])
        last_order_index <- index
        next()
      }
      else{
        next()
      }
    }
    
    # index != length(stk_data$Position) this condition makes sure we do not buy on the very last row
    if (order == "Buy" & index != length(stk_data$Position)){
      stk_data$Orders[index] <- "Buy"
      last_order_index <- index
      stk_data$Position[index] <- as.numeric(stk_data$Position[index - 1]) - as.numeric(stk_data[, tickerWAP][index])
    }
    else if (order == "Sell"){
      stk_data$Orders[index] <- "Sell"
      last_order_index <- index
      stk_data$Position[index] <- as.numeric(stk_data$Position[index - 1]) + as.numeric(stk_data[, tickerWAP][index])
    }
    else{
      stk_data$Position[index] <- stk_data$Position[index-1]
      next()
    }
  }
  #if the last order we did was buy, sell to get our net position
  if (toString(stk_data$Orders[last_order_index]) == "Buy"){
    stk_data$Orders[index] <- "Sell"
    last_order_index <- index
    stk_data$Position[index] <- as.numeric(stk_data$Position[index - 1]) + as.numeric(stk_data[, tickerWAP][index])
  }
  
  print(stk_data$Position[nrow(stk_data)])
  # stk_data$holdingGrossReturn[index] <-  - as.numeric(stk_data[, tickerWAP][1]) + as.numeric(stk_data[, tickerWAP][index])
  return (stk_data)
}



