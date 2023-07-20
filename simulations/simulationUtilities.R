# Functions related to modularized and reusable strategy simulation


#' Generic function to run a strategy on NYSE tickers in database
#' Can potentially speed up function by employing list of tickers to ignore in errored tickers as a set
run_Strategy_On_List_Of_Tickers <- function(tws, strategy, strategy_Buy_Or_Sell_Condition_Function, generate_Additional_Data_Function = NULL, 
                                            barsize = "1 day", duration = "3 Y", whatToShow = "TRADES", listOfTickers = read.csv("nyse-listed.csv")$ACT.Symbol, strategy_period_offset = 20){
  
  #Get Errored Tickers
  add_Errored_Tickers_To_Log("INITIALIZATION_TICKER")
  errLogFilePath <- file.path(get_Working_Directory_As_Path(),"data","ErroredTickers")
  csvFilePathName <- file.path(errLogFilePath, "ErroredTickers.csv")
  erredTickers <- read.csv(csvFilePathName, header = FALSE, stringsAsFactors = FALSE)
  
  # folderName <- create_Folder_Name(barsize, duration, strategy = strategy)
  folderPath <- paste0(get_Working_Directory_As_Path(), "\\data\\Strategy Results\\")
  summary_file_path_name <- paste0(folderPath, str_replace_all(strategy, " ", ""), str_replace_all(barsize, " ", ""), str_replace_all(duration, " ", ""), ".csv")
  tryCatch({
    all_tickers_summary <- read.csv(summary_file_path_name)
    if ("X" %in% colnames(all_tickers_summary)) {
      all_tickers_summary$X <- NULL
    }
  }, error = function(e) {
    cat("Couldn't read all_tickers_summary:", conditionMessage(e), "\n")
  })
  if (exists("all_tickers_summary")){
    completed_tickers <- all_tickers_summary$ticker
    listOfTickers <- listOfTickers[!(listOfTickers %in% completed_tickers)]
  }
  else{
    all_tickers_summary <- data.frame() 
  }
  listOfTickers <- listOfTickers[!(listOfTickers %in% erredTickers$V1)]
  for (ticker in listOfTickers) {
    #run garbage collector
    gc()
    stk_data <- retrieve_Base_Data(ticker, barsize = barsize, duration = duration)
    if (!is.null(stk_data)){
    summary_df <- simulate_Trading_On_Strategy(stk_data, ticker, strategy_Buy_Or_Sell_Condition_Function, 
                                             generate_Additional_Data_Function = generate_Additional_Data_Function, barsize = barsize, duration = duration, strategy_period_offset = strategy_period_offset)
    all_tickers_summary <- rbind(all_tickers_summary, summary_df)
    write.csv(all_tickers_summary , summary_file_path_name)
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


#' A function to write stock simulation data to a custom file 
write_Updated_Data <- function(data_to_write ,ticker, barsize, strategy){
  fileName <- createFileName(ticker, barsize, duration, strategy = strategy)
  folderPath <- paste0(get_Working_Directory_As_Path(), "\\data\\Strategy Results\\")
  write_Zoo_Data_To_Folder(data_to_write, fileName, folderPath)
}



#' A function that simulates trading on a stock, based on a given strategy.
#' @param stk_data an xts dataframe containing stock data
#' @param ticker a string containing the name of a stock
#' @param strategy_Buy_Or_Sell_Condition_Function a function that returns 1, -1, or 0 based on some market conditions
#' @param generate_Additional_Data_Function a function that generates additional columns in stk_data if necessary for the strategy
simulate_Trading_On_Strategy <- function(stk_data, ticker, strategy_Buy_Or_Sell_Condition_Function, generate_Additional_Data_Function = NULL, barsize = "1 day", duration = "1 Y", strategy_period_offset = 20) {
  ticker_wap <- paste0(ticker, ".WAP")
  if (!(is.null(generate_Additional_Data_Function))){
    stk_data <- generate_Additional_Data_Function(stk_data, ticker_wap)
  }
  last_order_index <- 1
  #Create Buy and Sell Variables
  
  #Remove NA values
  stk_data = na.omit(stk_data)
  for (index in (strategy_period_offset+1):nrow(stk_data)) {
    # Get order based on strategy conditions
    order <- strategy_Buy_Or_Sell_Condition_Function(stk_data, ticker_wap, index, last_order_index)
    
    # If this is the first order, wait until first Buy order to establish the position
    if (last_order_index == 1){
      if(order == 1){
        stk_data$Orders[index] <- 1
        stk_data$Position[index] <- - as.numeric(stk_data[, ticker_wap][index])
        last_order_index <- index
        next()
      }
      else{
        next()
      }
    }
    else {
      order_function_outputs <- order_selector(order)(stk_data, index, last_order_index, ticker_wap)
      stk_data <- order_function_outputs[[1]]
      last_order_index <- order_function_outputs[[2]]
    }
  }
  if (toString(stk_data$Orders[last_order_index]) == 1){
    stk_data$Orders[index] <- -1
    last_order_index <- index
    stk_data$Position[index] <- as.numeric(stk_data$Position[index - 1]) + as.numeric(stk_data[, ticker_wap][index])
  }
  summary_df <- create_summary_data(stk_data, ticker)
  return (summary_df)
}



