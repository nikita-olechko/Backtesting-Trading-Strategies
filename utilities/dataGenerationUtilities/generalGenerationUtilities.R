find_most_NAs_in_xts_object_columns <- function(data_xts){
  # Count the number of NAs in each column
  na_counts <- apply(is.na(data_xts), 2, sum) 
  
  # Find the column name with the maximum number of NAs
  max_na_column <- names(which.max(na_counts))
  
  # Get the count of NAs in the column with the most NAs
  max_na_count <- na_counts[max_na_column]
  if (as.numeric(max_na_count) > 0){
    return (as.numeric(max_na_count)+5)
  }
  else {
    return (5)
  }
}

add_Analysis_Data_To_Historical_Data <- function(stk_data, ticker){
  ticker_wap <- paste0(ticker, ".WAP")
  stk_data$Orders <- 0
  stk_data$Position <- 0
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 50, ma = "SMA")
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 20, ma = "SMA")
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 20, ma = "EMA")
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 20, ma = "WMA")
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 10, ma = "SMA")
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 10, ma = "EMA")
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 10, ma = "WMA")
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 5, ma = "SMA")
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 5, ma = "EMA")
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 5, ma = "WMA")
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 3, ma = "SMA")
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 3, ma = "EMA")
  # stk_data <- create_bollinger_bands(stk_data, ticker, periods = 3, ma = "WMA")
  # stk_data <- add_SD_To_Data(stk_data, ticker_wap)
  return(stk_data)
}


create_summary_data <- function(stk_data, ticker, summary_df = NULL){
  ticker_wap <- paste0(ticker, ".WAP")
  stk_data$holdingGrossReturn <- as.numeric(stk_data[, ticker_wap])/as.numeric(stk_data[, ticker_wap][1])
  # stk_data$returnOverHolding <-  stk_data[, ticker_wap]+stk_data[, "Position"]
  trade_completed_indices <- which(stk_data$Orders == -1)
  # Summarize position
  average_position_post_trade <- stk_data$Position[trade_completed_indices] %>% mean()
  sd_position_post_trade <- stk_data$Position[trade_completed_indices] %>% sd()
  max_position_post_trade <- stk_data$Position[trade_completed_indices] %>% max()
  min_position_post_trade <- stk_data$Position[trade_completed_indices] %>% min()
  
  changes_in_position_per_trade <- c()
  for (index in 2:length(trade_completed_indices)){
    change_in_position <- as.numeric(stk_data$Position[trade_completed_indices][index]) - as.numeric(stk_data$Position[trade_completed_indices][index-1])
    changes_in_position_per_trade <- append(changes_in_position_per_trade, change_in_position)
  }
  
  # Summarize changes in position
  average_change_in_position_per_trade <- mean(changes_in_position_per_trade)
  sd_change_in_position_per_trade <- sd(changes_in_position_per_trade)
  min_change_in_position_per_trade <- min(changes_in_position_per_trade)
  max_change_in_position_per_trade <- max(changes_in_position_per_trade)
  
  #Summarize holding gross return
  final_holding_gross_return <- as.numeric(stk_data$holdingGrossReturn[-1])
  average_holding_gross_return <- as.numeric(stk_data$holdingGrossReturn) %>% mean()
  sd_holding_gross_return <- as.numeric(stk_data$holdingGrossReturn) %>% sd()
  min_holding_gross_return <- as.numeric(stk_data$holdingGrossReturn) %>% min()
  max_holding_gross_return <- as.numeric(stk_data$holdingGrossReturn) %>% max()

  #Summarize absolute return
  
  #Summarize return over holding
  
  # Create a dataframe with the computed variables
  new_summary <- data.frame(
  ticker = ticker,
  AveragePosition = average_position_post_trade,
  SDPosition = sd_position_post_trade,
  MaxPosition = max_position_post_trade,
  MinPosition = min_position_post_trade,
  AvgChangeInPosition = average_change_in_position_per_trade,
  SDChangeInPosition = sd_change_in_position_per_trade,
  MinChangeInPosition = min_change_in_position_per_trade,
  MaxChangeInPosition = max_change_in_position_per_trade
  )
  if (!(is.null(summary_df))){
    new_summary <- rbind(summary_df, new_summary)
  }
  return (new_summary)
}



