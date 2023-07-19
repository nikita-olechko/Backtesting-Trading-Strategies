#Data Generation Utilities

# Functions related to calculating Bollinger Bands
library("TTR")


#' A function to create 1 and 2 sd, upper and lower Bollinger Bands along a stock
create_BBands <- function(stk_data, ticker, maName, ma, sma_length = 20) {
  tickerWAP <- paste0(ticker, ".WAP")
  stk_ma <- stk_data[, maName]
  BBandNameList <- create_Bband_Names(ma, sma_length)
  stk_data$BBand <- rollapply(
    stk_ma,
    width = sma_length,
    FUN = calculate_Upper_Bollinger_Band,
    wap = stk_data[, tickerWAP],
    align = "right",
    fill = NA,
    by.column = FALSE,
    partial = TRUE
  )
  names(stk_data)[names(stk_data) == "BBand"] <- toString(BBandNameList[1])
  # colnames(stk_data)["BBand"] <- toString(BBandNameList[1])
  stk_data$BBand <- rollapply(
    stk_ma,
    width = sma_length,
    FUN = calculate_Lower_Bollinger_Band,
    wap = stk_data[, tickerWAP],
    align = "right",
    fill = NA,
    by.column = FALSE,
    partial = TRUE
  )
  names(stk_data)[names(stk_data) == "BBand"] <- toString(BBandNameList[2])
  stk_data$BBand <- rollapply(
    stk_ma,
    width = sma_length,
    FUN = calculate_Upper_Bollinger_Band,
    wap = stk_data[, tickerWAP],
    deviations = 1,
    align = "right",
    fill = NA,
    by.column = FALSE,
    partial = TRUE
  )
  names(stk_data)[names(stk_data) == "BBand"] <- toString(BBandNameList[3])
  stk_data$BBand <- rollapply(
    stk_ma,
    width = sma_length,
    FUN = calculate_Lower_Bollinger_Band,
    wap = stk_data[, tickerWAP],
    deviations = 1,
    align = "right",
    fill = NA,
    by.column = FALSE,
    partial = TRUE
  )
  names(stk_data)[names(stk_data) == "BBand"] <- toString(BBandNameList[4])
  print(paste("stk_data with BBands created:", maName))
  return(stk_data)
}

#' A helper function to calculate the upper bollinger band for a given list of sma
calculate_Upper_Bollinger_Band <- function(sma, wap, deviations = 2) {
  dummy <- xts(rep(NA, nrow(sma)), order.by = index(sma))
  
  # Merge dummy and wap based on dates
  wap_indexed <- merge(dummy, wap, by = "row.names", all = FALSE)
  
  # Remove first column (which contains the row names)
  wap_indexed <- wap_indexed[, -c(1, 3)]
  upper_BBand <- as.numeric(sma[length(sma)]) + deviations * sd(wap_indexed)
  return(upper_BBand)
}

#' A helper function to calculate the lower bollinger band for a given list of sma
calculate_Lower_Bollinger_Band <- function(sma, wap, deviations = 2) {
  dummy <- xts(rep(NA, nrow(sma)), order.by = index(sma))
  
  # Merge dummy and wap based on dates
  wap_indexed <- merge(dummy, wap, by = "row.names", all = FALSE)
  
  # Remove first column (which contains the row names)
  wap_indexed <- wap_indexed[, -c(1, 3)]
  lower_BBand <- as.numeric(sma[length(sma)]) - deviations * sd(wap_indexed)
  return(lower_BBand)
}


#' A function that creates upper and lower bollinger bands from historical price data
#' Note that 'ma' stands for Moving Average
create_bollinger_bands <- function(stk_data, ticker, periods = 20, ma = "SMA"){
  tickerWAP <- paste0(ticker, ".WAP")
  maName <- paste0(periods, "Period", ma)
  maFunc <- get_MA_function(ma)
  stk_data$maName <- maFunc(stk_data[, tickerWAP], periods)
  names(stk_data)[names(stk_data) == "maName"] <- maName
  stk_data <- create_BBands(stk_data, ticker, maName, ma, sma_length = periods)
  return (stk_data)
}


get_MA_function <- function(ma){
  if (ma == "SMA"){
    return(SMA)
  }
  if (ma == "EMA"){
    return(EMA)
  }
  if (ma == "WMA"){
    return(WMA)
  }
  if (ma == "DEMA"){
    return(DEMA)
  }
  if (ma == "EVWMA"){
    return(EVWMA)
  }
  if (ma == "ZLEMA"){
    return(ZLEMA)
  }
  else{
    print("Moving Average Strategy Not Recognized")
    return(NULL)
  }
}

create_Bband_Names <- function(ma, periods){
  upperBband1SD <- paste0("upperBband2SD", periods, "Periods", ma)
  upperBband2SD <- paste0("lowerBband2SD", periods, "Periods", ma)
  lowerBband1SD <- paste0("upperBband1SD", periods, "Periods", ma)
  lowerBband2SD <- paste0("lowerBband1SD", periods, "Periods", ma)
  nameList <- list(upperBband1SD, upperBband2SD, lowerBband1SD, lowerBband2SD)
  return(nameList)
}

add_SD_To_Data <- function(stk_data, tickerWAP){
  stk_data$SD20Days <- rollapply(
    stk_data[, tickerWAP],
    width = 20,
    FUN = sd,
    align = "right",
    fill = NA,
    by.column = FALSE,
    partial = TRUE
  )
  stk_data$SD10Days <- rollapply(
    stk_data[, tickerWAP],
    width = 10,
    FUN = sd,
    align = "right",
    fill = NA,
    by.column = FALSE,
    partial = TRUE
  )  
  stk_data$SD5Days <- rollapply(
    stk_data[, tickerWAP],
    width = 5,
    FUN = sd,
    align = "right",
    fill = NA,
    by.column = FALSE,
    partial = TRUE
  )
  return(stk_data)
}