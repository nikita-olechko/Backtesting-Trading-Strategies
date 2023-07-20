#A test strategy for the backtesting framework
generate5PeriodSMA <- function(stk_data, ticker_wap, period=5){
  stk_data$sma <- rollapplyr(
    stk_data[, ticker_wap],
    width = period,
    FUN = mean,
    align = "right",
    fill = NA,
    partial = TRUE
  )
  return(stk_data)
}

sampleSMABuySellStrategy <- function(stk_data, ticker_wap, index, last_order_index){
  if (stk_data[, "Orders"][last_order_index] == 1){
    if (as.numeric(stk_data$sma[index]) < as.numeric(stk_data$sma[index-1])){
      return (-1)
    }
    else {
      return (2)
    }
  }
  else {
    if (as.numeric(stk_data$sma[index]) > as.numeric(stk_data$sma[index-1])){
      return (1)
    }
    else {
      return (0)
    }
  }
}