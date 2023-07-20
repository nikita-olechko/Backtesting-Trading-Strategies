# Create custom order actions here. Make sure to add the order to the 
# order_selector and make sure your strategy can return that order selector

order_selector <- function(order) {
  order <- as.character(order)
  order_list <- list("1" = buy_order, "-1" = sell_order, "2" = hold_order, "0" = nothing_order)
  return(order_list[[as.character(order)]])
}


buy_order <- function(stk_data, index, last_order_index, ticker_wap){
  stk_data$Orders[index] <- 1
  last_order_index <- index
  stk_data$Position[index] <- as.numeric(stk_data$Position[index - 1]) - as.numeric(stk_data[, ticker_wap][index])
  return (list(stk_data, last_order_index))
}

sell_order <- function(stk_data, index, last_order_index, ticker_wap){
  stk_data$Orders[index] <- -1
  last_order_index <- index
  stk_data$Position[index] <- as.numeric(stk_data$Position[index - 1]) + as.numeric(stk_data[, ticker_wap][index])
  return (list(stk_data, last_order_index))
}

hold_order <- function(stk_data, index, last_order_index, ...){
  stk_data$Orders[index] <- 2
  stk_data$Position[index] <- stk_data$Position[index-1]
  return (list(stk_data, last_order_index))
}

nothing_order <- function(stk_data, index, last_order_index, ...){
  stk_data$Position[index] <- stk_data$Position[index-1]
  return (list(stk_data, last_order_index))
}