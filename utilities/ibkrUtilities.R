# Collection of General Utility Functions Associated with IBKR
library("IBrokers")
library("tidyverse")
library("ichimoku")
library("zoo")
library("xts")
library("quantmod")
source('utilities/dataGenerationUtilities/bollingerBands.R')


#' Function to retrieve historical stock data using the Interactive Brokers TWS API.
get_Stock_Data <- function(tws, security, barsize = "1 day", duration = "1 Y",
                         whatToShow = 'TRADES') {
  #
  print("entered getStockData")
  ticker <- toString(security["symbol"])
  fileName <- create_Historical_Data_File_Name(ticker, barsize, duration)
  folderPath <- paste0(get_Working_Directory_As_Path(), "\\data\\Historical Data\\")
  # If the data already exists, retrieve it
  if (file_Exists_In_Folder(fileName, folderPath = folderPath)){
    filePathName <- paste0(folderPath, fileName)
    tryCatch({
    stk_data <- read.zoo(filePathName, header = TRUE)
    stk_data <- as.xts(stk_data)
    }, error = function(e) {
      cat("A read.zoo error occurred:", conditionMessage(e), "\n")
      stk_data <- NULL
    })
    
  }
  else {
    stk_data <- NULL
    tryCatch({
      stk_data <- reqHistoricalData(
        tws,
        security,
        barSize = barsize,
        duration = duration,
        whatToShow = whatToShow)
      # if new data acquired, write it to the historical data folder
      stk_data$index <- seq_len(nrow(stk_data))
      if (length(stk_data$index) <= 50){
        return (NULL)
      }
      stk_data <- add_Analysis_Data_To_Historical_Data(stk_data, ticker)
      write_Zoo_Data_To_Folder(stk_data, fileName, folderPath)
      write_xts_Data_To_Folder(stk_data, fileName, folderPath)
      print("Historical Data Created")
    }, error = function(e) {
      cat("An error occurred:", conditionMessage(e), "\n")
    })
    if (is.null(stk_data)){
      return (NULL)
    }
  }
  return(stk_data)
}


add_Analysis_Data_To_Historical_Data <- function(stk_data, ticker){
  tickerWAP <- paste0(ticker, ".WAP")
  stk_data$Orders <- 0
  stk_data$Position <- 0
  stk_data$holdingGrossReturn <- stk_data[, tickerWAP]/as.numeric(stk_data[, tickerWAP][1])
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
  stk_data <- add_SD_To_Data(stk_data, tickerWAP)
  return(stk_data)
}


#' A function that creates a customized data file name
createFileName <- function(ticker, barsize, duration, strategy, mac_len_short = 3, mac_len_long = 10){
  fileTicker <- str_replace_all(ticker, " ", "")
  fileBarSize <- str_replace_all(barsize, " ", "")
  fileDuration <- str_replace_all(duration, " ", "")
  fileStrategy <- str_replace_all(strategy, " ", "")
  fileMACLengths <- ""
  if (strategy == "MAC"){
    fileMACLengths <- paste0(mac_len_short, ".", mac_len_long)
  }
  filename <- paste0(strategy, fileMACLengths, fileTicker, fileBarSize, fileDuration)
  return(filename)
}

#' A function that creates a file name for storing historical data 
#' ignore date of file creation
create_Historical_Data_File_Name <- function(ticker, barsize, duration){
  fileTicker <- str_replace_all(ticker, " ", "")
  fileBarSize <- str_replace_all(barsize, " ", "")
  fileDuration <- str_replace_all(duration, " ", "")
  filename <- paste0("Historical", fileTicker, fileBarSize, fileDuration)
  return(filename)
}

#' A function that creates a customized data folder name
create_Folder_Name <- function(barsize, duration, strategy, mac_len_short = 3, mac_len_long = 10){
  fileBarSize <- str_replace_all(barsize, " ", "")
  fileDuration <- str_replace_all(duration, " ", "")
  fileMACLengths <- ""
  if (strategy == "MAC"){
    fileMACLengths <- paste0(mac_len_short, ".", mac_len_long)
  }
  folderName <- paste0(strategy, fileMACLengths, fileBarSize, fileDuration)
}


#' A function that check if a file exists within a specified path
file_Exists_In_Folder <- function(filename, folderPath = paste0(get_Working_Directory_As_Path(),"\\data\\")){
  file_path <- file.path(folderPath, filename)
  if (file.exists(file_path)) {
    print(paste(filename, "already exists"))
    return (TRUE)
  } else {
    return (FALSE)
  }
}

#' A function that adds errored tickers to a log file
add_Errored_Tickers_To_Log <- function(ticker) {
  logFilePath <- file.path(get_Working_Directory_As_Path(),"data","ErroredTickers")
  
  if (!dir.exists(logFilePath)) {
    dir.create(logFilePath, recursive = TRUE)
  }
  
  csvFilePathName <- file.path(logFilePath, "ErroredTickers.csv")
  
  # if the file doesn't exist, create it and add the ticker
  if (!file.exists(csvFilePathName)) {
    write.table(ticker, csvFilePathName, sep = ",", col.names = FALSE, row.names = FALSE, append = FALSE)
  } else {
    # if the file exists, read it
    existing_tickers <- read.csv(csvFilePathName, header = FALSE, stringsAsFactors = FALSE)
    
    # check if ticker is already in the file
    if (!(ticker %in% existing_tickers$V1)) {
      # if it's not in the file, append it
      write.table(ticker, csvFilePathName, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
    }
  }
}

#' A function that checks whether a data subfolder exists for a given strategy, and creates it if it doesn't
create_Data_Results_Directory <- function(barsize, duration, strategy = "MAC", mac_len_short=3, mac_len_long = 10){
  folderName <- create_Folder_Name(barsize, duration, strategy = strategy, mac_len_short= mac_len_short, mac_len_long = mac_len_long)
  logFilePath <- file.path(paste0(get_Working_Directory_As_Path(),"\\data\\", folderName))

  #If the folder doesn't exist, create it
  if (!dir.exists(logFilePath)) {
    dir.create(logFilePath, recursive = TRUE)
  } 
  return (folderName)
}


#' A function to write Zoo Data to a folder
#' @param folderPath A path from root to the specified folder. Must end in "\\", 
#' use get_Working_Directory_As_Path to get this path and add on your destination folders 
write_Zoo_Data_To_Folder <- function(zooData, fileName, folderPath){
  if (!is.zoo(zooData)){
    zooData <- as.zoo(zooData)
  }
  filePathName <- paste0(folderPath, fileName)
  write.zoo(zooData, filePathName, row.names = FALSE)
}


write_xts_Data_To_Folder <- function(data, fileName, folderPath){
  if (!is.xts(data)){
    data <- as.xts(data)
  }
  filePathName <- paste0(folderPath, fileName, ".csv")
  write.csv(data, filePathName, row.names = FALSE)
}

#' A function to get your working directory as a string to make things run on any system
get_Working_Directory_As_Path <- function(){
  wd <- toString(getwd())
  wd <- gsub('/', '\\\\', wd)
  return (wd)
}
