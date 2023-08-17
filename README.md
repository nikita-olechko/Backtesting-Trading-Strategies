# Backtesting-Trading-Strategies-In-R

This project contains a backtesting system that allows you to simulate trading strategies on historical data. It's written in R and is highly customizable, allowing you to define and test your own trading strategies. For demonstration purposes, the system uses a testing strategy based on the 60-period Simple Moving Average (SMA).

See Full Disclaimer Below.

# Features

Allows for custom trading strategies
Accesses completely up-to-date historical data from Interactive Brokers (IBKR)
Modular design to accommodate various order types and quantities
Supports limit and stop-loss orders in addition to market orders (and any custom order)
Capable of running multiple backtests with different strategies simultaneously
Includes a sample testing strategy based on 60-period SMA

Note: An updated version of this system exists in Python in the complementary repository 'Live-Algorithmic-Trading-In-Python'. It has many improved features, functionality, and is being monitored and updated. If you intend to use this system for live trading, building neural network models, or performing heavy-duty analysis, I encourage you to look into the other repository as it may better suit your needs.

# Requirements

R version 3.5 or higher
R packages: quantstrat, PerformanceAnalytics, TTR
Interactive Brokers account
IBKR Trader Workstation installed, open, and running on port 4000 (this is not the default port)

# Setup and Usage

Clone the repository to your local machine.

Install the necessary R packages using the following command: install.packages(c("quantstrat", "PerformanceAnalytics", "TTR"))

Modify the backtesting instantiation in a new strategy file. See sample '60PeriodSMA.R' for reference.

To create custom order strategies, modify the customOrders.R file to contain your new order type, and have your strategy function return a reference to it. 

Modify the main.R file to reflect any changes in barsize, duration, strategy name, strategy function, and data generation function(s).

Open IBKR Trader Workstation and make sure it is running on port 4000.

Run the R script with the command: Rscript main.R.

To create strategy report parameters, modify the function create_summary_data() in utilities/dataGenerationUtilities/generalGenerationUtilities.R

You will find a new file in the data/strategy Results folder in the format strategy + barsize + duration. This file will contain the results of your test strategy on each ticker. 

# Customizing Strategies

You can modify the backtesting system by changing the strategy functions in the R script. These functions determine the buy/sell decisions and the data generation for each backtest.

The testing strategy includes the following functions:

generate60PeriodSMA(barDataFrame): Generates new data based on existing bar data, specifically calculating the 60-period SMA.

sampleSMABuySellStrategy(barDataFrame): Defines the strategy's decision-making process. It returns 1 (BUY), -1 (SELL), 2 (HOLD), or 0 (no action), based on whether the average price is above or below the 60-period SMA. Hold exists for the purpose of analyzing data.

You can create your own strategy by developing similar functions that align with your trading approach. Any data generated in the first function can be used in the second function.

# Running Multiple Strategies

If you want to run several strategies simultaneously, you can create new instances of the backtest system. It's recommended to run them in separate consoles if you want to see live updates.

# Data

The system searches for asset data in 2 ways: first, locally on your system, and then from IBKR. If it cannot find the appropriate data on your system, it will attempt to retrieve it from IBKR and write it to the system locally. This is done to speed up future strategy testing, as retrieving data from IBKR takes time, especially for large datasets.

The system keeps an internal list of erroredTickers, as IBKR does not always successfully return all tickers, and so the system will skip tickers previously errored. To try all tickers again, delete the file /data/ErroredTickers/ErroredTickers.csv

# Disclaimer

This backtesting system is a proof-of-concept and should not be used for live trading without careful review and enhancements. Trading involves substantial risk, and there is always the potential for loss. Your results may vary depending on various factors, such as your background, experience, and work ethic. All financial numbers referenced here, or on any of my sites and projects, are estimates or projections and should not be considered exact or a guarantee of potential earnings. Always trade responsibly and at your own risk. I am NOT a financial advisor.
