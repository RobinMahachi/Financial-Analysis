

# Metrics -----------------------------------------------------------------


library(quantmod)
library(tidyverse)
library(skimr)
library(quantmod)
library(tidyverse)

# List of forex pairs
forex_pairs <- c('GBPAUD=X', 'GBPCAD=X', 'GBPJPY=X', 'GBPNZD=X', 'GBPUSD=X')

# Get data from Yahoo Finance
getSymbols(forex_pairs, src = 'yahoo')

# Extract adjusted close prices and merge into one data frame
forex_list <- lapply(forex_pairs, function(sym) {
  data <- get(sym)
  tibble(date = index(data), adjusted = Cl(data)) %>%
    rename(!!sym := adjusted)
})

# Reduce the list of data frames into a single data frame
forex_df <- reduce(forex_list, full_join, by = "date") %>%
  drop_na()

# Calculate log returns
log_returns <- forex_df %>%
  mutate(across(-date, ~ log(. / lag(.)), .names = "log_return_{col}")) %>%
  drop_na()
# Display the final data frame with log returns



# ETHEREUM ----------------------------------------------------------------


pkgs_load(type='financial')
pkgs_load(type='analysis')
pkgs_load(type='charts')

getSymbols('ETH-USD', src = 'yahoo', from = '2022-06-01', auto.assign = T)
eth <- `ETH-USD`[,6]
colnames(eth) <- c('ETH')
dygraph(eth)
eth_rets <- Return.calculate(eth, method = c('log'))
# Count the number of returns greater than 0%
positive_returns_count <- sum(eth_rets > 0, na.rm = TRUE)
# Print the result
cat("Number of returns greater than 0%:", positive_returns_count, "\n")


cryptos_load <- function(pair){
  if (pair == 'major') {
    crypto_tickers <- c('ADA-USD', 'BTC-USD', 'ETH-USD', 'USDT')
    getSymbols(crypto_tickers, src = 'yahoo', from = Sys.Date()-1000, auto.assign = TRUE)
  } else if (pair == 'maxvol') {
    vol_tickers <- c('BTC-USD', 'ETH-USD', 'USDT-USD', 'BNB-USD', 'SOL-USD', 'USDC-USD', 'XRP-USD', 'DOGE-USD')
    getSymbols(vol_tickers, src = 'yahoo', from = Sys.Date()-1000, auto.assign=T)
  } else if (type == 'charts') {
    easypackages::libraries('ggplot2', 'GGally', 'plot_ly')
  } else {
    warning('You had one job. Please specify "financial", "analysis", or "charts".')
    return(NULL)
  }
}

cryptos_load(pair = 'maxvol')

vol_tickers <- c('BTC-USD', 'ETH-USD', 'USDT-USD', 'BNB-USD', 'SOL-USD', 'USDC-USD', 'XRP-USD', 'DOGE-USD')
getSymbols(vol_tickers, src = 'yahoo', from = Sys.Date()-1000, auto.assign=T)
vol_cryptos <- cbind(`BTC-USD`, `ETH-USD`, `USDT-USD`, `BNB-USD`, `SOL-USD`, `USDC-USD`, `XRP-USD`, `DOGE-USD`)

df <- as_tibble(vol_cryptos)

.df <- as_tibble(rownames_to_column(as.data.frame(vol_cryptos), var = "Date"))

.vol <- .df |>
  select(Date,ends_with('Volume'))



rets <- Return.calculate(vol_cryptos, method=c('discrete'))
.rets <- as_tibble(rownames_to_column(as.data.frame(rets), var = "Date")) |> na.omit()
.rets <- .rets |> select(Date, contains('Adj'))

.rets |> select(Date, contains('Adj'))


# Function to remove the first period and everything after it
remove_after_period <- function(name) {
  sub("\\..*", "", name)
}

# Rename columns using the custom function
.rets <- .rets |> rename_with(remove_after_period, contains('Adj'))

.dfrets <- as.data.frame(.rets[,-1])

cor(.dfrets)
GGally::ggcorr(.dfrets)
summary(.dfrets)


# CVaR --------------------------------------------------------------------

# Assuming .dfrets is already defined
cvar_results <- apply(.dfrets, 2, CVaR)

# Convert the results into a data frame for easier handling if needed
cvar_results_df <- as.data.frame(cvar_results)

print(cvar_results_df)


# Drawdowns ---------------------------------------------------------------



library(PerformanceAnalytics)
library(purrr)
library(dplyr)

# Define the function to calculate AverageDrawdown for each column
calculate_AverageDrawdown <- function(data) {
  # Use map_dfc to apply AverageDrawdown to each column and return a data frame
  avg_drawdown_results <- map_dfc(data, AverageDrawdown)

  # Rename columns to indicate they contain AverageDrawdown results
  colnames(avg_drawdown_results) <- paste0("AvgDrawdown_", colnames(data))

  return(avg_drawdown_results)
}

# Example usage with .dfrets
# Assuming .dfrets is already defined
avg_drawdown_results <- calculate_AverageDrawdown(.dfrets)
print(avg_drawdown_results)


.rets |>
  select(Date,BTC) |>
  filter(BTC > 0.05)

library(dplyr)

.rets |>
  select(-Date) |>
  summarise(across(everything(), mean, na.rm = TRUE))

.rets |>
  select() |>
  CVaR()

library(describer)
describer::describe(.rets)

plot(TTR::SMA(.rets$BTC, n=10))
plot(TTR::EMA(.rets$BTC, n=10))




# bitcoin -----------------------------------------------------------------

# Core Tidyverse #
install.packages('glue')
install.packages('forcats')

library(tidyverse)
library(glue)
library(forcats)
# Time Series #
install.packages('timetk')
install.packages('tidyquant')
install.packages('tibbletime')
library(timetk)
library(tidyquant)
library(tibbletime)
# Visualization #
library(cowplot)
install.packages('cowplot')
# Preprocessing #
library(recipes)
install.packages('recipes')
# Sampling / Accuracy #
library(rsample)
install.packages('rsample')
library(yardstick)
install.packages('yardstick')
# Modeling #
library(keras)
