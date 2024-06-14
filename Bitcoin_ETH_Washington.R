
# Crypto Analysis ------------------------------------------------------------

# Load necessary libraries
library(quantmod)
library(dplyr)
library(TTR)
library(randomForest)

# Retrieve Bitcoin prices in USD
getSymbols("BTC-USD", src = "yahoo", from = "2020-01-01", to = Sys.Date())

# Combine the data into a single tibble
prices <- `BTC-USD`[, "BTC-USD.Close"]
prices <- as_tibble(prices, .name_repair = "minimal")
colnames(prices) <- "BTC_Close"


prices <- prices %>%
  mutate(Date = index(prices))

# Calculate returns
prices <- prices %>%
  mutate(BTC_R = (BTC_Close - lag(BTC_Close)) / lag(BTC_Close)) %>%
  na.omit()

# Calculate additional indicators
dtable <- cbind(
  prices$BTC_Close,
  prices$BTC_R,
  TTR::ALMA(prices$BTC_R, n = 9, offset = 0.85, sigma = 6),
  TTR::CMO(prices$BTC_R, n = 14),
  TTR::CTI(prices$BTC_R, n = 14),
  TTR::DEMA(prices$BTC_R, n = 10, v = 1, wilder = FALSE),
  TTR::DPO(prices$BTC_R, n = 10, maType = "EMA"),
  TTR::ZLEMA(prices$BTC_R, n = 20)
)
colnames(dtable) <- c('BTC_Close', 'RTNS', 'ALMA', 'CMO', 'CTI', 'DEMA', 'DPO', 'ZLEMA')
dtable <- as_tibble(dtable)
dtable <- na.omit(round(dtable, 4))

# Split the data into training and test sets
set.seed(123)
train_size <- floor(0.8 * nrow(dtable))
train_indices <- sample(seq_len(nrow(dtable)), size = train_size)

train_data <- dtable[train_indices, ]
test_data <- dtable[-train_indices, ]

# Train the Random Forest model

mod1 <- randomForest(RTNS ~ ALMA + CMO + CTI + DEMA + DPO + ZLEMA, data = train_data)

# Predict future returns using the model
predicted_returns <- predict(mod1, test_data)

# Add predictions to the test_data tibble
test_data <- test_data %>%
  mutate(Predicted_Return = predicted_returns)

# Plot the actual vs predicted returns using base R plot functions
plot(test_data$RTNS, type = 'l', col = 'blue', ylab = 'Return', xlab = 'Index',
     main = 'Actual vs Predicted Returns of Bitcoin')
lines(test_data$Predicted_Return, col = 'red')
legend("topright", legend = c("Actual Return", "Predicted Return"),
       col = c("blue", "red"), lty = 1)

# Function to calculate cumulative returns
calculate_cumulative_returns <- function(returns) {
  cum_returns <- cumsum(returns)
  return(cum_returns)
}

# Calculate cumulative returns for actual and predicted
test_data <- test_data %>%
  mutate(Cumulative_Return = calculate_cumulative_returns(RTNS),
         Cumulative_Predicted_Return = calculate_cumulative_returns(Predicted_Return))

# Plot the cumulative actual vs predicted returns using base R plot functions
plot(test_data$Cumulative_Return, type = 'l', col = 'blue', ylab = 'Cumulative Return', xlab = 'Index',
     main = 'Cumulative Actual vs Predicted Returns of Bitcoin')
lines(test_data$Cumulative_Predicted_Return, col = 'red')
legend("topright", legend = c("Actual Cumulative Return", "Predicted Cumulative Return"),
       col = c("blue", "red"), lty = 1)



# Crypto - ETH ------------------------------------------------------------

# Load necessary libraries
library(quantmod)
library(dplyr)
library(TTR)
library(randomForest)

# Retrieve Ethereum prices in USD
getSymbols("ETH-USD", src = "yahoo", from = "2020-01-01", to = Sys.Date())

# Combine the data into a single tibble
eth_prices <- `ETH-USD`[, "ETH-USD.Close"]
eth_prices <- as_tibble(eth_prices, .name_repair = "minimal")
colnames(eth_prices) <- "ETH_Close"
eth_prices <- eth_prices %>%
  mutate(Date = index(eth_prices))

# Calculate returns
eth_prices <- eth_prices %>%
  mutate(ETH_R = (ETH_Close - lag(ETH_Close)) / lag(ETH_Close)) %>%
  na.omit()

# Calculate additional indicators
eth_dtable <- cbind(
  eth_prices$ETH_Close,
  eth_prices$ETH_R,
  TTR::ALMA(eth_prices$ETH_R, n = 9, offset = 0.85, sigma = 6),
  TTR::CMO(eth_prices$ETH_R, n = 14),
  TTR::CTI(eth_prices$ETH_R, n = 14),
  TTR::DEMA(eth_prices$ETH_R, n = 10, v = 1, wilder = FALSE),
  TTR::DPO(eth_prices$ETH_R, n = 10, maType = "EMA"),
  TTR::ZLEMA(eth_prices$ETH_R, n = 20)
)
colnames(eth_dtable) <- c('ETH_Close', 'RTNS', 'ALMA', 'CMO', 'CTI', 'DEMA', 'DPO', 'ZLEMA')
eth_dtable <- as_tibble(eth_dtable)
eth_dtable <- na.omit(round(eth_dtable, 4))

# Split the data into training and test sets
set.seed(123)
train_size <- floor(0.8 * nrow(eth_dtable))
train_indices <- sample(seq_len(nrow(eth_dtable)), size = train_size)

train_data <- eth_dtable[train_indices, ]
test_data <- eth_dtable[-train_indices, ]

# Train the Random Forest model
mod1 <- randomForest(RTNS ~ ALMA + CMO + CTI + DEMA + DPO + ZLEMA, data = train_data)

# Predict future returns using the model
predicted_returns <- predict(mod1, test_data)

# Add predictions to the test_data tibble
test_data <- test_data %>%
  mutate(Predicted_Return = predicted_returns)

# Plot the actual vs predicted returns using base R plot functions
plot(test_data$RTNS, type = 'l', col = 'blue', ylab = 'Return', xlab = 'Index',
     main = 'Actual vs Predicted Returns of Ethereum')
lines(test_data$Predicted_Return, col = 'red')
legend("topright", legend = c("Actual Return", "Predicted Return"),
       col = c("blue", "red"), lty = 1)

# Function to calculate cumulative returns
calculate_cumulative_returns <- function(returns) {
  cum_returns <- cumsum(returns)
  return(cum_returns)
}

# Calculate cumulative returns for actual and predicted
test_data <- test_data %>%
  mutate(Cumulative_Return = calculate_cumulative_returns(RTNS),
         Cumulative_Predicted_Return = calculate_cumulative_returns(Predicted_Return))

# Plot the cumulative actual vs predicted returns using base R plot functions
plot(test_data$Cumulative_Return, type = 'l', col = 'blue', ylab = 'Cumulative Return', xlab = 'Index',
     main = 'Cumulative Actual vs Predicted Returns of Ethereum')
lines(test_data$Cumulative_Predicted_Return, col = 'red')
legend("topright", legend = c("Actual Cumulative Return", "Predicted Cumulative Return"),
       col = c("blue", "red"), lty = 1)

#Predict future returns using the model
#Predict future returns using the model
#Predict future returns using the model
#Predict future returns using the model
#Predict future returns using the model


# Forecasting future returns
# Extend the data to include future dates
future_dates <- seq(from = as.Date(max(eth_prices$Date)) + 1, by = "days", length.out = 30)
future_prices <- rep(NA, length(future_dates))
extended_prices <- tibble(Date = c(eth_prices$Date, future_dates),
                          ETH_Close = c(eth_prices$ETH_Close, future_prices))

# Recalculate technical indicators for the extended period
extended_prices <- extended_prices %>%
  mutate(ETH_R = (ETH_Close - lag(ETH_Close)) / lag(ETH_Close))

extended_dtable <- cbind(
  extended_prices$ETH_Close,
  extended_prices$ETH_R,
  TTR::ALMA(extended_prices$ETH_R, n = 9, offset = 0.85, sigma = 6),
  TTR::CMO(extended_prices$ETH_R, n = 14),
  TTR::CTI(extended_prices$ETH_R, n = 14),
  TTR::DEMA(extended_prices$ETH_R, n = 10, v = 1, wilder = FALSE),
  TTR::DPO(extended_prices$ETH_R, n = 10, maType = "EMA"),
  TTR::ZLEMA(extended_prices$ETH_R, n = 20)
)
colnames(extended_dtable) <- c('ETH_Close', 'RTNS', 'ALMA', 'CMO', 'CTI', 'DEMA', 'DPO', 'ZLEMA')
extended_dtable <- as_tibble(extended_dtable)
extended_dtable <- na.omit(round(extended_dtable, 4))

# Predict future returns using the model
future_predictions <- predict(mod1, extended_dtable[is.na(extended_dtable$RTNS), ])

# Add future predictions to the extended data
extended_dtable <- extended_dtable %>%
  mutate(Predicted_Return = ifelse(is.na(RTNS), future_predictions, NA))

# Plot the forecasted returns using base R plot functions
plot(extended_dtable$Predicted_Return, type = 'l', col = 'green', ylab = 'Predicted Return', xlab = 'Index',
     main = 'Forecasted Returns of Ethereum')
legend("topright", legend = c("Predicted Return"), col = c("green"), lty = 1)



# Load necessary libraries
library(quantmod)
library(dplyr)
library(TTR)
library(randomForest)

# Retrieve Ethereum prices in USD
getSymbols("ETH-USD", src = "yahoo", from = "2020-01-01", to = Sys.Date())

# Combine the data into a single tibble
eth_prices <- `ETH-USD`[, "ETH-USD.Close"]
eth_prices <- as_tibble(eth_prices, .name_repair = "minimal")
colnames(eth_prices) <- "ETH_Close"
eth_prices <- eth_prices %>%
  mutate(Date = index(eth_prices))

# Calculate returns
eth_prices <- eth_prices %>%
  mutate(ETH_R = (ETH_Close - lag(ETH_Close)) / lag(ETH_Close)) %>%
  na.omit()

# Calculate additional indicators
eth_dtable <- cbind(
  eth_prices$ETH_Close,
  eth_prices$ETH_R,
  TTR::ALMA(eth_prices$ETH_R, n = 9, offset = 0.85, sigma = 6),
  TTR::CMO(eth_prices$ETH_R, n = 14),
  TTR::CTI(eth_prices$ETH_R, n = 14),
  TTR::DEMA(eth_prices$ETH_R, n = 10, v = 1, wilder = FALSE),
  TTR::DPO(eth_prices$ETH_R, n = 10, maType = "EMA"),
  TTR::ZLEMA(eth_prices$ETH_R, n = 20)
)
colnames(eth_dtable) <- c('ETH_Close', 'RTNS', 'ALMA', 'CMO', 'CTI', 'DEMA', 'DPO', 'ZLEMA')
eth_dtable <- as_tibble(eth_dtable)
eth_dtable <- na.omit(round(eth_dtable, 4))

# Split the data into training and test sets
set.seed(123)
train_size <- floor(0.8 * nrow(eth_dtable))
train_indices <- sample(seq_len(nrow(eth_dtable)), size = train_size)

train_data <- eth_dtable[train_indices, ]
test_data <- eth_dtable[-train_indices, ]

# Train the Random Forest model
mod1 <- randomForest(RTNS ~ ALMA + CMO + CTI + DEMA + DPO + ZLEMA, data = train_data)

# Predict future returns using the model
predicted_returns <- predict(mod1, test_data)

# Add predictions to the test_data tibble
test_data <- test_data %>%
  mutate(Predicted_Return = predicted_returns)

# Plot the actual vs predicted returns using base R plot functions
plot(test_data$RTNS, type = 'l', col = 'blue', ylab = 'Return', xlab = 'Index',
     main = 'Actual vs Predicted Returns of Ethereum')
lines(test_data$Predicted_Return, col = 'red')
legend("topright", legend = c("Actual Return", "Predicted Return"),
       col = c("blue", "red"), lty = 1)

# Function to calculate cumulative returns
calculate_cumulative_returns <- function(returns) {
  cum_returns <- cumsum(returns)
  return(cum_returns)
}

# Calculate cumulative returns for actual and predicted
test_data <- test_data %>%
  mutate(Cumulative_Return = calculate_cumulative_returns(RTNS),
         Cumulative_Predicted_Return = calculate_cumulative_returns(Predicted_Return))

# Plot the cumulative actual vs predicted returns using base R plot functions
plot(test_data$Cumulative_Return, type = 'l', col = 'blue', ylab = 'Cumulative Return', xlab = 'Index',
     main = 'Cumulative Actual vs Predicted Returns of Ethereum')
lines(test_data$Cumulative_Predicted_Return, col = 'red')
legend("topright", legend = c("Actual Cumulative Return", "Predicted Cumulative Return"),
       col = c("blue", "red"), lty = 1)

# Forecasting future returns
# Extend the data to include future dates (but without recalculating indicators on NAs)
future_dates <- seq(from = as.Date(max(eth_prices$Date)) + 1, by = "days", length.out = 30)
future_prices <- rep(NA, length(future_dates))
extended_prices <- tibble(Date = c(eth_prices$Date, future_dates),
                          ETH_Close = c(eth_prices$ETH_Close, future_prices))

# Use the last known data to compute future returns and indicators
last_known_data <- tail(eth_dtable, 1)
for (i in 1:length(future_dates)) {
  last_known_data <- last_known_data %>%
    mutate(
      ETH_Close = NA,
      RTNS = predict(mod1, last_known_data)
    )
  extended_prices$ETH_Close[nrow(eth_prices) + i] <- last_known_data$ETH_Close
}

# Calculate technical indicators for the extended period (use actual data for known values)
extended_prices <- extended_prices %>%
  mutate(
    ETH_R = (ETH_Close - lag(ETH_Close)) / lag(ETH_Close),
    ALMA = TTR::ALMA(ETH_R, n = 9, offset = 0.85, sigma = 6),
    CMO = TTR::CMO(ETH_R, n = 14),
    CTI = TTR::CTI(ETH_R, n = 14),
    DEMA = TTR::DEMA(ETH_R, n = 10, v = 1, wilder = FALSE),
    DPO = TTR::DPO(ETH_R, n = 10, maType = "EMA"),
    ZLEMA = TTR::ZLEMA(ETH_R, n = 20)
  ) %>%
  na.omit()

# Predict future returns using the model
future_predictions <- predict(mod1, extended_prices)

# Add future predictions to the extended data
extended_prices <- extended_prices %>%
  mutate(Predicted_Return = future_predictions)

# Plot the forecasted returns using base R plot functions
plot(extended_prices$Date, extended_prices$Predicted_Return, type = 'l', col = 'green', ylab = 'Predicted Return', xlab = 'Date',
     main = 'Forecasted Returns of Ethereum')
legend("topright", legend = c("Predicted Return"), col = c("green"), lty = 1)
