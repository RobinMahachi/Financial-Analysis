library(quantmod)
library(PerformanceAnalytics)
library(e1071)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load data
getSymbols('GBPJPY=X', src = 'yahoo', auto.assign = TRUE)
jpy <- `GBPJPY=X`
colnames(jpy) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')

# Ensure no missing values
jpy <- na.omit(round(jpy, 4))
if (anyNA(jpy)) stop("NA values found in data")

# Backtesting parameters
rolling_window <- 252  # One year of data for training
forecast_horizon <- 1  # Forecast one day ahead

# Initialize vectors to store results
actuals <- vector()
predictions <- vector()
dates <- vector()

# Convert to data frame for use with SVR
jpy_df <- data.frame(Date = index(jpy), coredata(jpy))

# Rolling forecast
for (i in (rolling_window + 1):(nrow(jpy_df) - forecast_horizon)) {
  # Define training and test sets
  train_data <- jpy_df[(i - rolling_window):(i - 1), ]
  test_data <- jpy_df[i:(i + forecast_horizon - 1), ]
  
  # Train the SVR model
  svr_model <- svm(Adjusted ~ Open + High + Low + Close + Volume, data = train_data)
  
  # Make the forecast
  forecast_values <- predict(svr_model, test_data)
  
  # Store the results
  predictions <- c(predictions, forecast_values)
  actuals <- c(actuals, test_data$Adjusted)
  dates <- c(dates, test_data$Date)
}

# Create a data frame for plotting and analysis
backtest_results <- data.frame(
  Date = as.Date(dates),
  Actual = actuals,
  Predicted = predictions
)

# Plot the backtest results
ggplot(backtest_results, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Backtest of SVR Model for GBP/JPY Adjusted Closing Prices",
       x = "Date", y = "Adjusted Closing Price") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()

# Evaluate performance metrics
mse <- mean((backtest_results$Actual - backtest_results$Predicted)^2)
mae <- mean(abs(backtest_results$Actual - backtest_results$Predicted))

print(paste("Mean Squared Error (MSE):", round(mse, 6)))
print(paste("Mean Absolute Error (MAE):", round(mae, 6)))
