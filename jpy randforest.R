install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("randomForest")
install.packages("dplyr")
install.packages("ggplot2")

library(quantmod)
library(PerformanceAnalytics)
library(randomForest)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load data
getSymbols('GBPJPY=X', src = 'yahoo', auto.assign = TRUE)
jpy <- `GBPJPY=X`
colnames(jpy) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')

# Calculate returns
jpy$Return <- periodReturn(jpy$Adjusted, period = 'daily', type = 'log')
jpy <- na.omit(round(jpy, 4))

# Ensure no missing values
if (anyNA(jpy$Return)) stop("NA values found in Return")

# Backtesting parameters
rolling_window <- 252  # One year of data for training
forecast_horizon <- 1  # Forecast one day ahead

# Initialize vectors to store results
actuals <- vector()
predictions <- vector()
dates <- vector()

# Convert to data frame for use with randomForest
jpy_df <- data.frame(Date = index(jpy), coredata(jpy))

# Rolling forecast
for (i in (rolling_window + 1):(nrow(jpy_df) - forecast_horizon)) {
  # Define training and test sets
  train_data <- jpy_df[(i - rolling_window):(i - 1), ]
  test_data <- jpy_df[i:(i + forecast_horizon - 1), ]
  
  # Train the Random Forest model
  rf_model <- randomForest(Return ~ Open + High + Low + Close + Volume + Adjusted, data = train_data)
  
  # Make the forecast
  forecast_values <- predict(rf_model, test_data)
  
  # Store the results
  predictions <- c(predictions, forecast_values)
  actuals <- c(actuals, test_data$Return)
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
  labs(title = "Backtest of Random Forest Model for GBP/JPY Returns",
       x = "Date", y = "Return") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()

# Evaluate performance metrics
mse <- mean((backtest_results$Actual - backtest_results$Predicted)^2)
mae <- mean(abs(backtest_results$Actual - backtest_results$Predicted))

print(paste("Mean Squared Error (MSE):", round(mse, 6)))
print(paste("Mean Absolute Error (MAE):", round(mae, 6)))
