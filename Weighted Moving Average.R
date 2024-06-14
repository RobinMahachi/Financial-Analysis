.wma_function <- function(pair, type) {
  
  easypackages::libraries('quantmod', 'TTR', 'PerformanceAnalytics', 'dygraphs')
  
  getSymbols(pair, src = 'yahoo', from = Sys.Date() - 120, auto.assign = TRUE)
  
  if(type == 'wma') {
    stock_data <- get(pair)  # Get the data from the environment
    colnames(stock_data) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
    
    wma_values <- TTR::WMA(stock_data$Close, n = 14)  # Calculate WMA
    print(head(wma_values))
    
    # Convert wma_values to xts for dygraph
    wma_xts <- xts(wma_values, order.by = index(stock_data))
    
    # Create the title
    plot_title <- paste("Weighted Moving Average for", pair)
    
    # Create dygraph plot
    dygraph(wma_xts, main = plot_title) %>%
      dyAxis("y", label = "WMA") %>%
      dyOptions(colors = "red")
  }
}

.wma_function('USDTRY=X', type = 'wma')
