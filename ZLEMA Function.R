


# ZLEMA Function ----------------------------------------------------------


.zlema_function <- function(pair, type){
  
  easypackages::libraries('quantmod', 'TTR', 'PerformanceAnalytics', 'dygraphs')
  
  getSymbols(pair, src = 'yahoo', from = Sys.Date() - 120, auto.assign = TRUE)
  
  if(type == 'zlema'){
    stock_data <- get(pair)  # Get the data from the environment
    colnames(stock_data) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
    
    zlema_values <- TTR::ZLEMA(Cl(stock_data), n = 14)  # Calculate ZLEMA using Close prices
    print(head(zlema_values))
    
    # Convert zlema_values to xts for dygraph
    zlema_xts <- xts(zlema_values, order.by = index(stock_data))
    
    # Create the title
    plot_title <- paste("Zero-Lag Exponential Moving Average for", pair)
    
    # Create dygraph plot
    dygraph(zlema_xts, main = plot_title) %>%
      dyAxis("y", label = "ZLEMA") %>%
      dyOptions(colors = "purple")
  }
}

.zlema_function('USDTRY=X', type = 'zlema')
