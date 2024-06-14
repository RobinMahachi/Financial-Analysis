


# RSI Function ------------------------------------------------------------



.rsi_function <- function(pair, type){
  
  easypackages::libraries('quantmod', 'TTR', 'PerformanceAnalytics', 'dygraphs')
  
  getSymbols(pair, src = 'yahoo', from = Sys.Date() - 120, auto.assign = TRUE)
  
  if(type == 'rsi'){
    stock_data <- get(pair)  # Get the data from the environment
    colnames(stock_data) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
    
    rsi_values <- TTR::RSI(Cl(stock_data), n = 14)  # Use the Close prices for RSI calculation
    print(head(rsi_values))
    
    # Convert rsi_values to xts for dygraph
    rsi_xts <- xts(rsi_values, order.by = index(stock_data))
    
    # Create the title
    plot_title <- paste("Relative Strength Index for", pair)
    
    # Create dygraph plot
    dygraph(rsi_xts, main = plot_title) %>%
      dyAxis("y", label = "RSI") %>%
      dyOptions(colors = "blue")
  }
}

.rsi_function('USDTRY=X', type = 'rsi')
