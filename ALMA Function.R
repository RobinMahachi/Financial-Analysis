


# ALMA Function -----------------------------------------------------------



.alma_function <- function(pair, type){
  
  easypackages::libraries('quantmod', 'TTR', 'PerformanceAnalytics', 'dygraphs')
  
  getSymbols(pair, src = 'yahoo', from = Sys.Date() - 120, auto.assign = TRUE)
  
  if(type == 'alma'){
    stock_data <- get(pair)  # Get the data from the environment
    colnames(stock_data) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
    
    alma_values <- TTR::ALMA(Cl(stock_data), n = 14, offset = 0.85, sigma = 6)  # Use the Close prices for ALMA calculation
    print(head(alma_values))
    
    # Convert alma_values to xts for dygraph
    alma_xts <- xts(alma_values, order.by = index(stock_data))
    
    # Create the title
    plot_title <- paste("Arnaud Legoux Moving Average for", pair)
    
    # Create dygraph plot
    dygraph(alma_xts, main = plot_title) %>%
      dyAxis("y", label = "ALMA") %>%
      dyOptions(colors = "green")
  }
}

.alma_function('USDTRY=X', type = 'alma')
                 
                 