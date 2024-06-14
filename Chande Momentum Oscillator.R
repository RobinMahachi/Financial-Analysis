

# Chande Momentum Oscillator ----------------------------------------------

################# ~~~~~~~~~~~~~~~~~~~~~ basic necessities
?TTR::CMO()  # base package for analysis
pkgs_load(type = 'financial')   # MAHACHI function to load packages needed

################# ~~~~~~~~~~~~~~~~~~~~~ gather cmo data and NA treatment

cmo <- TTR::CMO(gpy[,1], n = 14)
.cmo <- as_tibble(cmo) |> na.omit()


# CMO Function ------------------------------------------------------------


.cmo_function <- function(pair, type){
  
  easypackages::libraries('quantmod', 'TTR', 'PerformanceAnalytics', 'dygraphs')
  
  getSymbols(pair, src = 'yahoo', from = Sys.Date() - 120, auto.assign = TRUE)
  
  if(type == 'cmo'){
    stock_data <- get(pair)  # Get the data from the environment
    colnames(stock_data) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
    
    x <- TTR::CMO(Cl(stock_data), n = 14)  # Use the Close prices for CMO calculation
    print((x))
    
    # Convert x to xts for dygraph
    x_xts <- xts(x, order.by = index(stock_data))
    
    # Create the title
    plot_title <- paste("Chande Momentum Oscillator for", pair)
    
    # Create dygraph plot
    dygraph(x_xts, main = plot_title) %>%
      dyAxis("y", label = "CMO") %>%
      dyOptions(colors = "magenta")
  }
}

.cmo_function('USDTRY=X', type = 'cmo')
