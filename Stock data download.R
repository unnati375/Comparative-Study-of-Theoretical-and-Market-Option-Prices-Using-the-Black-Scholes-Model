install.packages("quantmod")
library(quantmod)
symbol <- "INFY.NS"
start_date <- as.Date("2025-01-31")
end_date <- as.Date("2025-07-02")
stock_data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
stock_df <- data.frame(Date = index(stock_data), coredata(stock_data))
write.csv(stock_df, file = paste0(symbol, "_data.csv"), row.names = FALSE)
cat("Saved to:", paste0(symbol, "_data.csv"), "\n")