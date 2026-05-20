# Load necessary packages
library(readr)
library(dplyr)
library(lubridate)

# Read the data
data <- read_csv("C:/Users/UTTATI/OneDrive/Desktop/Projects/DM project/VOLTAS.NS_data.csv")

# View column names and structure
str(data)
head(data)

# Step 1: Take log of each value
log_prices <- log(data$Close)

# Step 2: Compute log returns: X_k = log(P_k) - log(P_{k-1})
log_returns <- diff(log_prices)

# Step 3: Optionally add it to your dataset
data_log_returns <- data[-1, ]  # Drop first row since diff reduces length by 1
data_log_returns$log_return <- log_returns

# Step 4: View or export
head(data_log_returns)  # View first few rows
library(zoo)  # For rollmean
# Step 3: Calculate rolling mean of log returns with window size = 59
data_log_returns$rolling_mean_59 <- zoo::rollmean(data_log_returns$log_return, k = 59, align = "right", fill = NA)
head(data_log_returns)

# Assuming 'log_return' is the column in data_log_returns
# Calculate rolling variance over a window of 59
data_log_returns$rolling_var_59 <- rollapply(
  data_log_returns$log_return,
  width = 59,
  FUN = var,
  align = "right",
  fill = NA
)

# View result
head(data_log_returns, 10)
# Estimate annualized rolling volatility
data_log_returns$rolling_sigma_59 <- sqrt(252 * data_log_returns$rolling_var_59)
head(data_log_returns)
# Save the data_log_returns dataframe to CSV
write.csv(data_log_returns, "data_log_returns.csv", row.names = FALSE)


