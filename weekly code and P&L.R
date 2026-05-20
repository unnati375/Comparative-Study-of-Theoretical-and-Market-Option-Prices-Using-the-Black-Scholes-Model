# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)

# Read the dataset
data <- read_csv("C:/Users/UTTATI/OneDrive/Desktop/Projects/DM project/Final data/VOLTAS PE (1240) FINAL2.csv")

# Convert Date column to Date format
data$Date <- as.Date(data$Date)

# Sort by date just in case
data <- data %>% arrange(Date)

# Create a week group based on 7-day blocks from the start date
start_date <- min(data$Date)
data <- data %>%
  mutate(month_group = as.integer(difftime(Date, start_date, units = "days")) %/% 7)

# Assign the last sigma value in each week_group to all rows in that group
data <- data %>%
  group_by(month_group) %>%
  mutate(monthly_sigma = last(rolling_sigma_59)) %>%
  ungroup()

# View result
head(data[, c("Date", "rolling_sigma_59", "monthly_sigma")], 40)
library(ggplot2)
ggplot(data, aes(x = Date, y = monthly_sigma)) +
  geom_line(color = "blue") +
  labs(title = "Sigma vs Date",
       x = "Date",
       y = "Sigma") +
  theme_minimal()


black_scholes_call <- function(S0, K, r, sigma_hat, t) {
  omega <- (r * t + 0.5 * sigma_hat^2 * t - log(K / S0)) / (sigma_hat * sqrt(t))
  C <- S0 * pnorm(omega) - K * exp(-r * t) * pnorm(omega - sigma_hat * sqrt(t))
  return(C)
}
data$C30 <- mapply(black_scholes_call,
                          S0 = data$Underlying.Value,
                          K = data$Strike.Price,
                          r = 0.06,
                          sigma_hat = data$monthly_sigma,
                          t = data$time_to_expiry_years)
data$diff30 <- data$Settle.Price- data$C30
head(data)


# Black-Scholes Put Option Price
black_scholes_put <- function(S0, K, r, sigma_hat, t) {
  omega <- (r * t + 0.5 * sigma_hat^2 * t - log(K / S0)) / (sigma_hat * sqrt(t))
  P <- K * exp(-r * t) * pnorm(-omega + sigma_hat * sqrt(t)) - S0 * pnorm(-omega)
  return(P)
}
data$P30 <- mapply(black_scholes_put,
                          S0 = data$Underlying.Value,
                          K = data$Strike.Price,
                          r = 0.06,
                          sigma_hat = data$monthly_sigma,
                          t = data$time_to_expiry_years)
data$diff30 <- data$Settle.Price- data$P30
head(data)

library(readr)
library(dplyr)
library(tidyr)


library(ggplot2)

# Plot diff vs. DATE
ggplot(data, aes(x = Date, y = diff30)) +
  geom_line(color = "steelblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "VOLTAS PE (1240)",
       x = "Date",
       y = "Price Difference (Settle Price - BS Price)") +
  theme_minimal()

#Call option
delta_call <- function(S0, K, r, sigma_hat, t) {
  omega <- (r * t + 0.5 * sigma_hat^2 * t - log(K / S0)) / (sigma_hat * sqrt(t))
  Delta <- pnorm(omega)
  return(Delta)
}
data$Delta30 <- mapply(delta_call,
                              S0 = data$Underlying.Value,
                              K = data$Strike.Price,
                              r = 0.06,
                              sigma_hat = data$monthly_sigma,
                              t = data$time_to_expiry_years)
#Put option
delta_put <- function(S0, K, r, sigma_hat, t) {
  omega <- (r * t + 0.5 * sigma_hat^2 * t - log(K / S0)) / (sigma_hat * sqrt(t))
  Delta <- pnorm(omega) -1
  return(Delta)
}
data$Delta30 <- mapply(delta_put,
                              S0 = data$Underlying.Value,
                              K = data$Strike.Price,
                              r = 0.06,
                              sigma_hat = data$monthly_sigma,
                              t = data$time_to_expiry_years)
head(data)

# Initialize hedge portfolio
data$Stock_Position <- NA
data$Cash_Position <- NA
data$Portfolio_Value <- NA
data$PnL <- NA

# Assume initial option position is short 1 contract
# Initial hedge: Buy delta units of stock
data$Stock_Position[1] <- data$Delta30[1]
data$Cash_Position[1] <- -data$Stock_Position[1] * data$Underlying.Value[1]
data$Portfolio_Value[1] <- data$Stock_Position[1] * data$Underlying.Value[1] + data$Cash_Position[1]
data$PnL[1] <- 0  # No P&L on day 1

# Loop through and simulate delta hedging
for (i in 2:nrow(data)) {
  # Rebalance stock position to match new delta
  delta_change <- data$Delta30[i] - data$Stock_Position[i-1]
  
  # Buy/sell delta_change units of stock
  data$Stock_Position[i] <- data$Delta30[i]
  data$Cash_Position[i] <- data$Cash_Position[i-1] - delta_change * data$Underlying.Value[i]
  
  # Calculate portfolio value and P&L
  data$Portfolio_Value[i] <- data$Stock_Position[i] * data$Underlying.Value[i] + data$Cash_Position[i]
  data$PnL[i] <- data$Portfolio_Value[i] - data$Portfolio_Value[1]
}

# Plot P&L over time
ggplot(data, aes(x = Date, y = -PnL)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Delta-Neutral Hedging P&L Over Time VOLTAS PE (1240)",
       x = "Date", y = "P&L (in currency units)") +
  theme_minimal()

library(ggplot2)
# Step 4: Reshape for plotting
data <- data %>%
  select(Date, P30, Settle.Price) %>%
  pivot_longer(cols = c(P30, Settle.Price), names_to = "Type", values_to = "Price")
# Step 5: Plot
ggplot(data, aes(x = Date, y = Price, color = Type)) +
  geom_line(size = 1) +
  labs(title = "Theoretical vs Market Put Option Prices HDFC (1920)",
       x = "Date", y = "Price",
       color = "Price Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "top"
  )
# View final dataframe
head(data)