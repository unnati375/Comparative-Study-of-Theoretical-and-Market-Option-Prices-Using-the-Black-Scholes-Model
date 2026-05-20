library(dplyr)
library(lubridate)
library(timeDate) # Needed for business day calculation
library(readr)
data <- read_csv("C:/Users/UTTATI/OneDrive/Desktop/Projects/DM project/OPTSTK_VOLTAS_PE (1240).csv")

# Convert to Date format (adjust column names if needed)
data$Date <- as.Date(data$Date)
data$Expiry <- as.Date(data$Expiry)

# Filter data for target date range
filtered_data <- data %>%
  filter(Date >= as.Date("2025-05-02") & Date <= as.Date("2025-07-01"))


# Calculate business days between DATE and EXPIRY_DATE
filtered_data$working_days <- as.numeric(mapply(function(start, end) {
  sum(isBizday(timeSequence(from = start, to = end, by = "day")))
}, filtered_data$Date, filtered_data$Expiry))

# Convert working days to time to expiry in years
filtered_data$time_to_expiry_years <- filtered_data$working_days / 252

# View result
head(filtered_data)
#for call option
black_scholes_call <- function(S0, K, r, sigma_hat, t) {
  omega <- (r * t + 0.5 * sigma_hat^2 * t - log(K / S0)) / (sigma_hat * sqrt(t))
  C <- S0 * pnorm(omega) - K * exp(-r * t) * pnorm(omega - sigma_hat * sqrt(t))
  return(C)
}
filtered_data$C <- mapply(black_scholes_call,
                          S0 = filtered_data$`Underlying Value`,
                          K = filtered_data$`Strike Price`,
                          r = 0.06,
                          sigma_hat = filtered_data$rolling_sigma_59,
                          t = filtered_data$time_to_expiry_years)
filtered_data$diff <- filtered_data$`Settle Price`- filtered_data$C
# Black-Scholes Put Option Price
black_scholes_put <- function(S0, K, r, sigma_hat, t) {
  omega <- (r * t + 0.5 * sigma_hat^2 * t - log(K / S0)) / (sigma_hat * sqrt(t))
  P <- K * exp(-r * t) * pnorm(-omega + sigma_hat * sqrt(t)) - S0 * pnorm(-omega)
  return(P)
}
filtered_data$P <- mapply(black_scholes_put,
                          S0 = filtered_data$`Underlying Value`,
                          K = filtered_data$`Strike Price`,
                          r = 0.06,
                          sigma_hat = filtered_data$rolling_sigma_59,
                          t = filtered_data$time_to_expiry_years)
filtered_data$diff <- filtered_data$`Settle Price`- filtered_data$P

write.csv(filtered_data, "filtered_data_with_call_prices.csv", row.names = FALSE)
