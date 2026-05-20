library(dplyr)
library(lubridate)
library(ggplot2)
library(timeDate) # Needed for business day calculation
library(readr)
data <- read.csv("C:/Users/UTTATI/OneDrive/Desktop/Projects/DM project/Final data/HDFC PE (1920) FINAL.csv")
data$Date <- as.Date(data$Date)
data$Expiry <- as.Date(data$Expiry)
filtered_data <- data %>%
  filter(Date >= as.Date("2025-05-02") & Date <= as.Date("2025-07-01"))
#Call option
delta_call <- function(S0, K, r, sigma_hat, t) {
  omega <- (r * t + 0.5 * sigma_hat^2 * t - log(K / S0)) / (sigma_hat * sqrt(t))
  Delta <- pnorm(omega)
  return(Delta)
}
filtered_data$Delta <- mapply(delta_call,
                          S0 = data$Underlying.Value,
                          K = data$Strike.Price,
                          r = 0.06,
                          sigma_hat = data$rolling_sigma_59,
                          t = data$time_to_expiry_years)
#Put option
delta_put <- function(S0, K, r, sigma_hat, t) {
  omega <- (r * t + 0.5 * sigma_hat^2 * t - log(K / S0)) / (sigma_hat * sqrt(t))
  Delta <- pnorm(omega) -1
  return(Delta)
}
filtered_data$Delta <- mapply(delta_put,
                              S0 = data$Underlying.Value,
                              K = data$Strike.Price,
                              r = 0.06,
                              sigma_hat = data$rolling_sigma_59,
                              t = data$time_to_expiry_years)
head(filtered_data)
write.csv(filtered_data, "HDFC PE (1920) FINAL2.csv", row.names = FALSE)


#Plot
library(ggplot2)

data <- read.csv("C:/Users/UTTATI/OneDrive/Desktop/Projects/DM project/Final data/VOLTAS PE (1200) FINAL2.csv")
# Assuming `df` already has Delta_Calc and time_to_expiry_years
ggplot(data, aes(x = time_to_expiry_years, y = Delta)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", alpha = 0.5) +
  labs(
    title = "Delta vs Time to Expiration - VOLTAS PE (1200) ",
    x = "Time to Expiration (Years)",
    y = "Delta (Call Option)"
  ) +
  theme_minimal()
