# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)

# Read the dataset
data <- read_csv("C:/Users/UTTATI/OneDrive/Desktop/Projects/DM project/Final data/VOLTAS CE (1200) FINAL2.csv")

# Convert Date column to Date format
data$Date <- as.Date(data$Date)

# Sort by date just in case
data <- data %>% arrange(Date)

# Create a week group based on 7-day blocks from the start date
start_date <- min(data$Date)
data <- data %>%
  mutate(weekly_group = as.integer(difftime(Date, start_date, units = "days")) %/% 7)

# Assign the last sigma value in each week_group to all rows in that group
data <- data %>%
  group_by(weekly_group) %>%
  mutate(weekly_sigma = last(rolling_sigma_59)) %>%
  ungroup()
head(data[, c("Date", "rolling_sigma_59", "weekly_sigma")], 40)

black_scholes_call <- function(S0, K, r, sigma_hat, t) {
  omega <- (r * t + 0.5 * sigma_hat^2 * t - log(K / S0)) / (sigma_hat * sqrt(t))
  C <- S0 * pnorm(omega) - K * exp(-r * t) * pnorm(omega - sigma_hat * sqrt(t))
  return(C)
}
data$C7 <- mapply(black_scholes_call,
                   S0 = data$Underlying.Value,
                   K = data$Strike.Price,
                   r = 0.06,
                   sigma_hat = data$weekly_sigma,
                   t = data$time_to_expiry_years)
data$diff7 <- data$Settle.Price- data$C7

# Black-Scholes Put Option Price
black_scholes_put <- function(S0, K, r, sigma_hat, t) {
  omega <- (r * t + 0.5 * sigma_hat^2 * t - log(K / S0)) / (sigma_hat * sqrt(t))
  P <- K * exp(-r * t) * pnorm(-omega + sigma_hat * sqrt(t)) - S0 * pnorm(-omega)
  return(P)
}
data$P7 <- mapply(black_scholes_put,
                   S0 = data$Underlying.Value,
                   K = data$Strike.Price,
                   r = 0.06,
                   sigma_hat = data$weekly_sigma,
                   t = data$time_to_expiry_years)
data$diff7 <- data$Settle.Price- data$P7

#Call option
delta_call <- function(S0, K, r, sigma_hat, t) {
  omega <- (r * t + 0.5 * sigma_hat^2 * t - log(K / S0)) / (sigma_hat * sqrt(t))
  Delta <- pnorm(omega)
  return(Delta)
}
data$Delta7 <- mapply(delta_call,
                       S0 = data$Underlying.Value,
                       K = data$Strike.Price,
                       r = 0.06,
                       sigma_hat = data$weekly_sigma,
                       t = data$time_to_expiry_years)
#Put option
delta_put <- function(S0, K, r, sigma_hat, t) {
  omega <- (r * t + 0.5 * sigma_hat^2 * t - log(K / S0)) / (sigma_hat * sqrt(t))
  Delta <- pnorm(omega) -1
  return(Delta)
}
data$Delta7 <- mapply(delta_put,
                       S0 = data$Underlying.Value,
                       K = data$Strike.Price,
                       r = 0.06,
                       sigma_hat = data$weekly_sigma,
                       t = data$time_to_expiry_years)

data$Stock_Position <- NA
data$Cash_Position <- NA
data$Portfolio_Value <- NA
data$PnL <- NA

# Assume initial option position is short 1 contract
# Initial hedge: Buy delta units of stock
data$Stock_Position[1] <- data$Delta7[1]
data$Cash_Position[1] <- -data$Stock_Position[1] * data$Underlying.Value[1]
data$Portfolio_Value[1] <- data$Stock_Position[1] * data$Underlying.Value[1] + data$Cash_Position[1]
data$PnL[1] <- 0  # No P&L on day 1

# Loop through and simulate delta hedging
for (i in 2:nrow(data)) {
  # Rebalance stock position to match new delta
  delta_change <- data$Delta7[i] - data$Stock_Position[i-1]
  
  # Buy/sell delta_change units of stock
  data$Stock_Position[i] <- data$Delta7[i]
  data$Cash_Position[i] <- data$Cash_Position[i-1] - delta_change * data$Underlying.Value[i]
  
  # Calculate portfolio value and P&L
  data$Portfolio_Value[i] <- data$Stock_Position[i] * data$Underlying.Value[i] + data$Cash_Position[i]
  data$PnL[i] <- (data$Portfolio_Value[i] - data$Portfolio_Value[1])
}


# Get unique sorted dates
unique_dates <- sort(unique(data$Date))

# Find the cutoff date for 2 months (approximately 60 days)
cutoff_date <- unique_dates[round(length(unique_dates) * 2 / 3)]

# Split the dataset
train_data <- data %>% filter(Date <= cutoff_date)
test_data <- data %>% filter(Date > cutoff_date)

# View summary
cat("Training Data Range:", min(train_data$Date), "to", max(train_data$Date), "\n")

cat("Testing Data Range:", min(test_data$Date), "to", max(test_data$Date), "\n")


# Linear Regression Model
model_pnl <- lm(PnL ~ weekly_sigma + Underlying.Value + time_to_expiry_years , data = train_data)

summary(model_pnl)

# Predict
predicted_pnl <- predict(model_pnl, newdata = test_data)

# Compare actual vs predicted
plot(test_data$PnL, predicted_pnl, xlab = "Actual PnL", ylab = "Predicted PnL", main = "PnL Prediction VOLTAS PE (1240)")
abline(0, 1, col = "red")

# Calculate residuals (errors)
errors <- predicted_pnl - test_data$PnL

# Plot Actual PnL vs Residuals
plot(test_data$PnL, errors,
     xlab = "Actual PnL",
     ylab = "Residuals",
     main = "Actual PnL vs Residuals",
     pch = 19, col = "blue")

# Add a horizontal line at 0 for reference
abline(h = 0, col = "red", lwd = 2)

# QQ plot for residuals
qqnorm(errors, main = "QQ Plot of Prediction Errors")
qqline(errors, col = "red", lwd = 2)

# Shapiro-Wilk normality test
shapiro_test <- shapiro.test(errors)

# Print Shapiro-Wilk test result
print(shapiro_test)

# Assuming your fitted model is called 'model'
# and your errors are in 'errors'

# --- 1. Check Homoscedasticity ---
# Plot residuals vs fitted values
plot(fitted(model_pnl), residuals(model_pnl),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Breusch-Pagan test for heteroscedasticity
library(lmtest)
bptest(model_pnl)  # Null: homoscedasticity

# --- 2. Check Autocorrelation ---
# Plot autocorrelation function of residuals
acf(residuals(model_pnl), main = "ACF of Residuals")

# Durbin-Watson test for autocorrelation
dwtest(model_pnl)  # Null: no autocorrelation

model_pnl2 <- lm(PnL ~  (weekly_sigma * Underlying.Value) + time_to_expiry_years , data = train_data)

summary(model_pnl2)

# Predict
predicted_pnl2 <- predict(model_pnl2, newdata = test_data)

# Compare actual vs predicted
plot(test_data$PnL, predicted_pnl2, xlab = "Actual PnL", ylab = "Predicted PnL", main = "PnL Prediction VOLTAS CE (1200)")
abline(0, 1, col = "red")

# Calculate residuals (errors)
errors2 <- predicted_pnl2 - test_data$PnL

# Plot Actual PnL vs Residuals
plot(test_data$PnL, errors2,
     xlab = "Actual PnL",
     ylab = "Residuals",
     main = "Actual PnL vs Residuals",
     pch = 19, col = "blue")

# Add a horizontal line at 0 for reference
abline(h = 0, col = "red", lwd = 2)



#ARIMA
# Load required library
library(forecast)

# Assume train_data and test_data are data frames with column "PnL"
train_pnl <- ts(train_data$PnL, frequency = 1)  # Set frequency = 1 if daily/irregular
test_pnl  <- test_data$PnL

# Fit ARIMA model on training data
fit_arima <- auto.arima(train_pnl)

# Print model summary
summary(fit_arima)

# Forecast for the length of test data
forecast_arima <- forecast(fit_arima, h = length(test_pnl))

# Compare forecasted vs actual PnL
predicted_pnl <- as.numeric(forecast_arima$mean)
actual_pnl <- as.numeric(test_pnl)

# Combine results into a data frame
comparison <- data.frame(
  Actual = actual_pnl,
  Predicted = predicted_pnl
)

print(comparison)

# Calculate error metrics
mae <- mean(abs(actual_pnl - predicted_pnl))
rmse <- sqrt(mean((actual_pnl - predicted_pnl)^2))

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Square Error (RMSE):", rmse, "\n")

# Plot actual vs predicted
plot(actual_pnl, type = "l", col = "blue", lwd = 2, ylab = "PnL", xlab = "Time", main = "Actual vs Predicted PnL")
lines(predicted_pnl, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1, lwd = 2)

# Install if not already
install.packages("tseries")
library(tseries)

# Augmented Dickey-Fuller Test
adf_test <- adf.test(train_pnl)

# View result
print(adf_test)

# First differencing
train_pnl_diff1 <- diff(train_pnl, differences = 2)

# ADF test on differenced data
adf_test_diff1 <- adf.test(train_pnl_diff1, alternative = "stationary")
print(adf_test_diff1)

# Load library
library(forecast)

# Plot ACF and PACF for differenced series
par(mfrow = c(1, 2))  # side-by-side plots
Acf(train_pnl_diff1, main = "ACF of Differenced Series")
Pacf(train_pnl_diff1, main = "PACF of Differenced Series")

# Fit ARIMA(1,2,1)
fit_arima_121 <- arima(train_pnl, order = c(1,2,1))

# Summary
summary(fit_arima_121)

# Forecast on test data horizon
library(forecast)
forecast_pnl <- forecast(fit_arima_121, h = length(test_data$PnL))

par(mfrow = c(1,1))
# Plot forecast vs actual test data
plot(forecast_pnl, main = "Forecast vs Actual Test PnL")
lines(test_data$PnL, col = "blue", lwd = 2)
legend("topleft", legend = c("Forecast", "Actual"), 
       col = c("red", "blue"), lty = 1, bty = "n")
