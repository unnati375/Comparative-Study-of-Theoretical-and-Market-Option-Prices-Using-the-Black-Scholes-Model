# Load necessary library
library(dplyr)
library(ggplot2)

# Load your data (replace with your actual file)
df <- read.csv("C:/Users/UTTATI/OneDrive/Desktop/Projects/DM project/Final data/VOLTAS PE (1240) FINAL2.csv")

# Ensure date is Date type
df$Date <- as.Date(df$Date)

# Sort by date
df <- df %>% arrange(Date)

# Initialize hedge portfolio
df$Stock_Position <- NA
df$Cash_Position <- NA
df$Portfolio_Value <- NA
df$PnL <- NA

# Assume initial option position is short 1 contract
# Initial hedge: Buy delta units of stock
df$Stock_Position[1] <- df$Delta[1]
df$Cash_Position[1] <- -df$Stock_Position[1] * df$Underlying.Value[1]
df$Portfolio_Value[1] <- df$Stock_Position[1] * df$Underlying.Value[1] + df$Cash_Position[1]
df$PnL[1] <- 0  # No P&L on day 1

# Loop through and simulate delta hedging
for (i in 2:nrow(df)) {
  # Rebalance stock position to match new delta
  delta_change <- df$Delta[i] - df$Stock_Position[i-1]
  
  # Buy/sell delta_change units of stock
  df$Stock_Position[i] <- df$Delta[i]
  df$Cash_Position[i] <- df$Cash_Position[i-1] - delta_change * df$Underlying.Value[i]
  
  # Calculate portfolio value and P&L
  df$Portfolio_Value[i] <- df$Stock_Position[i] * df$Underlying.Value[i] + df$Cash_Position[i]
  df$PnL[i] <- df$Portfolio_Value[i] - df$Portfolio_Value[1]
}

# Plot P&L over time
ggplot(df, aes(x = Date, y = PnL)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Delta-Neutral Hedging P&L Over Time VOLTAS PE (1240)",
       x = "Date", y = "P&L (in currency units)") +
  theme_minimal()

# View final dataframe
head(df)
