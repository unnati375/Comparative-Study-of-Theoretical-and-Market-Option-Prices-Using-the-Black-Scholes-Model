library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Read the CSV
data <- read_csv("C:/Users/UTTATI/OneDrive/Desktop/Projects/DM project/Final data/VOLTAS PE (1200) FINAL.csv")

# Step 2: Convert DATE to Date format
data$Date <- as.Date(data$Date)


# Step 4: Reshape for plotting
plot_data <- data %>%
  select(Date, P, `Settle Price`) %>%
  pivot_longer(cols = c(P, `Settle Price`), names_to = "Type", values_to = "Price")

# Step 5: Plot
ggplot(plot_data, aes(x = Date, y = Price, color = Type)) +
  geom_line(size = 1) +
  labs(title = "Theoretical vs Market Put Option Prices INFY (1520)",
       x = "Date", y = "Price",
       color = "Price Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "top"
  )

library(ggplot2)

# Ensure DATE is Date type
data$Date <- as.Date(data$Date)

# Plot diff vs. DATE
ggplot(data, aes(x = Date, y = diff)) +
  geom_line(color = "steelblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "VOLTAS PE (1200)",
       x = "Date",
       y = "Price Difference (Settle Price - BS Price)") +
  theme_minimal()

library(ggplot2)
library(dplyr)

# Assuming you already have these four dataframes:
# infy_data, hdfc_data, hindalco_data, voltas_data

# Step 1: Add a 'Company' column to each
infy_data$Company <- "INFY"
hdfc_data$Company <- "HDFC"
hindalco_data$Company <- "HINDALCO"
voltas_data$Company <- "VOLTAS"

# Step 2: Combine them into one dataframe
combined_data <- bind_rows(infy_data, hdfc_data, hindalco_data, voltas_data)

# Step 3: Filter only call options
call_data <- combined_data %>% filter(`Option type` == "CE")

# Step 4: Convert DATE column to Date type
call_data$DATE <- as.Date(call_data$DATE)

# Step 5: Plot diff vs DATE for all companies
ggplot(call_data, aes(x = DATE, y = diff, color = Company)) +
  geom_line(size = 1) +
  labs(
    title = "Pricing Error Over Time for Call Options (CE)",
    x = "Date",
    y = "Pricing Error (diff = Settle Price − BS Price)",
    color = "Company"
  ) +
  theme_minimal()
