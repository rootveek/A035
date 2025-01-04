# Install and load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(knitr)

# Load the dataset (ensure the CSV file is in the working directory)
car_data <- read.csv("car_resale_prices.csv", stringsAsFactors = FALSE)
cat("Dataset loaded successfully. Total records:", nrow(car_data), "\n")

# Function to clean and convert resale price to numeric values
parse_resale_price <- function(price_text) {
  price_text <- trimws(price_text)
  if (grepl("Crore", price_text)) {
    as.numeric(gsub("[^0-9.]", "", price_text)) * 1e7
  } else if (grepl("Lakh", price_text)) {
    as.numeric(gsub("[^0-9.]", "", price_text)) * 1e5
  } else {
    as.numeric(gsub("[^0-9.]", "", price_text))
  }
}

# Apply the conversion to the resale_price column
car_data$resale_price <- sapply(car_data$resale_price, parse_resale_price)
cat("Resale price column cleaned and converted.\n")

# Clean and convert kms_driven to numeric
car_data$kms_driven <- as.numeric(gsub("[^0-9]", "", car_data$kms_driven))
cat("Kilometers driven column cleaned and converted to numeric.\n")

# Remove records with missing or invalid data
filtered_data <- car_data %>%
  filter(!is.na(resale_price), !is.na(fuel_type)) %>%
  drop_na()
cat("Filtered dataset. Remaining records:", nrow(filtered_data), "\n")

# Subset the data for petrol and diesel cars
petrol_cars <- filtered_data %>% filter(fuel_type == "Petrol")
diesel_cars <- filtered_data %>% filter(fuel_type == "Diesel")
cat("Number of petrol cars:", nrow(petrol_cars), "\n")
cat("Number of diesel cars:", nrow(diesel_cars), "\n")

# Custom function to format values in lakhs
format_in_lakhs <- function(value) {
  paste0(format(value / 1e5, big.mark = ",", scientific = FALSE), " L")
}

# 1. Histogram of Resale Prices
histogram_plot <- ggplot(filtered_data, aes(x = resale_price)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(
    title = "Distribution of Car Resale Prices",
    x = "Resale Price (₹)",
    y = "Frequency"
  ) +
  scale_x_continuous(labels = format_in_lakhs) +
  theme_minimal()

print(histogram_plot)

# 2. Boxplot of Resale Prices by Fuel Type
boxplot_chart <- ggplot(filtered_data, aes(x = fuel_type, y = resale_price, fill = fuel_type)) +
  geom_boxplot() +
  labs(
    title = "Resale Prices by Fuel Type",
    x = "Fuel Type",
    y = "Resale Price (₹)"
  ) +
  scale_y_continuous(labels = format_in_lakhs) +
  theme_minimal()

print(boxplot_chart)

# 3. Perform an Independent Samples T-Test
t_test <- t.test(petrol_cars$resale_price, diesel_cars$resale_price, var.equal = FALSE)

cat("T-Test Results:\n")
cat("T-Statistic:", t_test$statistic, "\n")
cat("P-Value:", t_test$p.value, "\n")

# 4. Interpret Results
if (t_test$p.value < 0.05) {
  cat("The difference in resale prices between petrol and diesel cars is statistically significant.\n")
} else {
  cat("No statistically significant difference in resale prices between petrol and diesel cars.\n")
}

# Additional Summary
cat("Resale Price Summary:\n")
print(summary(filtered_data$resale_price))

cat("Mean Resale Price (Petrol Cars):", mean(petrol_cars$resale_price), "\n")
cat("Mean Resale Price (Diesel Cars):", mean(diesel_cars$resale_price), "\n")

# Car with the highest resale price
most_expensive_car <- filtered_data[which.max(filtered_data$resale_price), ]
cat("Details of the car with the highest resale price:\n")
print(most_expensive_car)

# Top 10 cars by resale price
top_10 <- filtered_data %>%
  arrange(desc(resale_price)) %>%
  head(10)

cat("Top 10 Cars by Resale Price:\n")
print(top_10)

# If better formatting is desired
if (requireNamespace("knitr", quietly = TRUE)) {
  cat("Formatted Top 10 Cars:\n")
  print(kable(top_10, format = "markdown"))
}

# Top 20 Resale Prices
top_20_prices <- filtered_data %>%
  arrange(desc(resale_price)) %>%
  select(resale_price) %>%
  head(20)

cat("Top 20 Resale Prices:\n")
print(top_20_prices)

# Format the top 20 prices for display
formatted_prices <- top_20_prices %>%
  mutate(formatted = paste0("₹", format(resale_price, big.mark = ",", scientific = FALSE)))

cat("Formatted Resale Prices (Top 20):\n")
print(formatted_prices$formatted)

