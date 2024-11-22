# Install and load required libraries
required_packages <- c("tidyverse", "ggplot2", "scales", "knitr")

# Install missing packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(knitr)

# Load the dataset (assuming the CSV file is in the working directory)
df <- read.csv("car_resale_prices.csv")
cat("Dataframe loaded. Number of rows:", nrow(df), "\n")

# Clean and preprocess resale_price column
convert_resale_price <- function(price) {
  price <- trimws(price)
  if (grepl("Crore", price)) {
    as.numeric(gsub("[^0-9.]", "", price)) * 10000000
  } else if (grepl("Lakh", price)) {
    as.numeric(gsub("[^0-9.]", "", price)) * 100000
  } else {
    as.numeric(gsub("[^0-9.]", "", price))
  }
}

df$resale_price <- sapply(df$resale_price, convert_resale_price)
cat("Resale price converted.\n")

# Convert kms_driven to numeric
df$kms_driven <- as.numeric(gsub("[^0-9.]", "", df$kms_driven))
cat("kms_driven converted to numeric.\n")

# Filter rows with missing or irrelevant data
df <- df %>% drop_na(fuel_type, resale_price)
cat("Dataframe filtered. New number of rows:", nrow(df), "\n")

# Subsets for analysis
petrol_cars <- df %>% filter(fuel_type == "Petrol")
diesel_cars <- df %>% filter(fuel_type == "Diesel")
cat("Number of petrol cars:", nrow(petrol_cars), "\n")
cat("Number of diesel cars:", nrow(diesel_cars), "\n")

# Function to format large numbers (in lakhs)
format_lakhs <- function(x) {
  paste(format(x / 100000, big.mark = ",", scientific = FALSE), "L")
}

# 1. Histogram for Resale Price
hist_plot <- ggplot(df, aes(x = resale_price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Resale Price (Overall)",
       x = "Resale Price (₹)",
       y = "Count") +
  scale_x_continuous(labels = format_lakhs,
                     breaks = seq(0, max(df$resale_price), length.out = 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(hist_plot)

# 2. Boxplot for Resale Price by Fuel Type
box_plot <- ggplot(df, aes(x = fuel_type, y = resale_price, fill = fuel_type)) +
  geom_boxplot() +
  labs(title = "Resale Price by Fuel Type", x = "Fuel Type", y = "Resale Price (₹)") +
  scale_y_continuous(labels = format_lakhs) +
  theme_minimal()

print(box_plot)

# 3. Independent Samples T-Test
t_test_result <- t.test(petrol_cars$resale_price, diesel_cars$resale_price, var.equal = FALSE)

cat("T-statistic:", t_test_result$statistic, "\n")
cat("P-value:", t_test_result$p.value, "\n")

# 4. Interpretation
if (t_test_result$p.value < 0.05) {
  cat("There is a significant difference in the mean resale price between petrol and diesel cars.\n")
} else {
  cat("There is no significant difference in the mean resale price between petrol and diesel cars.\n")
}

# Additional summary statistics
cat("Summary statistics for resale price:\n")
print(summary(df$resale_price))

cat("Mean resale price for petrol cars:\n")
print(mean(petrol_cars$resale_price))

cat("Mean resale price for diesel cars:\n")
print(mean(diesel_cars$resale_price))

# Find the car with the maximum resale price
max_price_car <- df[which.max(df$resale_price), ]
cat("Car with the maximum resale price:\n")
print(max_price_car)

# Top 10 cars by resale price
top_10_cars <- df %>%
  arrange(desc(resale_price)) %>%
  head(10)

cat("Top 10 cars by resale price:\n")
print(top_10_cars)

# If you want a more formatted output
if (requireNamespace("knitr", quietly = TRUE)) {
  cat("Formatted Top 10 Cars:\n")
  print(kable(top_10_cars, format = "markdown"))
}

# Top 20 prices
top_20_prices <- df %>%
  arrange(desc(resale_price)) %>%
  select(resale_price) %>%
  head(20)

cat("Top 20 prices:\n")
print(top_20_prices)

# Format prices with currency symbols
formatted_prices <- top_20_prices %>%
  mutate(formatted_price = paste0("₹", format(resale_price, big.mark = ",", scientific = FALSE)))

cat("Formatted prices (Top 20):\n")
print(formatted_prices$formatted_price)
