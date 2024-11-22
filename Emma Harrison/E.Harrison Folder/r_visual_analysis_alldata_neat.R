library(ggplot2)

car_data <- read.csv("car_resale_prices.csv")

# Filter dataset 2018 and 2020
filtered_data <- subset(car_data, registered_year >= 2018 & registered_year <= 2020)
write.csv(filtered_data, "filtered_car_data_2018_2020.csv", row.names = FALSE)

# Remove non-numeric characters and convert
car_data$resale_price <- as.numeric(gsub("[^0-9.]", "", car_data$resale_price))

car_data <- na.omit(car_data)

# Function to remove outliers using IQR
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data <- data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ]
  return(data)
}

car_data_cleaned <- remove_outliers(car_data, "resale_price")

summary(car_data_cleaned$resale_price)

# Count how many outliers BEFORE removal
count_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- data[[column]] < lower_bound | data[[column]] > upper_bound
  return(sum(outliers, na.rm = TRUE))
}

num_outliers <- count_outliers(car_data, "resale_price")
cat("Number of outliers before removal:", num_outliers, "\n")

rows_removed <- nrow(car_data) - nrow(car_data_cleaned)
cat("Number of rows removed (outliers):", rows_removed, "\n")

write.csv(car_data_cleaned, "car_resale_prices_cleaned.csv", row.names = FALSE)

# Histogam
ggplot(car_data_cleaned, aes(x = resale_price)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", colour = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(car_data_cleaned$resale_price, na.rm = TRUE),
                                         sd = sd(car_data_cleaned$resale_price, na.rm = TRUE)),
                colour = "red", size = 1) +
  labs(title = "Histogram of Resale Price with Normal Curve",
       x = "Resale Price",
       y = "Density") +
  theme_minimal()

# Convert fuel_type to factor for Wilcoxon test
car_data_cleaned$fuel_type <- as.factor(car_data_cleaned$fuel_type)

# pairwise Wilcoxon test
pairwise_results <- pairwise.wilcox.test(
  x = car_data_cleaned$resale_price,
  g = car_data_cleaned$fuel_type,
  p.adjust.method = "bonferroni"
)

# Results
print(pairwise_results)