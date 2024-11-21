# Load the dataset
car_data <- read.csv("car_resale_prices.csv")

# Filter data for years 2018 to 2020
filtered_data <- subset(car_data, registered_year >= 2018 & registered_year <= 2020)

# View the filtered data
head(filtered_data)

# Save the filtered dataset to a new file (optional)
write.csv(filtered_data, "filtered_car_data_2018_2020.csv", row.names = FALSE)







# Load the dataset
car_data <- read.csv("car_resale_prices.csv")

# Define a function to remove outliers based on the IQR method
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)  # First quartile (25th percentile)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)  # Third quartile (75th percentile)
  IQR <- Q3 - Q1                                    # Interquartile range
  lower_bound <- Q1 - 1.5 * IQR                     # Lower bound
  upper_bound <- Q3 + 1.5 * IQR                     # Upper bound
  
  # Filter rows within the bounds
  data <- data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ]
  return(data)
}

# Remove outliers from the 'resale_price' column
# Replace 'resale_price' with the actual column name in your dataset
car_data_cleaned <- remove_outliers(car_data, "resale_price")

# Check the cleaned dataset
summary(car_data_cleaned$resale_price)

# Save the cleaned dataset to a new file (optional)
write.csv(car_data_cleaned, "car_resale_prices_cleaned.csv", row.names = FALSE)






# Check the data type
str(car_data$resale_price)

# Remove any non-numeric characters (like currency symbols) and convert to numeric
car_data$resale_price <- as.numeric(gsub("[^0-9.]", "", car_data$resale_price))

# Check again to ensure it's now numeric
summary(car_data$resale_price)





# Remove rows with missing values in the resale_price column
car_data <- na.omit(car_data)





car_data_cleaned <- remove_outliers(car_data, "resale_price")




# Function to count outliers using IQR method
count_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)  # First quartile
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)  # Third quartile
  IQR <- Q3 - Q1                                     # Interquartile range
  lower_bound <- Q1 - 1.5 * IQR                      # Lower bound
  upper_bound <- Q3 + 1.5 * IQR                      # Upper bound
  
  # Identify outliers
  outliers <- data[[column]] < lower_bound | data[[column]] > upper_bound
  
  # Count the number of outliers
  return(sum(outliers, na.rm = TRUE))
}

# Count the number of outliers in the 'resale_price' column
num_outliers <- count_outliers(car_data, "resale_price")
cat("Number of outliers before removal:", num_outliers, "\n")

# Remove outliers using the previous function
car_data_cleaned <- remove_outliers(car_data, "resale_price")

# Count the number of rows removed
rows_removed <- nrow(car_data) - nrow(car_data_cleaned)
cat("Number of rows removed (outliers):", rows_removed, "\n")

# Check the cleaned dataset
summary(car_data_cleaned$resale_price)
# Function to count outliers using IQR method
count_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)  # First quartile
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)  # Third quartile
  IQR <- Q3 - Q1                                     # Interquartile range
  lower_bound <- Q1 - 1.5 * IQR                      # Lower bound
  upper_bound <- Q3 + 1.5 * IQR                      # Upper bound
  
  # Identify outliers
  outliers <- data[[column]] < lower_bound | data[[column]] > upper_bound
  
  # Count the number of outliers
  return(sum(outliers, na.rm = TRUE))
}

# Count the number of outliers in the 'resale_price' column
num_outliers <- count_outliers(car_data, "resale_price")
cat("Number of outliers before removal:", num_outliers, "\n")

# Remove outliers using the previous function
car_data_cleaned <- remove_outliers(car_data, "resale_price")

# Count the number of rows removed
rows_removed <- nrow(car_data) - nrow(car_data_cleaned)
cat("Number of rows removed (outliers):", rows_removed, "\n")

# Check the cleaned dataset
summary(car_data_cleaned$resale_price)






# Remove non-numeric characters (e.g., ₹, commas)
car_data$resale_price <- gsub("[^0-9.]", "", car_data$resale_price)

# Convert the cleaned column to numeric
car_data$resale_price <- as.numeric(car_data$resale_price)

# Check for successful conversion
summary(car_data$resale_price)





sum(is.na(car_data$resale_price))  # Count missing values

# Remove rows with missing or invalid data
car_data <- car_data[!is.na(car_data$resale_price), ]





car_data_cleaned <- remove_outliers(car_data, "resale_price")









# Load necessary libraries
library(ggplot2)

# Create a histogram with normal curve overlay
ggplot(car_data_cleaned, aes(x = resale_price)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", colour = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(car_data_cleaned$resale_price, na.rm = TRUE),
                                         sd = sd(car_data_cleaned$resale_price, na.rm = TRUE)),
                colour = "red", size = 1) +
  labs(title = "Histogram of Resale Price with Normal Curve",
       x = "Resale Price",
       y = "Density") +
  theme_minimal()





car_data_cleaned$fuel_type <- as.factor(car_data_cleaned$fuel_type)
sum(is.na(car_data_cleaned$resale_price))
sum(is.na(car_data_cleaned$fuel_type))






# Run pairwise Wilcoxon test
pairwise_results <- pairwise.wilcox.test(
  x = car_data_cleaned$resale_price,    # Dependent variable
  g = car_data_cleaned$fuel_type,      # Independent variable (factor)
  p.adjust.method = "bonferroni"       # Adjust p-values for multiple comparisons
)

# View the results
pairwise_results








