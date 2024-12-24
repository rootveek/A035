library(dplyr)
library(ggplot2)

MAX_PRICE_THRESHOLD <- 2500000
VALID_FUEL_TYPES <- c("Petrol", "Diesel", "Electric", "CNG")
convert_price <- function(price_str) {
  if(is.na(price_str)) return(NA)
  clean_price <- gsub("\\s+", "", as.character(price_str))
  clean_price <- gsub(",", "", clean_price)
  if(grepl("Crore", clean_price)) {
    return(as.numeric(sub("Crore", "", clean_price)) * 10000000)
  } else if(grepl("Lakh", clean_price)) {
    return(as.numeric(sub("Lakh", "", clean_price)) * 100000)
  } else {
    return(as.numeric(gsub("[₹]", "", clean_price)))
  }
}
process_car_data <- function(file_path) {
  tryCatch({
    df <- read.csv(file_path, stringsAsFactors = FALSE)
    clean_df <- df %>%
      mutate(
        resale_price = sapply(resale_price, convert_price),
        kms_driven = as.numeric(gsub("[,Kms]", "", kms_driven)),
        price_in_lakh = resale_price / 100000
      ) %>%
      filter(
        fuel_type %in% VALID_FUEL_TYPES,
        resale_price <= MAX_PRICE_THRESHOLD,
        !is.na(fuel_type),
        !is.na(price_in_lakh)
      )
    price_hist <- ggplot(clean_df, aes(x = price_in_lakh)) +
      geom_histogram(bins = 20, fill = "blue", alpha = 0.7) +
      labs(
        title = "Car Resale Price Distribution",
        x = "Price (Lakh ₹)",
        y = "Frequency"
      )
    price_by_fuel <- ggplot(clean_df, aes(x = fuel_type, y = price_in_lakh)) +
      geom_boxplot(aes(fill = fuel_type)) +
      labs(
        title = "Resale Price by Fuel Type",
        x = "Fuel Type",
        y = "Price (Lakh ₹)"
      )
    stat_test <- tryCatch({
      kruskal.test(price_in_lakh ~ fuel_type, data = clean_df)
    }, error = function(e) {
      warning("Statistical test failed")
      return(NULL)
    })
    ggsave("price_histogram.png", price_hist)
    ggsave("price_by_fuel.png", price_by_fuel)
    list(
      data = clean_df,
      stats = stat_test,
      plots = list(price_hist, price_by_fuel)
    )
  }, error = function(e) {
    print(paste("Error processing file:", e$message))
    return(NULL)
  })
}
main <- function() {
  result <- process_car_data("car_resale_prices.csv")
  if(!is.null(result)) {
    print("Analysis completed")
    print(summary(result$data))
    print(result$stats)
  } else {
    print("Analysis failed")
  }
}
