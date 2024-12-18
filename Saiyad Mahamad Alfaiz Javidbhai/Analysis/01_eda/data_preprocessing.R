library(dplyr)
car_data <- read.csv("car_data.csv")
clean_data <- car_data %>%
    mutate(
          resale_price = as.numeric(gsub("₹|Lakh", "", resale_price)),
        kms_driven = as.numeric(gsub(",|Kms", "", kms_driven)),
        engine_capacity = as.numeric(gsub("cc", "", engine_capacity))
    )




summary(clean_data)
