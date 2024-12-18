library(dplyr)
library(ggplot2)

# Price vs Age Analysis
analyze_age_correlation <- function(data) {
    data$car_age <- 2024 - data$registered_year
    age_correlation <- cor.test(data$resale_price, data$car_age)
    age_plot <- ggplot(data, aes(x=car_age, y=resale_price)) +
        geom_point() +
        labs(title="Car Age vs Resale Price",
             x="Car Age (Years)",
             y="Resale Price (Lakhs)")
    
    return(list(correlation=age_correlation, plot=age_plot))
}

# Price vs Kilometers Driven
analyze_km_correlation <- function(data) {
    km_correlation <- cor.test(data$resale_price, data$kms_driven)
    
    km_plot <- ggplot(data, aes(x=kms_driven, y=resale_price)) +
        geom_point() +
        labs(title="Kilometers Driven vs Resale Price",
             x="Kilometers Driven",
             y="Resale Price (Lakhs)")
    
    return(list(correlation=km_correlation, plot=km_plot))
}

# Price vs Engine Capacity
analyze_engine_correlation <- function(data) {
    engine_correlation <- cor.test(data$resale_price, 
                                 data$engine_capacity)
    
    engine_plot <- ggplot(data, aes(x=engine_capacity, y=resale_price)) +
        geom_point() +
        labs(title="Engine Capacity vs Resale Price",
             x="Engine Capacity (cc)",
             y="Resale Price (Lakhs)")
    
    return(list(correlation=engine_correlation, plot=engine_plot))
}
