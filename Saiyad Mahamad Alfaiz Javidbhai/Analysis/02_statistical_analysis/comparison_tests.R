library(dplyr)
library(ggplot2)

# Compare prices by fuel type
analyze_fuel_type_prices <- function(data) {
    fuel_test <- aov(resale_price ~ fuel_type, data=data)
    fuel_plot <- ggplot(data, aes(x=fuel_type, y=resale_price)) +
        geom_boxplot(fill="lightblue") +
        labs(title="Resale Price Distribution by Fuel Type",
             x="Fuel Type",
             y="Resale Price (Lakhs)")
    
    return(list(test=summary(fuel_test), plot=fuel_plot))
}

# Compare prices by transmission type
analyze_transmission_prices <- function(data) {
    trans_test <- t.test(resale_price ~ transmission_type, data=data)
    trans_plot <- ggplot(data, aes(x=transmission_type, y=resale_price)) +
        geom_boxplot(fill="lightgreen") +
        labs(title="Resale Price Distribution by Transmission",
             x="Transmission Type",
             y="Resale Price (Lakhs)")
    
    return(list(test=trans_test, plot=trans_plot))
}

# Compare prices by owner type
analyze_owner_type_prices <- function(data) {
    owner_test <- aov(resale_price ~ owner_type, data=data)
    owner_plot <- ggplot(data, aes(x=owner_type, y=resale_price)) +
        geom_boxplot(fill="lightpink") +
        labs(title="Resale Price Distribution by Owner Type",
             x="Owner Type",
             y="Resale Price (Lakhs)")
    
    return(list(test=summary(owner_test), plot=owner_plot))
}
