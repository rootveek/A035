library(dplyr)
library(ggplot2)
#testing
#testing
df <- read.csv("C:/Users/rajpu/Desktop/rutvik/A035/Rutvik Kishorbhai Vadher/car_resale_prices.csv")

convert_resale_price <- function(price) {
  price <- gsub("\\s+", "", as.character(price))
  if (grepl("Crore", price)) {
    price <- as.numeric(gsub("[₹Crore,]", "", price)) * 10000000
  } else if (grepl("Lakh", price)) {
    price <- as.numeric(gsub("[₹Lakh,]", "", price)) * 100000
  } else {
    price <- as.numeric(gsub("[₹,]", "", price))
  }
  return(price)
}

df <- df %>%
  mutate(
    resale_price = sapply(resale_price, convert_resale_price),
    kms_driven = as.numeric(gsub("[,Kms]", "", kms_driven)),
    resale_price_lakh = resale_price / 100000
  ) %>%
  filter(
    fuel_type %in% c("Petrol", "Diesel", "Electric", "CNG"),
    resale_price <= 2500000
  ) %>%
  filter(!is.na(fuel_type), !is.na(resale_price_lakh))

ggplot(df, aes(x = resale_price_lakh)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 20,
    fill = "skyblue",
    color = "black",
    alpha = 0.7
  ) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Histogram of Resale Price in Lakh ₹",
    x = "Resale Price (Lakh ₹)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(df, aes(x = fuel_type, y = resale_price_lakh, fill = fuel_type)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Resale Price by Fuel Type",
    x = "Fuel Type",
    y = "Resale Price (Lakh ₹)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

kruskal_test <- kruskal.test(resale_price_lakh ~ fuel_type, data = df)
if (kruskal_test$p.value < 0.05) {
  pairwise_wilcox <- pairwise.wilcox.test(df$resale_price_lakh, df$fuel_type, p.adjust.method = "bonferroni")
  print(pairwise_wilcox)
}

if (kruskal_test$p.value < 0.05) {
  cat("\nConclusion: Reject the null hypothesis. There is a significant difference in resale price between at least two fuel types.\n")
} else {
  cat("\nConclusion: Fail to reject the null hypothesis. No significant difference in resale price between fuel types.\n")
}
