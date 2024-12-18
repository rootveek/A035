library(corrplot)
numeric_cols <- car_data[,c("resale_price", "kms_driven", "engine_capacity", "max_power")]
correlation_matrix <- cor(numeric_cols, use="complete.obs")
corrplot(correlation_matrix, method="color", type="upper", 
         addCoef.col = "black", tl.col="black", tl.srt=45,
         title="Correlation Between Numerical Variables")
