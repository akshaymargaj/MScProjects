# ========================================================
# CI7330 - Assignment 2 
# ========================================================
# Question 1: Red Wine Quality
# ========================================================

getwd()

setwd("D:/MSc Assignments/DataVisual/Assignment 2")

# 1. Load the data
wine <- read.csv("redwine.csv")
library(ggplot2)

# Quick checks
head(wine)
summary(wine)
sum(is.na(wine))

# 2. Very basic exploratory plots (you can add more if you want)
hist(wine$quality, main="Wine Quality Distribution", 
     xlab="Quality", ylab = "Total Cases", col="lightblue", breaks=15)

boxplot(wine$alcohol ~ round(wine$quality), 
        main="Alcohol by Quality Level", 
        xlab="Quality", ylab="Alcohol", col="lightgreen")

boxplot(wine$volacid ~ round(wine$quality), 
        main="Volatile Acidity by Quality Level", 
        xlab="Quality", ylab="Volatile Acidity", col="salmon")

-------------------------------------------------------------------------

install.packages("reshape2")  
    
library(ggplot2)
library(reshape2)

# Select only numeric columns (exclude id if present)
numeric_cols <- c("fixacid", "volacid", "citric", "residsugar", "chlorides", 
                  "freeSO2", "totalSO2", "density", "pH", "sulphates", "alcohol", "quality")

numeric_data <- wine[, numeric_cols]

cor_matrix <- cor(numeric_data, use = "complete.obs")

correlation_matrix <- melt(cor_matrix)

# Plot heatmap
ggplot(correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1,1), space = "Lab", name = "Correlation") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap of Predictors and Wine Quality",
       x = "", y = "") + coord_fixed()

# ================================================
# Model Prediction using MANUAL BACKWARD ELIMINATION
# ================================================

# Step 1: Full model
Model1 <- lm(quality ~ fixacid + volacid + citric + residsugar + chlorides + freeSO2 + 
               totalSO2 + density + pH + sulphates + alcohol, data = wine)
summary(Model1)

# Step 2: Remove the least significant (example: freeSO2 often first)
model2 <- lm(quality ~ fixacid + volacid + citric + residsugar + chlorides + totalSO2 + 
               density + pH + sulphates + alcohol, data = wine)
summary(model2)

# Step 3: Remove next least significant (example: residsugar often next)
model3 <- lm(quality ~ fixacid + volacid + citric + chlorides + totalSO2 + density + pH + 
               sulphates + alcohol, data = wine)
summary(model3)


# Step 4: Keep going until all remaining variables are significant (p < 0.05)
model4 <- lm(quality ~ volacid + chlorides + totalSO2 + pH + sulphates + alcohol,
                  data = wine)
summary(model4)

# ================================================
# Final model diagnostics
# ================================================

# 1. Residuals vs Fitted
  ggplot(data = data.frame(fitted = fitted(final_model), resid = residuals(final_model)), aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, color = "darkred") +
  labs(title = "Residuals vs Fitted Values (Final Model)", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# 2. Simple Histogram of Residuals
ggplot(data = data.frame(resid = residuals(final_model)), aes(x = resid)) +
  geom_histogram(binwidth = 0.2, fill = "lightblue", color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Residuals (Final Model)", x = "Residuals", y = "Count") +
  theme_minimal()


# 3. Normal Q-Q Plot of Residuals
ggplot(data = data.frame(resid = residuals(final_model)), aes(sample = resid)) +
  stat_qq(color = "blue") + stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot of Residuals (Final Model)", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
  

# ================================================
# Some useful numbers for the report
# ================================================

new_wine <- data.frame(
  volacid = 0.50,
  chlorides = 0.08,
  totalSO2 = 35,
  pH = 3.30,
  sulphates = 0.65,
  alcohol = 10.5
)

predict(final_model, new_wine)
predict(final_model, new_wine, interval = "confidence")

cat("Final Adjusted R-squared:", round(summary(final_model)$adj.r.squared, 4), "\n")
cat("Residual Standard Error:", round(summary(final_model)$sigma, 4), "\n")