# ========================================================
# CI7330 - Assignment 2 
# ========================================================
# Question 2: Hip Fracture Prediction Analysis
# ========================================================

# Load data
bmd <- read.csv("bmd.csv")
library(ggplot2)
library(reshape2)

# Basic checks
head(bmd)
dim(bmd)
sum(is.na(bmd))

summary(bmd)
table(bmd$fracture)

# Summary by fracture group
aggregate(bmd ~ fracture, data = bmd, FUN = mean)
aggregate(bmd ~ fracture, data = bmd, FUN = sd)

fracture_counts <- as.data.frame(table(bmd$fracture))
colnames(fracture_counts) <- c("Fracture", "Count")
fracture_counts$Fracture <- factor(fracture_counts$Fracture, levels = c("0", "1"),
                                   labels = c("No Fracture", "Fracture"))

ggplot(fracture_counts, aes(x = Fracture, y = Count, fill = Fracture)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_manual(values = c("No Fracture" = "lightblue", "Fracture" = "salmon")) +
  labs(title = "Distribution of Hip Fracture Status", x = "Fracture Status", y = "Number of Patients") +
  ylim(0, 140) + theme_minimal(base_size = 12) + theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"))


#Histogram of fracture
# Simple bar chart of fracture (base R – clean and safe)
barplot(table(bmd$fracture),
        names.arg = c("No Fracture", "Fracture"),
        col = c("lightblue", "salmon"),
        main = "Distribution of Hip Fracture Status",
        xlab = "Fracture Status", ylab = "Number of Patients",
        ylim = c(0, 140))

# Histogram of BMD
hist(bmd$bmd, 
     main = "Distribution of Bone Mineral Density (bmd)", col = "lightblue", border = "black",
     xlab = "bmd (g/cm²)", ylab = "No. of observations (n)", breaks = 10)


# Boxplot of BMD along fracture
boxplot(bmd ~ fracture, data = bmd,
        main = "bmd by Fracture Status",
        xlab = "Fracture (0 = No, 1 = Yes)", ylab = "bmd (g/cm²)",
        col = c("lightblue", "salmon"))


# Correlation heatmap
# Select numeric and binary columns (fracture is 0/1, treated as numeric)
numeric_data <- bmd[, c("age", "sex", "weight_kg", "height_cm", "medication", "waiting_time", "bmd", "fracture")]

cor_matrix <- cor(numeric_data, use = "complete.obs")

cormartix_melt <- melt(cor_matrix)

ggplot(cormartix_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap of Predictors and Hip Fracture",
       x = "", y = "") +
  coord_fixed()

# T-test
t.test(bmd ~ fracture, data = bmd)

# logistic regression model
modellr <- glm(fracture ~ bmd, data = bmd, family = binomial)
summary(modellr)

------------------------------------------------------------------------------------------
  
# Full logistic regression
model1 <- glm(fracture ~ age + sex + weight_kg + height_cm + medication + waiting_time + bmd, data = bmd, family = binomial)
summary(model1)

model2 <- glm(fracture ~ age + sex + weight_kg + height_cm + medication + bmd, data = bmd, family = binomial)
summary(model2)

model3 <- glm(fracture ~ age + sex + height_cm + medication + bmd, data = bmd, family = binomial)
summary(model3)

model4 <- glm(fracture ~ age + sex + medication + bmd, data = bmd, family = binomial)
summary(model4)

model5 <- glm(fracture ~ sex + medication + bmd, data = bmd, family = binomial)
summary(model5)


# Final model
final_model <- model5
summary(final_model)

# Final pseudo R²
1 - (final_model$deviance / final_model$null.deviance)

-------------------------------------------------------------------------------------

# Clean ggplot histogram of deviance residuals
ggplot(data = data.frame(resid = residuals(model5, type = "deviance")), aes(x = resid)) +
  geom_histogram(binwidth = 0.3, fill = "lightblue", color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Deviance Residuals",x = "Residuals", y = "Total Observations") +
  theme_minimal()


# 1. Cleaner Residuals vs Fitted (ggplot)
ggplot(data = data.frame(fitted = fitted(model5), resid = residuals(model5, type = "deviance")),
       aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.7, color = "blue", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + scale_x_continuous(limits = c(0, 1))


# 2. Cleaner Q-Q plot (ggplot)
ggplot(data = data.frame(resid = residuals(model5, type = "deviance")), aes(sample = resid)) +
  stat_qq(color = "blue") + stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Residuals") +
  theme_minimal()