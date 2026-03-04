# Load the data
df <- read.csv("Employeesatisfaction.csv")

# Convert categorical variables
data$education <- factor(data$education, levels = 1:5, labels = c("No formal qualifications", "Minimum 5 GCSEs", "A level or equivalent", "HE degree", "Post degree qualification"))
data$Sex <- factor(data$Sex)
data$Jobsatisfaction <- factor(data$Jobsatisfaction, levels = 1:5, labels = c("Not satisfied", "Somewhat unsatisfied", "Neutral", "Somewhat satisfied", "Very satisfied"))

# View structure to confirm
str(data)

#Question 1:

# Summary statistics
summary(df$Age)
mode_age <- as.numeric(names(sort(table(df$Age), decreasing = TRUE)[1]))

# Visualization: Histogram
hist(df$Age, main = "Distribution of Employee Ages", xlab = "Age (years)", ylab = "Total Employees", col = "lightblue", border = "black", breaks = 10)


#Question 2:

# Summary statistics
summary(df$incomeGBP)
mode_income <- as.numeric(names(sort(table(df$incomeGBP), decreasing = TRUE)[1]))

# Visualization: Histogram
hist(df$incomeGBP, main = "Distribution of Monthly Incomes", xlab = "Income (GBP)", ylab = "Total Employees", col = "lightgreen", border = "black", breaks = 10)

#Question 3:

# Summary statistics
summary(df$distance)
mode_dist <- as.numeric(names(sort(table(df$distance), decreasing = TRUE)[1]))

# Visualization: Histogram
hist(df$distance, main = "Distribution of Travel Distances to Work", xlab = "Distance (miles)", ylab = "Total Employees", col = "lightyellow", border = "black", breaks = 5)

#Question 4:

# Frequency table
edu_table <- table(df$education)

# Proportions
edu_prop <- prop.table(edu_table) * 100

# Combine into a data frame for display
edu_summary <- data.frame(Level = names(edu_table), Count = as.vector(edu_table), Proportion = as.vector(edu_prop))
print(edu_summary)


#Ques 5:

# Visualization: Scatterplot
plot(df$Age, df$incomeGBP, main = "Monthly Income vs. Employee Age", xlab = "Age (years)", ylab = "Income (GBP)", pch = 19, col = "blue")

# Correlation
cor(df$Age, df$incomeGBP)


#Ques 6:

# Visualization: Boxplot
boxplot(incomeGBP ~ Sex, data = df, main = "Monthly Income by Sex", xlab = "Sex", ylab = "Income (GBP)", col = c("pink", "lightblue"), notch = TRUE)

# T-test
t.test(incomeGBP ~ Sex, data = df)


#ques 7:

# Visualization: Boxplot
boxplot(incomeGBP ~ education, data = df, main = "Monthly Income by Education Level", xlab = "Education Level (1-5)", ylab = "Income (GBP)", col = rainbow(5), notch = TRUE)

# ANOVA
anova_result <- aov(incomeGBP ~ factor(education), data = df)
summary(anova_result)

# Post-hoc Tukey HSD
TukeyHSD(anova_result)


#Ques 8:

# (i) Crosstab for Sex
table_sex <- table(df$Jobsatisfaction, df$Sex)
prop.table(table_sex, margin = 2)  # Column proportions
chisq.test(table_sex)

# (ii) Crosstab for Education
table_edu <- table(df$Jobsatisfaction, df$education)
prop.table(table_edu, margin = 2)  # Column proportions
chisq.test(table_edu)