#Check the directory
getwd()

#Change the directory to the necessary file path
setwd("D:/MSc Assignments/DataVisual/Assign1")


# Load required libraries
library(ggplot2)


#Data Loading
data <- read.csv("Employeesatisfaction.csv")


# Convert categorical variables
data$education <- factor(data$education, levels = 1:5, labels = c("No formal qualifications", "Minimum 5 GCSEs", "A level or equivalent", "HE degree", "Post degree qualification"))
data$Sex <- factor(data$Sex)
data$Jobsatisfaction <- factor(data$Jobsatisfaction, levels = 1:5, labels = c("Not satisfied", "Somewhat unsatisfied", "Neutral", "Somewhat satisfied", "Very satisfied"))

# View structure to confirm
str(data)


#Question 1: Describe the age profile of the workforce

hist(data$Age, main = "Distribution of Employee Ages", xlab = "Age (in years)", col = "lightblue", border = "black", breaks = 10)
summary(data$Age)
sd(data$Age)


#Question 2: Describe the average monthly incomes of employees.

summary(data$incomeGBP)
hist(data$incomeGBP, main="Distribution of Monthly Incomes", xlab="Income (GBP)", col="lightgreen", border="black")


#Question 3: Describe and summarize the average distances employees travel to work.

summary(data$distance)
hist(data$distance, main="Distribution of Travel Distances", xlab="Distance (miles)", col="lightyellow", border="black")


#Question 4: Describe and summarize employees’ educational backgrounds.

table(data$education)
prop.table(table(data$education))


#Question 5: Explore incomes by age

plot(data$Age, data$incomeGBP, main="Income vs Age", xlab="Age (years)", ylab="Income (GBP)", pch=19, col="blue")
cor(data$Age, data$incomeGBP)


#Question 6: Explore incomes between the sexes

boxplot(incomeGBP ~ Sex, data=data, main="Income by Sex", ylab="Income (GBP)", col=c("pink", "lightblue"))
t.test(incomeGBP ~ Sex, data=data)


#Question 7: Explore incomes by education

boxplot(incomeGBP ~ education, data=data, main="Income by Education Level", ylab="Income (GBP)", col="red")

anova_result <- aov(incomeGBP ~ factor(education), data=data)
summary(anova_result)
TukeyHSD(anova_result)


#Question 8: Explore job satisfaction by sex and educational background

# (i) Crosstab_for_Sex
table_sex <- table(data$Jobsatisfaction, data$Sex)
prop.table(table_sex, margin=2)  # Column proportions
chisq.test(table_sex)

# (ii) Crosstab_for_Education
table_edu <- table(data$Jobsatisfaction, data$education)
prop.table(table_edu, margin=2)  # Column proportions
chisq.test(table_edu)