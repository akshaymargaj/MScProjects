getwd()

setwd("D:/MSc Assignments/DataVisual/Assign1")

library(ggplot2)
employees <- read.csv("Employeesatisfaction.csv")

# ----------------------------------Question1-------------------------------------
  
employees$Age_Group <- cut(employees$Age, breaks = c(17, 25, 35, 45, 55, 61),
                           labels = c("18–25 years", "26–35 years", "36–45 years", "46–55 years", "56–60 years"),
                           right = FALSE)
age_summary <- as.data.frame(table(employees$Age_Group))
names(age_summary) <- c("Age_Group", "Count")

age_summary$Percentage <- round(age_summary$Count / sum(age_summary$Count) * 100, 1)

ggplot(age_summary, aes(x = Age_Group, y = Count, fill = Age_Group)) + geom_col(colour = "black") +
  geom_text(aes(label = paste(Count, "\n(", Percentage, "%)")), size = 3.5) +
  scale_fill_manual(values = c("#ffffcc", "deepskyblue","#00bfff", "#87cefa", "lightyellow")) +
  labs(title = "Distribution of Employee's by Age Groups", x = "Age Group", y = "No. of Employees") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")
  
  

------------------------------------------------------------------
  
  ggplot(employees, aes(x = incomeGBP, fill = ..count..)) +
  geom_histogram(breaks = seq(0, 10000, by = 1000), colour = "black") +
  geom_text(stat = "bin", breaks = seq(0, 10000, by = 1000), aes(label = ..count..), vjust = -0.5, size = 3) +
  scale_fill_gradient(low = "lightyellow", high = "skyblue", name = "EmpCount") +
  scale_x_continuous(breaks = seq(0, 10000, by = 1000)) +
  labs(title = "Distribution of Employees Monthly Incomes", x = "Monthly Income (GBP)", y = "No. of Employees") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))


---------------------------------------------------------------------
  
  ggplot(employees, aes(x = distance, fill = ..count..)) +
  geom_histogram(breaks = seq(0, 30, by = 5), colour = "black") +
  geom_text(stat = "bin", breaks = seq(0, 30, by = 5), aes(label = ..count..), vjust = -0.5, size = 3.5) +
  scale_fill_gradient(low = "lightyellow", high = "skyblue", name = "EmpCount") +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) + 
  labs(title = "Distribution of Employee Travel Distances to Work", x = "Distance (miles)", y = "No. of Employees") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  

---------------------------------------------------------------------
  
  counts <- table(employees$education)

# Percentages
percent <- round(prop.table(counts) * 100, 1)

edu_summary <- data.frame(
  Level = 1:5,
  Description = c("No formal qualifications", "Minimum 5 GCSEs", "A level or equivalent",
                  "HE degree", "Post degree qualification"),
  Count = as.vector(counts), "Percentage (%)" = as.vector(percent))
edu_summary <- rbind(edu_summary, c("Total", "", sum(edu_summary$Count), 100.0))
print(edu_summary, row.names = FALSE)


---------------------------------------------------------------------
  
  
  ggplot(employees, aes(x = Age, y = incomeGBP)) +
  geom_point(alpha = 0.3, colour = "steelblue", size = 2) +
  geom_smooth(method = "lm", colour = "red", se = FALSE, linetype = "dashed", size = 1.2) + 
  labs(title = "Relationship Between Monthly Income and Age",
       x = "Age (in years)", y = "Monthly Income (GBP)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))



----------------------------------------------------------------------
  
  
  ggplot(employees, aes(x = Sex, y = incomeGBP, fill = Sex)) +
  geom_boxplot(colour = "black") +
  scale_fill_manual(values = c("Female" = "lightpink", "Male" = "lightblue")) +
  labs(title = "Monthly Income by Sex", x = "Sex", y = "Monthly Income (GBP)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

t.test(incomeGBP ~ Sex, data = employees)



----------------------------------------------------------------------
  
employees$Edu_Level <- factor(employees$education,
                                levels = 1:5,
                                labels = c("1: No qualifications",
                                           "2: Minimum 5 GCSEs",
                                           "3: A level or equivalent",
                                           "4: HE degree",
                                           "5: Post-degree"))

ggplot(employees, aes(x = factor(education), y = incomeGBP, fill = Edu_Level)) +
  geom_boxplot(colour = "black") + 
  scale_fill_manual(values = c("lightyellow", "lightgreen", "skyblue", "orange", "lightpink"),
  name = "Education Level") + labs(title = "Monthly Income Distribution by Education Level",
  x = "Education Level", y = "Monthly Income (GBP)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "right")

# ANOVA
income_aov <- aov(incomeGBP ~ factor(education), data = employees)
summary(income_aov)

# Post-hoc Tukey HSD
TukeyHSD(income_aov)


------------------------------------------------------------------------

# (i) Job Satisfaction by Sex

tab_sex <- table(employees$Jobsatisfaction, employees$Sex)

rownames(tab_sex) <- c("Not satisfied", "Neutral", "Satisfied", "Very Satisfied")

print(tab_sex)

prop_sex <- round(prop.table(tab_sex, margin = 2) * 100, 1)
cat("\nColumn Percentages (%):\n")
print(prop_sex)

cat("\nChi-square Test for Sex:\n")
print(chisq.test(tab_sex))


# (ii) Job Satisfaction by Education

tab_edu <- table(employees$Jobsatisfaction, employees$education)

rownames(tab_edu) <- c("Not satisfied", "Neutral", "Satisfied", "Very Satisfied")
colnames(tab_edu) <- c("No formal qualifications", "Minimum 5 GCSEs", "A level or equivalent", 
                       "HE degree", "Post degree qualification")
print(tab_edu)

prop_edu <- round(prop.table(tab_edu, margin = 2) * 100, 1)
cat("\nColumn Percentages (%):\n")
print(prop_edu)

cat("\nChi-square Test for Education:\n")
print(chisq.test(tab_edu))