# Install necessary packages
if (!require("tidyverse")) install.packages("tidyverse", repos = "https://cran.r-project.org")
if (!require("car")) install.packages("car", repos = "https://cran.r-project.org")

# Load libraries
library(tidyverse)
library(car)

# Load the data
data <- read.csv("./goats/neymar.csv")

# Inspect the data
print(head(data))
summary(data)

# Convert necessary columns to appropriate types
data$At_score <- as.numeric(sub(":", ".", data$At_score))  # Convert At_score to numeric
data$Minute <- as.numeric(gsub("\\D", "", data$Minute))   # Convert Minute to numeric
data <- data[!is.na(data$At_score) & !is.na(data$Minute), ]  # Remove rows with missing numeric values

# ANOVA: Grouping data by 'Competition' (categorical variable)
anova_results <- aov(At_score ~ Competition, data = data)
anova_summary <- summary(anova_results)

# Save ANOVA results to CSV
anova_table <- as.data.frame(anova_summary[[1]])  # Convert ANOVA summary to dataframe
write.csv(anova_table, "ANOVA_results_Neymar.csv")

# Regression: Minute as independent variable, At_score as dependent variable
regression_model <- lm(At_score ~ Minute, data = data)
regression_summary <- summary(regression_model)

# Save Regression results to CSV
regression_table <- as.data.frame(cbind(Estimate = coef(regression_model), confint(regression_model)))
write.csv(regression_table, "Regression_results_Neymar.csv")

# Save a cleaned version of the dataset
write.csv(data, "Cleaned_Data_Neymar.csv")

# Print results to console
print("ANOVA and regression results saved to CSV files.")
