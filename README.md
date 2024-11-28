Choosen goat - Messi
People to compare with -  Ronaldo,Neymar and Lewandowski

Solutions According to each Step
- Building own data table from Anova and regression tests

R Code which i used to convert each players dataset to my own as required
This is for the ANOVA Tst
```r

if (!require("tidyverse")) install.packages("tidyverse", repos = "https://cran.r-project.org")
if (!require("car")) install.packages("car", repos = "https://cran.r-project.org")
library(tidyverse)
library(car)
data <- read.csv("./goats/neymar.csv")
print(head(data))
summary(data)

data$At_score <- as.numeric(sub(":", ".", data$At_score))  #
data$Minute <- as.numeric(gsub("\\D", "", data$Minute))   # Convert Minute to numeric
data <- data[!is.na(data$At_score) & !is.na(data$Minute), ]  # Remove rows with missing numeric values


anova_results <- aov(At_score ~ Competition, data = data)
anova_summary <- summary(anova_results)


anova_table <- as.data.frame(anova_summary[[1]]) 
write.csv(anova_table, "ANOVA_results_Neymar.csv")


regression_model <- lm(At_score ~ Minute, data = data)
regression_summary <- summary(regression_model)


regression_table <- as.data.frame(cbind(Estimate = coef(regression_model), confint(regression_model)))
write.csv(regression_table, "Regression_results_Neymar.csv")


write.csv(data, "Cleaned_Data_Neymar.csv")


print("ANOVA and regression results saved to CSV files.")

```

Anova Test 
R script code for this test
```r
library(dplyr)
data_messi <- read.csv("./Cleaned_Data.csv")
data_rondo <- read.csv("./Cleaned_Data_Ronaldo.csv") 
data_neymar <- read.csv("./Cleaned_Data_Neymar.csv")
data_messi$Player <- "Messi"
data_rondo$Player <- "Ronaldo"
data_neymar$Player <- "Neymar"
combined_data <- bind_rows(data_messi, data_rondo,data_neymar)
performance_data <- combined_data %>% 
  select(Player, Minute) %>% 
  filter(!is.na(Minute))
anova_result <- aov(Minute ~ Player, data = performance_data)
summary(anova_result)
sink("anova_results.txt")
summary(anova_result)
sink()

boxplot(Minute ~ Player, data = performance_data, 
        main = "Comparison of Performance: Messi vs Ronaldo vs Neymar",
        xlab = "Player",
        ylab = "Minutes Played",
        col = c("blue", "red","yellow"))

```
Here we run the anova results in a anova_results.txt file  and also create a visual illustration box plot
Results summary of this test(ANOVA)
- Terms of minutes Played

The ANOVA results for comparing the performance of Messi, Ronaldo, and Neymar based on minutes played indicate the following:
The analysis of variance (ANOVA) tested the null hypothesis that there is no significant difference in the mean minutes played among the three players. The F-statistic for the test was 0.604, with a p-value of 0.547. Since the p-value is much greater than the common significance level of 0.05, we fail to reject the null hypothesis. This suggests that there is no statistically significant difference in the average minutes played between Messi, Ronaldo, and Neymar in the provided dataset.
Additionally, the boxplot visualization allows for a visual comparison of the distributions of minutes played by each player, but the statistical analysis confirms that any observed differences are not significant.
Terms of minutes goal scored
The ANOVA analysis aimed to examine whether there is a significant difference in performance scores (At_score) among three players: Messi, Ronaldo, and Neymar. The results of the one-way ANOVA are summarized in the table.
The analysis yielded an F-value of 1.975 with 1 degree of freedom for the "Player" variable and 1412 degrees of freedom for residuals. The corresponding p-value is 0.16. Since the p-value exceeds the standard significance threshold of 0.05, we fail to reject the null hypothesis. This indicates that there is no statistically significant difference in the performance scores among the three players at the chosen significance level.
The boxplot further visualizes the distribution of performance scores for Messi, Ronaldo, and Neymar, showing overlapping ranges, which aligns with the ANOVA results. These findings suggest that the differences in mean performance scores across the three players are not substantial enough to be considered statistically significant.
Regression Results Summary
```r
# Load necessary libraries
library(dplyr)

# Read Messi dataset
data_messi <- read.csv("./Cleaned_Data.csv")


data_rondo <- read.csv("./Cleaned_Data_Ronaldo.csv") 

data_messi$Player <- "Messi"
data_rondo$Player <- "Ronaldo"


combined_data <- bind_rows(data_messi, data_rondo)


performance_data <- combined_data %>%
  select(Player, Minute) %>%
  filter(!is.na(Minute))

# Convert 'Player' to a factor
performance_data$Player <- as.factor(performance_data$Player)

# Perform regression analysis
regression_result <- lm(Minute ~ Player, data = performance_data)

# Display summary of the regression model
summary(regression_result)

# Save the regression results to a text file
sink("reg.txt")
summary(regression_result)
sink()

# Optional: Visualize the regression results
boxplot(Minute ~ Player, data = performance_data,
        main = "Regression Analysis: Messi vs Ronaldo",
        xlab = "Player",
        ylab = "Minutes Played",
        col = c("blue", "red"))

```
Summary in terms of Minutes Played
The regression analysis examined the relationship between the number of minutes played (Minute) and the player (Player: Messi or Ronaldo). The model had an intercept estimate of 86.44, which represents the average minutes played by Messi (the reference group). The coefficient for Ronaldo (-6.674) indicates that Ronaldo plays, on average, approximately 6.67 fewer minutes than Messi, though this difference was not statistically significant (p = 0.439).
The residual standard error was 162 minutes, suggesting considerable variability in minutes played within and across players. The R-squared value was 0.000425, indicating that the player variable explains only 0.04% of the variance in minutes played. The F-statistic (0.6003, p = 0.4386) further confirms that the model does not significantly explain the variation in minutes played. Overall, the regression results suggest that the difference in playing minutes between Messi and Ronaldo is minimal and not statistically significant.
Summary in terms of At_Score
This regression analysis examines the difference in "At_score" between two players, Messi and Ronaldo, based on the `Player` factor. The model’s intercept (representing Messi) has an estimated score of 1.64243 with a high level of statistical significance (p < 2e-16). The coefficient for Ronaldo (-0.10992) suggests a slightly lower average "At_score" compared to Messi, but this difference is not statistically significant (p = 0.16). The residuals range from -1.6324 to 7.4775, indicating some variability in the data. The model's residual standard error is 1.471, and the R-squared value is very low (0.0014), indicating the model explains little of the variation in "At_score". The overall F-statistic (1.975) and its associated p-value (0.1602) further confirm that the difference between the two players is not statistically significant. This suggests that player identity does not strongly influence "At_score" in this dataset.