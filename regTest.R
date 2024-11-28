# Load necessary libraries
library(dplyr)

# Read Messi dataset
data_messi <- read.csv("./Cleaned_Data.csv")


data_rondo <- read.csv("./Cleaned_Data_Ronaldo.csv") 

data_messi$Player <- "Messi"
data_rondo$Player <- "Ronaldo"


combined_data <- bind_rows(data_messi, data_rondo)


performance_data <- combined_data %>%
  select(Player, At_score) %>%
  filter(!is.na(At_score))

# Convert 'Player' to a factor
performance_data$Player <- as.factor(performance_data$Player)

# Perform regression analysis
regression_result <- lm(At_score ~ Player, data = performance_data)

# Display summary of the regression model
summary(regression_result)

# Save the regression results to a text file
sink("reg.txt")
summary(regression_result)
sink()

# Optional: Visualize the regression results
boxplot(At_score ~ Player, data = performance_data,
        main = "Regression Analysis: Messi vs Ronaldo",
        xlab = "Player",
        ylab = "At_scores Played",
        col = c("blue", "red"))
