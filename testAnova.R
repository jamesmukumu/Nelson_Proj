# Load necessary library
library(tidyverse)

# Read the dataset
data <- read.csv("")

# Add a column to distinguish between Messi and Ronaldo data
# Assuming the file includes rows for both players in separate groups
data$Player <- factor(data$Player) # Make sure there is a 'Player' column

# Check the structure of the dataset
str(data)

# Perform ANOVA test on a specific performance metric (e.g., 'Goals', 'Assists')
# Replace 'Metric' with the actual column name you want to analyze
anova_results <- aov(Metric ~ Player, data = data)

# Print summary of ANOVA
summary(anova_results)

# Optional: Perform a Tukey HSD post-hoc test if ANOVA is significant
tukey_results <- TukeyHSD(anova_results)
print(tukey_results)

# Save results to a text file
sink("anova_results.txt")
cat("ANOVA Results:\n")
print(summary(anova_results))
cat("\nTukey HSD Test Results:\n")
print(tukey_results)
sink()
