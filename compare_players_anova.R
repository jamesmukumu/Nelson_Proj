library(dplyr)
data_messi <- read.csv("./Cleaned_Data.csv")
data_rondo <- read.csv("./Cleaned_Data_Ronaldo.csv") 
data_neymar <- read.csv("./Cleaned_Data_Neymar.csv")
data_messi$Player <- "Messi"
data_rondo$Player <- "Ronaldo"
data_neymar$Player <- "Neymar"
combined_data <- bind_rows(data_messi, data_rondo,data_neymar)
performance_data <- combined_data %>% 
  select(Player, At_score) %>% 
  filter(!is.na(At_score))
anova_result <- aov(At_score ~ Player, data = performance_data)
summary(anova_result)
sink("anova_results_at_score.txt")
summary(anova_result)
sink()

boxplot(At_score ~ Player, data = performance_data, 
        main = "Comparison of Performance: Messi vs Ronaldo vs Neymar",
        xlab = "Player",
        ylab = "Playing",
        col = c("blue", "red","yellow"))



# Load necessary libraries
library(dplyr)

# Load the data
data_messi <- read.csv("./Cleaned_Data.csv")
data_rondo <- read.csv("./Cleaned_Data_Ronaldo.csv")
data_neymar <- read.csv("./Cleaned_Data_Neymar.csv")

# Add Player column
data_messi$Player <- "Messi"
data_rondo$Player <- "Ronaldo"
data_neymar$Player <- "Neymar"

# Combine the data
combined_data <- bind_rows(data_messi, data_rondo, data_neymar)

# Create a contingency table for Player and Position
contingency_table <- table(combined_data$Player, combined_data$Position)

# Perform the chi-square test of independence
chi_square_result <- chisq.test(contingency_table)

# Output the results to a file
sink("chi_square_results_position.txt")
print(chi_square_result)
sink()

# Visualize the data using a bar plot
barplot(contingency_table,
        beside = TRUE,
        legend = TRUE,
        col = c("blue", "red", "yellow"),
        main = "Distribution of Positions: Messi vs Ronaldo vs Neymar",
        xlab = "Position",
        ylab = "Frequency")
