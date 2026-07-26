# Load necessary packages
library(NHANES)     # NHANES dataset
library(dplyr)      # Data manipulation
library(ggplot2)    # Data visualization
library(summarytools) # Detailed descriptive statistics

# Load the NHANES dataset
data("NHANES")

# View structure of the dataset
str(NHANES)

# Glimpse of the first few rows
head(NHANES)

# Select relevant variables for analysis
nhanes_subset <- NHANES %>%
  select(Age, Gender, BMI, HealthGen, SleepHrsNight, Depressed, PhysActive)

# Drop rows with missing values
nhanes_clean <- na.omit(nhanes_subset)

# Summary statistics
summary(nhanes_clean)

# Detailed descriptive stats using summarytools
dfSummary(nhanes_clean)

# Distribution of Age
ggplot(nhanes_clean, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# Boxplot of BMI by Gender
ggplot(nhanes_clean, aes(x = Gender, y = BMI, fill = Gender)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "BMI by Gender", x = "Gender", y = "BMI")

# Bar plot: Physical Activity Status
ggplot(nhanes_clean, aes(x = PhysActive)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Physical Activity Status", x = "Physically Active?", y = "Count")

# Relationship between Sleep Hours and Depression
ggplot(nhanes_clean, aes(x = SleepHrsNight, fill = Depressed)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Sleep Hours by Depression Status", x = "Sleep Hours", y = "Count")

# Correlation matrix (numeric variables only)
cor_data <- nhanes_clean %>%
  select_if(is.numeric)

cor_matrix <- cor(cor_data, use = "complete.obs")
print(cor_matrix)
