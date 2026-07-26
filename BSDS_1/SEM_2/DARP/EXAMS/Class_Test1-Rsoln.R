# Load libraries
library(dplyr)
library(ggplot2)

# Load data
rcs2024 <- read.csv("https://deepayan.github.io/BSDS/2025-01-DARP/slides/data/rcs2024.csv")

# --- Step 1: Clean the data ---
rcs2024 <- rcs2024 %>%
  mutate(
    sex_of_driver = na_if(sex_of_driver, -1),
    age_of_driver = na_if(age_of_driver, -1),
    driver_sex = factor(sex_of_driver, labels = c("Male", "Female", "Not Known"))
  )

# --- Step 2: Find collision involving 25 vehicles ---
coll25 <- rcs2024 %>%
  count(collision_index) %>%
  filter(n == 25) %>%
  pull(collision_index)

dsub25 <- rcs2024 %>%
  filter(collision_index == coll25)

# Number of drivers by sex
dsub25 %>%
  group_by(driver_sex) %>%
  summarise(n_drivers = n(), .groups = "drop") %>%
  print()

# Average age of drivers by sex
dsub25 %>%
  group_by(driver_sex) %>%
  summarise(avg_age = mean(age_of_driver, na.rm = TRUE), .groups = "drop") %>%
  print()

# --- Step 3: Subset of collisions with exactly one vehicle ---
vars <- c("vehicle_type", "driver_sex", "age_of_driver")

coll1 <- rcs2024 %>%
  count(collision_index) %>%
  filter(n == 1) %>%
  pull(collision_index)

dsub1 <- rcs2024 %>%
  filter(collision_index %in% coll1) %>%
  select(all_of(vars))

# --- Step 4: Summaries for collisions with 1 vehicle ---
# Number of such collisions
nrow(dsub1)

# Proportion with missing driver age
mean(is.na(dsub1$age_of_driver))

# Proportion with driver sex "Not Known"
mean(dsub1$driver_sex == "Not Known", na.rm = TRUE)

# Average age by driver sex
dsub1 %>%
  group_by(driver_sex) %>%
  summarise(avg_age = mean(age_of_driver, na.rm = TRUE), .groups = "drop") %>%
  print()

# Vehicle type frequency (sorted)
dsub1 %>%
  count(vehicle_type) %>%
  arrange(n) %>%
  print()

# --- Step 5: Graphical Analysis ---

# 5.1 Boxplot (separated by whether age > 40)
dsub1 %>%
  mutate(age_group = ifelse(age_of_driver > 40, ">40", "<=40")) %>%
  ggplot(aes(x = driver_sex, y = age_of_driver, fill = driver_sex)) +
  geom_boxplot() +
  facet_wrap(~ age_group) +
  labs(title = "Driver Age vs Sex, Split by Age > 40", x = "Driver Sex", y = "Age of Driver") +
  theme_minimal()

# 5.2 Violin plot
ggplot(dsub1, aes(x = driver_sex, y = age_of_driver, fill = driver_sex)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Age by Driver Sex", x = "Driver Sex", y = "Age of Driver") +
  theme_minimal()

# 5.3 Stripplot (jitter plot)
ggplot(dsub1, aes(x = driver_sex, y = age_of_driver, color = driver_sex)) +
  geom_jitter(width = 0.2, alpha = 0.7) +
  labs(title = "Stripplot of Driver Age by Sex", x = "Driver Sex", y = "Age of Driver") +
  theme_minimal()

# 5.4 QQ plot comparing to Exponential distribution (only for age > 40)
rcs2024 %>%
  filter(!is.na(age_of_driver), age_of_driver > 40) %>%
  ggplot(aes(sample = age_of_driver, color = driver_sex)) +
  stat_qq(distribution = qexp) +
  stat_qq_line(distribution = qexp) +
  labs(title = "QQ Plot (Age > 40) vs Exponential Distribution", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# 5.5 Density plot
ggplot(rcs2024, aes(x = age_of_driver, color = driver_sex, fill = driver_sex)) +
  geom_density(alpha = 0.3, na.rm = TRUE) +
  labs(title = "Density Plot of Driver Age by Sex", x = "Age of Driver", y = "Density") +
  theme_minimal()

# 5.6 Histogram by driver sex
ggplot(rcs2024, aes(x = age_of_driver, fill = driver_sex)) +
  geom_histogram(binwidth = 5, position = "dodge", na.rm = TRUE) +
  facet_wrap(~ driver_sex) +
  labs(title = "Histogram of Driver Age by Sex", x = "Age of Driver", y = "Count") +
  theme_minimal()
