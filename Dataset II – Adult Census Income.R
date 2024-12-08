# ============================================================================== #
# Numerical Assignment: Exploration of Univariate and Multivariate Data Using R
# ============================================================================== #

# Name :  Yoogesh Kumar M
# Roll No: 24BM6JP61

# ============================================================================== #
# Initial Setup
# ============================================================================== #
# Load libraries
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("ggbiplot", quietly = TRUE)) {
  devtools::install_github("vqv/ggbiplot")
}

library(dplyr)
library(ggplot2)
library(corrplot)
library(devtools)
library(ggbiplot)

# Set working directory
if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
census_data <- read.csv("adult.csv")

# Clean column names (if needed)
colnames(census_data) <- make.names(colnames(census_data))

# ============================================================================== #
# Univariate Analysis
# ============================================================================== #

# ============================================================================== #
# Question 1 -> Data Overview
# ============================================================================== #

# Data Overview
str(census_data)
dim(census_data)  # Check dimensions
summary(census_data)  # Summary of variables

# ============================================================================== #
# Question 2 -> Summary Statistics
# ============================================================================== #

# Summary statistics for numerical variables
census_data %>%
  dplyr::select(age, fnlwgt, education.num, capital.gain, capital.loss, hours.per.week) %>%
  summary()

# ============================================================================== #
# Question 3 -> Distribution Visualization
# ============================================================================== #

# Histogram for Age
ggplot(census_data, aes(x = age)) +
  geom_histogram(bins = 30, fill = "#4DB6AC", color = "#004D40", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency") +
  theme(text = element_text(size = 14))

# Boxplot for Hours Per Week
ggplot(census_data, aes(y = hours.per.week)) +
  geom_boxplot(fill = "#42A5F5", color = "#BF360C", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Boxplot of Hours per Week", y = "Hours per Week") +
  theme(text = element_text(size = 14))


# ============================================================================== #
# Question 4 -> Categorical Variable Analysis
# ============================================================================== #

# Bar Plot for Workclass
ggplot(census_data, aes(x = workclass)) +
  geom_bar(fill = "#42A5F5", color = "#0D47A1", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Distribution of Workclass", x = "Workclass", y = "Count") +
  theme(text = element_text(size = 14))

# ============================================================================== #
# Multivariate Analysis
# ============================================================================== #

# ============================================================================== #
# Question 5 -> Correlation Analysis
# ============================================================================== #

# Correlation Matrix
# Select two numerical variables: age and hours.per.week
# Explicitly call dplyr::select to avoid masking
selected_variables <- census_data %>%
  dplyr::select(age, hours.per.week)

# Calculate the Pearson correlation coefficient
correlation_coefficient <- cor(selected_variables$age, selected_variables$hours.per.week, use = "complete.obs")

# Display the result
cat("The Pearson correlation coefficient between Age and Hours per Week is:", correlation_coefficient)

# ============================================================================== #
# Question 6 -> Scatter Plot Visualization
# ============================================================================== #

# Scatter Plot for Hours per Week vs Age
ggplot(census_data, aes(x = age, y = hours.per.week)) +
  geom_point(color = "#1E88E5", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#D81B60", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Scatter Plot: Age vs Hours per Week", x = "Age", y = "Hours per Week") +
  theme(text = element_text(size = 14))

# ============================================================================== #
# Question 7 -> Multiple Regression
# ============================================================================== #

# Convert income to binary numeric values for regression
# For example: "<=50K" -> 0, ">50K" -> 1
census_data$income_binary <- ifelse(census_data$income == ">50K", 1, 0)

# Ensure there are no NA/NaN/Inf in the predictors or the response
census_data_clean <- census_data %>%
  filter(!is.na(age), !is.na(fnlwgt), !is.na(education.num), 
         !is.na(capital.gain), !is.na(capital.loss), !is.na(hours.per.week), 
         !is.na(income_binary))

# Linear regression model
reg_model <- lm(income_binary ~ age + fnlwgt + education.num + capital.gain + capital.loss + hours.per.week, 
                data = census_data_clean)

# Display summary of the regression model
summary(reg_model)

# ============================================================================== #
# Question 8 -> Model Diagnostics
# ============================================================================== #

# Residuals vs Fitted
plot(reg_model, which = 1, col = "#FF9800", main = "Residuals vs Fitted")

# Normal Q-Q Plot
plot(reg_model, which = 2, col = "#8E24AA", main = "Normal Q-Q Plot")

# ============================================================================== #
# Advanced Analysis
# ============================================================================== #

# ============================================================================== #
# Question 9 -> Principal Component Analysis (PCA)
# ============================================================================== #

# PCA analysis
numerical_data <- census_data_clean %>% dplyr::select(age, fnlwgt, education.num, capital.gain, capital.loss, hours.per.week)
pca_result <- prcomp(numerical_data, scale. = TRUE)

# Scree plot
plot(pca_result, type = "l", main = "Scree Plot", col = "#43A047", lwd = 2)

# Explained variance
summary(pca_result)

# ============================================================================== #
# Question 10 -> PCA Interpretation
# ============================================================================== #

# PCA Biplot Focusing on Variables
ggbiplot(pca_result,
         obs.scale = 1,
         var.scale = 1,
         ellipse = TRUE,
         circle = TRUE,
         var.axes = TRUE,
         groups = census_data_clean$income) +
  scale_color_manual(values = c("<=50K" = "lightgreen", ">50K" = "grey")) +
  theme_minimal() +
  labs(title = "PCA Biplot: Adult Census Income Dataset", color = "Income Level") +
  theme(text = element_text(size = 14),
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "right")
