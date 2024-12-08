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
employee_data <- read.csv("Employee.csv")

# ______________________________________________________________________________ #
# ============================================================================== #
# Univariate Analysis
# ============================================================================== #
# ______________________________________________________________________________ #

# ============================================================================== #
# Question 1 -> Data Overview
# ============================================================================== #

# Data Overview
str(employee_data)
dim(employee_data)  # Check dimensions
summary(employee_data)  # Summary of variables

# ============================================================================== #
# Question 2 -> Summary Statistics
# ============================================================================== #

# Summary statistics for numerical variables
employee_data %>%
  select(JoiningYear, PaymentTier, Age, ExperienceInCurrentDomain, LeaveOrNot) %>%
  summary()

# ============================================================================== #
# Question 3 -> Distribution Visualization
# ============================================================================== #

# Histogram for Age
ggplot(employee_data, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "#4DB6AC", color = "#004D40", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency") +
  theme(text = element_text(size = 14))

# Boxplot for ExperienceInCurrentDomain
ggplot(employee_data, aes(y = ExperienceInCurrentDomain)) +
  geom_boxplot(fill = "#42A5F5", color = "#BF360C", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Boxplot of Experience in Current Domain", y = "Experience (Years)") +
  theme(text = element_text(size = 14))


# ============================================================================== #
# Question 4 -> Categorical Variable Analysis
# ============================================================================== #

# Bar Plot for Education
ggplot(employee_data, aes(x = Education)) +
  geom_bar(fill = "#42A5F5", color = "#0D47A1", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Distribution of Education Levels", x = "Education", y = "Count") +
  theme(text = element_text(size = 14))

# ______________________________________________________________________________ #
# ============================================================================== #
# Multivariate Analysis
# ============================================================================== #
# ______________________________________________________________________________ #

# ============================================================================== #
# Question 5 -> Correlation Analysis
# ============================================================================== #

# Select the variables for analysis
selected_variables <- employee_data %>% select(ExperienceInCurrentDomain, Age)

# Calculate the Pearson correlation coefficient
correlation_coefficient <- cor(selected_variables$ExperienceInCurrentDomain, 
                               selected_variables$Age, 
                               method = "pearson", 
                               use = "complete.obs")

# Display the result
cat("The Pearson correlation coefficient between ExperienceInCurrentDomain and Age is:", correlation_coefficient)

# ============================================================================== #
# Question 6 -> Scatter Plot Visualization
# ============================================================================== #

# Scatter Plot for Age vs Experience
ggplot(employee_data, aes(x = Age, y = ExperienceInCurrentDomain)) +
  geom_point(color = "#1E88E5", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#D81B60", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Scatter Plot: Age vs Experience in Current Domain", x = "Age", y = "Experience (Years)") +
  theme(text = element_text(size = 14))


# ============================================================================== #
# Question 7 -> Multiple Regression
# ============================================================================== #

# Linear regression model
reg_model <- lm(LeaveOrNot ~ Age + PaymentTier + ExperienceInCurrentDomain, data = employee_data)
summary(reg_model)


# ============================================================================== #
# Question 8 -> Model Diagnostics
# ============================================================================== #

# Residuals vs Fitted
plot(reg_model, which = 1, col = "#FF9800", main = "Residuals vs Fitted")

# Normal Q-Q Plot
plot(reg_model, which = 2, col = "#8E24AA", main = "Normal Q-Q Plot")

# ______________________________________________________________________________ #
# ============================================================================== #
# Advanced Analysis
# ============================================================================== #
# ______________________________________________________________________________ #

# ============================================================================== #
# Question 9 -> Principal Component Analysis (PCA)
# ============================================================================== #

# PCA analysis
numerical_data <- employee_data %>% select(JoiningYear, PaymentTier, Age, ExperienceInCurrentDomain, LeaveOrNot)
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
         groups = employee_data$Gender) +
  scale_color_manual(values = c("Female" = "lightgreen", "Male" = "grey")) +
  theme_minimal() +
  labs(title = "PCA Biplot: Employee Dataset", color = "Gender") +
  theme(text = element_text(size = 14),
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "right")


