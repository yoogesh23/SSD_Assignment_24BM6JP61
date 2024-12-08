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
car_data <- read.csv("CarPrice_Assignment.csv")

# Clean column names (if needed)
colnames(car_data) <- make.names(colnames(car_data))

# ============================================================================== #
# Univariate Analysis
# ============================================================================== #

# ============================================================================== #
# Question 1 -> Data Overview
# ============================================================================== #

# Data Overview
str(car_data)
dim(car_data)  # Check dimensions
summary(car_data)  # Summary of variables

# ============================================================================== #
# Question 2 -> Summary Statistics
# ============================================================================== #

# Summary statistics for numerical variables
car_data %>%
  select(wheelbase, carlength, carwidth, curbweight, enginesize, horsepower, price) %>%
  summary()

# ============================================================================== #
# Question 3 -> Distribution Visualization
# ============================================================================== #

# Histogram for Car Width
ggplot(car_data, aes(x = carwidth)) +
  geom_histogram(bins = 30, fill = "#4DB6AC", color = "#004D40", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Distribution of Car Width", x = "Car Width", y = "Frequency") +
  theme(text = element_text(size = 14))

# Boxplot for Price
ggplot(car_data, aes(y = price)) +
  geom_boxplot(fill = "#42A5F5", color = "#BF360C", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Boxplot of Car Prices", y = "Price") +
  theme(text = element_text(size = 14))


# ============================================================================== #
# Question 4 -> Categorical Variable Analysis
# ============================================================================== #

# Bar Plot for Car Body Types
ggplot(car_data, aes(x = carbody)) +
  geom_bar(fill = "#42A5F5", color = "#0D47A1", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Distribution of Car Body Types", x = "Car Body", y = "Count") +
  theme(text = element_text(size = 14))

# ============================================================================== #
# Multivariate Analysis
# ============================================================================== #

# ============================================================================== #
# Question 5 -> Correlation Analysis
# ============================================================================== #

# Select variables for analysis
selected_variables <- car_data %>% select(enginesize, price)

# Calculate the Pearson correlation coefficient
correlation_coefficient <- cor(selected_variables$enginesize, 
                               selected_variables$price, 
                               method = "pearson", 
                               use = "complete.obs")

# Display the result
cat("The Pearson correlation coefficient between Engine Size and Price is:", correlation_coefficient)

# ============================================================================== #
# Question 6 -> Scatter Plot Visualization
# ============================================================================== #

# Scatter Plot for Engine Size vs Price
ggplot(car_data, aes(x = enginesize, y = price)) +
  geom_point(color = "#1E88E5", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#D81B60", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Scatter Plot: Engine Size vs Price", x = "Engine Size", y = "Price") +
  theme(text = element_text(size = 14))

# ============================================================================== #
# Question 7 -> Multiple Regression
# ============================================================================== #

# Linear regression model
reg_model <- lm(price ~ enginesize + curbweight + horsepower + carwidth + carlength, data = car_data)
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
numerical_data <- car_data %>% select(wheelbase, carlength, carwidth, curbweight, enginesize, horsepower, price)
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
         groups = car_data$carbody) +
  scale_color_manual(values = c("sedan" = "lightgreen", "hatchback" = "blue", "convertible" = "orange")) +
  theme_minimal() +
  labs(title = "PCA Biplot: Car Price Dataset", color = "Car Body Type") +
  theme(text = element_text(size = 14),
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "right")
