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
retail_data <- read.csv("retail_sales_dataset.csv")

# Clean column names (if needed)
colnames(retail_data) <- make.names(colnames(retail_data))

# ============================================================================== #
# Univariate Analysis
# ============================================================================== #

# ============================================================================== #
# Question 1 -> Data Overview
# ============================================================================== #

# Data Overview
str(retail_data)
dim(retail_data)  # Check dimensions
summary(retail_data)  # Summary of variables

# ============================================================================== #
# Question 2 -> Summary Statistics
# ============================================================================== #

# Summary statistics for numerical variables
retail_data %>%
  select(Quantity, Price.per.Unit, Total.Amount) %>%
  summary()

# ============================================================================== #
# Question 3 -> Distribution Visualization
# ============================================================================== #

# Histogram for Total Amount
ggplot(retail_data, aes(x = Total.Amount)) +
  geom_histogram(bins = 30, fill = "#4DB6AC", color = "#004D40", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Distribution of Total Amount", x = "Total Amount", y = "Frequency") +
  theme(text = element_text(size = 14))

# Boxplot for Price per Unit
ggplot(retail_data, aes(y = Price.per.Unit)) +
  geom_boxplot(fill = "#42A5F5", color = "#BF360C", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Boxplot of Price per Unit", y = "Price per Unit") +
  theme(text = element_text(size = 14))


# ============================================================================== #
# Question 4 -> Categorical Variable Analysis
# ============================================================================== #

# Bar Plot for Product Categories
ggplot(retail_data, aes(x = Product.Category)) +
  geom_bar(fill = "#42A5F5", color = "#0D47A1", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Distribution of Product Categories", x = "Product Category", y = "Count") +
  theme(text = element_text(size = 14))

# ============================================================================== #
# Multivariate Analysis
# ============================================================================== #

# ============================================================================== #
# Question 5 -> Correlation Analysis
# ============================================================================== #

# Select the variables for analysis
selected_variables <- retail_data %>% select(Quantity, Total.Amount)

# Calculate the Pearson correlation coefficient
correlation_coefficient <- cor(selected_variables$Quantity, 
                               selected_variables$Total.Amount, 
                               method = "pearson", 
                               use = "complete.obs")

# Display the result
cat("The Pearson correlation coefficient between Quantity and Total Amount is:", correlation_coefficient)

# ============================================================================== #
# Question 6 -> Scatter Plot Visualization
# ============================================================================== #

# Scatter Plot for Quantity vs Total Amount
ggplot(retail_data, aes(x = Quantity, y = Total.Amount)) +
  geom_point(color = "#1E88E5", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#D81B60", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Scatter Plot: Quantity vs Total Amount", x = "Quantity", y = "Total Amount") +
  theme(text = element_text(size = 14))

# ============================================================================== #
# Question 7 -> Multiple Regression
# ============================================================================== #

# Linear regression model
reg_model <- lm(Total.Amount ~ Quantity + Price.per.Unit, data = retail_data)
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
numerical_data <- retail_data %>% select(Quantity, Price.per.Unit, Total.Amount)
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
         groups = retail_data$Gender) +
  scale_color_manual(values = c("Male" = "lightgreen", "Female" = "blue")) +
  theme_minimal() +
  labs(title = "PCA Biplot: Retail Sales Dataset", color = "Gender") +
  theme(text = element_text(size = 14),
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "right")

