# Load libraries
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
# Load required libraries
library(randomForest)
library(caret)
library(dplyr)

if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the dataset
data <- read.csv("adult.csv", header = TRUE, stringsAsFactors = TRUE)

# View the structure of the dataset
str(data)

# Clean and preprocess data
data <- na.omit(data)  # Remove rows with missing values
data$income <- as.factor(data$income)  # Convert target variable to factor

# Split the data into train and test sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$income, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Build Random Forest Model
rf_model <- randomForest(income ~ ., data = trainData, ntree = 100, importance = TRUE)

# Print the model summary
print(rf_model)

# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = testData)

# Calculate Accuracy
confusion_matrix <- confusionMatrix(rf_predictions, testData$income)
print(confusion_matrix)

# Print Accuracy
cat("Accuracy:", confusion_matrix$overall['Accuracy'], "\n")
