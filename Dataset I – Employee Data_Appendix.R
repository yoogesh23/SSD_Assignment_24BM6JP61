# Load libraries
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")

# Load necessary libraries
library(randomForest)  # For Random Forest
library(caret)         # For data splitting and evaluation

if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the dataset
df <- read.csv("Employee.csv")

# Label Encoding for Categorical Variables
df$Gender <- as.numeric(factor(df$Gender))
df$EverBenched <- as.numeric(factor(df$EverBenched))
df$Education <- as.numeric(factor(df$Education))
df$City <- as.numeric(factor(df$City))

# Define Features and Target
X <- df %>% select(-LeaveOrNot)
y <- df$LeaveOrNot

# Train-test split
set.seed(42)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Model Training (Random Forest Classifier)
rf_model <- randomForest(as.factor(y_train) ~ ., data = X_train, ntree = 100)

# Predictions
y_pred <- predict(rf_model, X_test)

# Calculate Accuracy
conf_matrix <- confusionMatrix(as.factor(y_pred), as.factor(y_test))
accuracy <- conf_matrix$overall['Accuracy']
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
