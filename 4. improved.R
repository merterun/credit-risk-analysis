# Load required libraries
library(tidyverse)
library(caret)

# Read in data
credit_data <- read_csv("credit_customers.csv")

# Convert the class variable to a binary numeric variable
credit_data$class <- ifelse(credit_data$class == "good", 1, 0)

# Split data into training and testing sets
set.seed(123)
split <- sample.split(credit_data$class, SplitRatio = 0.7)
train_data <- subset(credit_data, split == TRUE)
test_data <- subset(credit_data, split == FALSE)

# Create a logistic regression model
log_model <- train(class ~ ., 
                   data = train_data, 
                   method = "glm",
                   family = "binomial",
                   trControl = trainControl(method = "cv"))

# Make predictions on the test data using the logistic regression model
log_pred <- predict(log_model, newdata = test_data)

# Evaluate the logistic regression model using confusion matrix and other metrics
log_cm <- confusionMatrix(log_pred, test_data$class)
log_accuracy <- log_cm$overall[1]
log_precision <- log_cm$byClass["Pos Pred Value"]
log_recall <- log_cm$byClass["Sensitivity"]
log_f1 <- log_cm$byClass["F1"]

# Print logistic regression model metrics
cat("Logistic Regression Metrics:\n")
cat(paste0("Accuracy: ", log_accuracy, "\n"))
cat(paste0("Precision: ", log_precision, "\n"))
cat(paste0("Recall: ", log_recall, "\n"))
cat(paste0("F1 Score: ", log_f1, "\n"))

# Create a decision tree model
tree_model <- train(class ~ ., 
                    data = train_data, 
                    method = "rpart")

# Make predictions on the test data using the decision tree model
tree_pred <- predict(tree_model, newdata = test_data)

# Evaluate the decision tree model using confusion matrix and other metrics
tree_cm <- confusionMatrix(tree_pred, test_data$class)
tree_accuracy <- tree_cm$overall[1]
tree_precision <- tree_cm$byClass["Pos Pred Value"]
tree_recall <- tree_cm$byClass["Sensitivity"]
tree_f1 <- tree_cm$byClass["F1"]

# Print decision tree model metrics
cat("\nDecision Tree Metrics:\n")
cat(paste0("Accuracy: ", tree_accuracy, "\n"))
cat(paste0("Precision: ", tree_precision, "\n"))
cat(paste0("Recall: ", tree_recall, "\n"))
cat(paste0("F1 Score: ", tree_f1, "\n"))
