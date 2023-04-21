credit_data <- read.csv("credit_customers.csv")


credit_data$class <- ifelse(credit_data$class == "good", 1, 0)

# split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(credit_data$class, p = 0.7, list = FALSE)
train_data <- credit_data[train_index, ]
test_data <- credit_data[-train_index, ]

# Create a logistic regression model
log_model <- glm(class ~ ., data = train_data, family = "binomial")

# Make predictions on the test data using the logistic regression model
log_pred <- predict(log_model, newdata = test_data, type = "response")

# Convert predicted probabilities to predicted classes (0 or 1) based on a threshold of 0.5
log_pred_class <- ifelse(log_pred > 0.5, "1", "0")

log_pred_class <- as.numeric(log_pred_class)

# Convert the class variable in test_data to factor with the same levels as in log_pred_class
log_pred_class <- factor(log_pred_class, levels = levels(factor(test_data$class)))
test_data$class <- factor(test_data$class, levels = levels(factor(log_pred_class)))

# Evaluate the logistic regression model using confusion matrix and other metrics
log_cm <- confusionMatrix(log_pred_class, test_data$class)
log_accuracy <- log_cm$overall[1]
log_precision <- log_cm$byClass["Pos Pred Value"]
log_recall <- log_cm$byClass["Sensitivity"]
log_f1 <- log_cm$byClass["F1"]

log_f1

library(rpart)
# Create a decision tree model
tree_model <- rpart(class ~ ., data = train_data, method = "class")

# Make predictions on the test data using the decision tree model
tree_pred <- predict(tree_model, newdata = test_data, type = "class")

# Evaluate the decision tree model using confusion matrix and other metrics
tree_cm <- confusionMatrix(tree_pred, test_data$class)
tree_accuracy <- tree_cm$overall[1]
tree_precision <- tree_cm$byClass["Pos Pred Value"]
tree_recall <- tree_cm$byClass["Sensitivity"]
tree_f1 <- tree_cm$byClass["F1"]

tree_f1




Based on the code and output provided, it appears that the project involves analyzing a dataset of credit customers and attempting to predict whether they are good or bad customers based on various features. The dataset was split into training and testing sets, and two models were created and evaluated: a logistic regression model and a decision tree model.

The logistic regression model achieved an F1 score of 0.573, while the decision tree model achieved a lower F1 score of 0.453. The logistic regression model appears to be the better-performing model overall.

However, it is important to note that the F1 score alone does not provide a complete picture of model performance, and it is important to consider other metrics such as accuracy, precision, and recall as well. It would also be beneficial to perform further analysis on the dataset to identify which features are most important in predicting good or bad customers.

Overall, the analysis presented in this code provides a good starting point for predicting credit customer behavior, but further analysis and refinement of the models may be necessary for more accurate predictions.
