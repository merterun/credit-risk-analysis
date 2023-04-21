# Load the necessary packages
library(tidyverse)
library(gridExtra)
library(cowplot)
library(lubridate)
library(GGally)
library(pROC)


# Read in the data
credit_data <- read.csv("credit_customers.csv")

# Explore the data
summary(credit_data)
str(credit_data)

## Remove "own_telephone" column, there are more NA values than the other
credit_data <- credit_data[, names(credit_data) != "own_telephone"]


# Clean the data
credit_data$checking_status <- factor(credit_data$checking_status, levels = c("no checking", "<0", "0<=X<200", ">=200"))
credit_data$savings_status <- factor(credit_data$savings_status, levels = c("no known savings", "<100", "100<=X<500", "500<=X<1000", ">=1000"))
credit_data$credit_history <- factor(credit_data$credit_history, levels = c("no credits taken", "all credits paid back duly", "existing paid", "delayed previously", "critical/other existing credit"))
credit_data$purpose <- factor(credit_data$purpose, levels = c("new car", "used car", "furniture/equipment", "radio/tv", "domestic appliance", "repairs", "education", "vacation", "retraining", "business", "other"))
credit_data$employment <- factor(credit_data$employment, levels = c("unemployed", "<1", "1<=X<4", "4<=X<7", ">=7"))
credit_data$personal_status <- factor(credit_data$personal_status, levels = c("male single", "female div/dep/mar", "male mar/wid", "male div/sep"))
credit_data$other_parties <- factor(credit_data$other_parties, levels = c("none", "co applicant", "guarantor"))
credit_data$property_magnitude <- factor(credit_data$property_magnitude, levels = c("no known property", "car", "life insurance", "real estate"))
credit_data$other_payment_plans <- factor(credit_data$other_payment_plans, levels = c("none", "bank", "stores"))
credit_data$housing <- factor(credit_data$housing, levels = c("rent", "own", "for free"))
credit_data$job <- factor(credit_data$job, levels = c("unskilled resident", "unemployed", "skilled", "high qualif/self emp/mgmt"))
credit_data$foreign_worker <- factor(credit_data$foreign_worker, levels = c("no", "yes"))
credit_data$class <- factor(credit_data$class, levels = c("0", "1"))

# Exploratory Data Analysis (EDA)
eda_plots <- credit_data %>%
  select(duration, credit_amount, installment_commitment, age, num_dependents, class) %>%
  ggpairs(columns = 1:5, ggplot2::aes(color = class))

eda_plots

# Split data into training and testing sets
library(caTools)
set.seed(123)
split <- sample.split(credit_data$class, SplitRatio = 0.7)
train_data <- subset(credit_data, split == TRUE)
test_data <- subset(credit_data, split == FALSE)

# Remove rows with missing values
#credit_data <- na.omit(credit_data)
# Remove rows with missing values
#train_data <- na.omit(train_data)
# Remove rows with missing values
#test_data <- na.omit(test_data)



#############
model_1 <- glm(class ~ checking_status + duration + credit_history + purpose + 
                 credit_amount + savings_status + employment + installment_commitment + 
                 personal_status + other_parties + residence_since + property_magnitude + age + 
                 other_payment_plans + housing + existing_credits + job + num_dependents, data = train_data, family = binomial(link="logit"))

model_1
# Model evaluation and feature selection
#a. Evaluate model performance using confusion matrix, ROC curve and AUC
#b. Select top features using recursive feature elimination (RFE) and compare model performance
#c. Interpret the coefficients of the final model and analyze the impact of each feature on the credit status

#Here is a sample code to evaluate the model and perform feature selection:
 # Predict the class probabilities and labels on test set
probabilities <- predict(model_1, newdata = test_data, type = "response")


predicted_class <- ifelse(probabilities > 0.5, "1", "0")

#Create confusion matrix
table(actual = test_data$class, predicted = predicted_class)


#Create ROC curve and calculate AUC
library(pROC)
roc_data <- roc(test_data$class, probabilities)
plot(roc_data)
auc(roc_data)


# Remove rows with missing values
credit_data <- na.omit(credit_data)

#Perform recursive feature elimination (RFE) to select top features
library(caret)
rfe_control <- rfeControl(method = "cv", number = 5)
rfe_result <- rfe(train_data[, 1:18], train_data[, 19], sizes = c(1:18),
                  rfeControl = rfe_control, method = "glm", family = "binomial")

# Plot the results of RFE
plot(rfe_result)

# Select top 10 features
selected_features <- colnames(train_data[, -19])[rfe_result$optVariables]

# Model 2 - using only selected features
model_2 <- glm(class ~ ., data = train_data[, c(selected_features, "class")], family = binomial(link="logit"))

# Model evaluation and interpretation of results
# a. Evaluate model performance using confusion matrix, ROC curve and AUC
probabilities_model2 <- predict(model_2, newdata = test_data[, selected_features], type = "response")
predicted_class_model2 <- ifelse(probabilities_model2 > 0.5, "good", "bad")
table(actual = test_data$class, predicted = predicted_class_model2)

roc_data_model2 <- roc(test_data$class, probabilities_model2)
plot(roc_data_model2)
auc(roc_data_model2)

# b. Interpret the coefficients of the final model and analyze the impact of each feature on the credit status
summary(model_2)


#The above code performs the following steps:
  
#  Predict the class probabilities and labels on the test set using the first logistic regression model we built.
#Create a confusion matrix to evaluate the performance of the model.
#Create an ROC curve and calculate the AUC to evaluate the performance of the model.
#Perform recursive feature elimination (RFE) to select the top features that contribute the most to the model's performance.
#    Build a new logistic regression model with the selected features.
#    Predict the class probabilities and labels on the test set using the final model we built.
#    Create a confusion matrix to evaluate the performance of the final model.
#    Create an ROC curve and calculate the AUC to evaluate the performance of the final model.
#    Print a summary of the final model, including the coefficients and their standard errors, z-values, and p-values.

#The RFE step will take some time to run, as we are performing cross-validation for each subset of features. However, this step is important to identify the most important features and improve the performance of our model.


# The coefficients represent the change in the log odds of the response variable per unit change in the predictor variable
# If the coefficient is positive, then the odds of the response variable being 'good' increases with increasing predictor variable.
# If the coefficient is negative, then the odds of the response variable being 'good' decreases with increasing predictor variable.
# For categorical variables, each level of the variable is compared to the reference level. If the coefficient is positive for a level,
# then that level of the variable is associated with higher odds of the response variable being 'good' than the reference level.

# The coefficient of 'checking_status[T.<0]' is negative, which means that having checking status '<0' is associated with lower odds of
# the credit status being 'good' than having 'no checking' status.
# The coefficient of 'duration' is negative, which means that longer duration of credit is associated with lower odds of
# the credit status being 'good'.
# The coefficient of 'credit_history[T.critical/other existing credit]' is negative, which means that having critical/other existing credit history
# is associated with lower odds of the credit status being 'good' than having no credit history.
# The coefficient of 'credit_history[T.delayed previously]' is negative, which means that having delayed credit history
# is associated with lower odds of the credit status being 'good' than having no credit history.
# The coefficient of 'purpose[T.business]' is negative, which means that having 'business' purpose of credit
# is associated with lower odds of the credit status being 'good' than having 'new car' purpose.
# The coefficient of 'purpose[T.furniture/equipment]' is negative, which means that having 'furniture/equipment' purpose of credit
# is associated with lower odds of the credit status being 'good' than having 'new car' purpose.
# The coefficient of 'savings_status[T.100<=X<500]' is positive, which means that having savings status between '100<=X<500'
# is associated with higher odds of the credit status being 'good' than having 'no known savings' status.
# The coefficient of 'savings_status[T.>=100


                                                                                                                                                                                                                                                                                                                                                                                    
