# Load the necessary packages
library(tidyverse)
library(gridExtra)
library(cowplot)
library(lubridate)
library(GGally)


# Read in the data
credit_data <- read.csv("credit_customers.csv")

# Explore the data
summary(credit_data)
str(credit_data)

#There are more NA values than the other
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
credit_data$class <- factor(credit_data$class, levels = c("bad", "good"))

# Exploratory Data Analysis (EDA)
eda_plots <- credit_data %>%
  select(duration, credit_amount, installment_commitment, age, num_dependents, class) %>%
  ggpairs(columns = 1:5, ggplot2::aes(color = class))

eda_plots

# Model Building
train_index <- sample(seq_len(nrow(credit_data)), size = 0.7 * nrow(credit_data))
train_data <- credit_data[train_index,]
test_data <- credit_data[-train_index,]

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


predicted_class <- ifelse(probabilities > 0.5, "good", "bad")

#Create confusion matrix
table(actual = test_data$class, predicted = predicted_class)


#Create ROC curve and calculate AUC
library(pROC)
roc_data <- roc(test_data$class, probabilities)
plot(roc_data)
auc(roc_data)

#Perform recursive feature elimination (RFE) to select top features
library(caret)
library(mlr)
library(mlrCPO)

#library(mice)
# Impute missing values using mice package
#imputed_data <- mice(train_data, m = 5, method = "pmm")
#task <- makeClassifTask(id = "train_data", data = imputed_data, target = "class")

# Define task
task <- makeClassifTask(id = "train_data", data = train_data, target = "class")


library(glmnet)
# Define learner
learner <- makeLearner("classif.glmnet", predict.type = "prob")

# Define resampling strategy
resampling <- makeResampleDesc("CV", iters = 5)

# Define the feature selection control options
ctrl <- makeFeatSelControlSequential(method = "sbs", maxit = 5)

# Define the feature selection method
feature_selection <- makeFeatSelWrapper(learner, resampling = resampling, control = ctrl)

# Perform feature selection
selected_features <- selectFeatures(feature_selection, resampling = resampling, control = ctrl, show.info = FALSE)$selected.features
