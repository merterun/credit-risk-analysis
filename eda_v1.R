# Load required packages
library(tidyverse)
library(caret)

# Load dataset
credit_data <- read.csv("credit_customers.csv")

# Overview of data
glimpse(credit_data)


#Factors that affect credit status
#a. Analysis of credit_history, checking_status, purpose, credit_amount, etc.
#b. Identification of key features that contribute to good or bad credit status
# Visualize target variable
ggplot(credit_data, aes(x = class)) +
  geom_bar() +
  xlab("Credit Status") +
  ylab("Count") +
  ggtitle("Distribution of Credit Status")

# Create summary table of features by credit status
credit_summary <- credit_data %>%
  group_by(class) %>%
  summarise(
    mean_duration = mean(duration),
    mean_amount = mean(credit_amount),
    count_by_history = table(credit_history)
  )

credit_summary
# Visualize credit history by credit status

ggplot(credit_data, aes(x = credit_history, fill = class)) +
  geom_bar(position = "dodge") +
  xlab("Credit History") +
  ylab("Count") +
  ggtitle("Credit History by Credit Status")

# Visualize checking status by credit status
ggplot(credit_data, aes(x = checking_status, fill = class)) +
  geom_bar(position = "dodge") +
  xlab("Checking Status") +
  ylab("Count") +
  ggtitle("Checking Status by Credit Status")

# Visualize purpose by credit status
ggplot(credit_data, aes(x = purpose, fill = class)) +
  geom_bar(position = "dodge") +
  xlab("Purpose") +
  ylab("Count") +
  ggtitle("Purpose by Credit Status")

# 2. Relationship between personal characteristics and credit status
#a. Analysis of personal_status, age, job, etc.
#b. Comparison of credit status between different groups (e.g. married vs. single)

# Create summary table of personal characteristics by credit status
# Summarize personal characteristics by credit status
personal_summary <- credit_data %>%
  group_by(class) %>%
  summarize(
    age_mean = mean(age),
    age_sd = sd(age),
    job_count = n_distinct(job),
    job_list = paste(unique(job), collapse = ", "),
    personal_status_count = n_distinct(personal_status),
    personal_status_list = paste(unique(personal_status), collapse = ", ")
  )

table(personal_summary)


# Visualize personal status by credit status
ggplot(credit_data, aes(x = personal_status, fill = class)) +
  geom_bar(position = "dodge") +
  xlab("Personal Status") +
  ylab("Count") +
  ggtitle("Personal Status by Credit Status")



# Visualize age by credit status
ggplot(credit_data, aes(x = class, y = age)) +
  geom_boxplot() +
  xlab("Credit Status") +
  ylab("Age") +
  ggtitle("Age by Credit Status")

# Visualize job by credit status
ggplot(credit_data, aes(x = job, fill = class)) +
  geom_bar(position = "dodge") +
  xlab("Job") +
  ylab("Count") +
  ggtitle("Job by Credit Status")


# Chi-squared test of personal status and credit status
chisq.test(credit_data$personal_status, credit_data$class)

# T-test for age by credit status
t.test(age ~ class, data = credit_data)


#3. Impact of financial situation on credit status
#a. Analysis of savings_status, existing_credits, installment_commitment, etc.
#b. Identification of financial factors that are associated with good or bad credit status

savings_summary <- credit_data %>%
  group_by(class) %>%
  reframe(count_by_savings = table(savings_status))

table(savings_summary)

credit_summary <- credit_data %>%
  group_by(class) %>%
  reframe(
    count_by_existing_credits = table(existing_credits),
    mean_installment_commitment = mean(installment_commitment)
  )

table(credit_summary)

installment_summary <- credit_data %>%
  group_by(class) %>%
  summarise(
    mean_installment_commitment = mean(installment_commitment),
    median_credit_amount = median(credit_amount)
  )

table(installment_summary)




