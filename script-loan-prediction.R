# 0. SET UP ----------------------------------------------------------------------------------------
# LOAD LIBRARIES
library(dplyr)
library(here)
library(readr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(caret)
library(ROCR)

# READ DATA
train_demo <- read_csv("raw_data/traindemographics.csv")
train_perf <- read_csv("raw_data/trainperf.csv")
train_prev <- read_csv("raw_data/trainprevloans.csv")

test_demo <- read_csv("raw_data/testdemographics.csv")
test_perf <- read_csv("raw_data/testperf.csv")
test_prev <- read_csv("raw_data/testprevloans.csv")
# Note: test_perf date columns are not dates - parsing failure

summary(train_demo)
summary(train_perf)

# 1. EDA -------------------------------------------------------------------------------------------

# Target variable - classification
train_perf %>%
  group_by(good_bad_flag) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=good_bad_flag, y=count)) +
  geom_col() +
  theme(legend.position = "none") +
  ggtitle("Distribution of classification")

# Distribution of loan amount
ggplot(train_perf, aes(loanamount)) +
  geom_histogram() +
  ggtitle("Distribution of loan amount")


# 2. CLEAN, PRE-PROCESS AND PREPARE ----------------------------------------------------------------
# Combine data sets
train_perf <- train_perf %>%
  left_join(train_demo, by = "customerid")

test_perf <- test_perf %>%
  left_join(train_demo, by = "customerid")

# Missing values
# replace missing categorical variables with another level ("not_available") and convert to factors
train_perf <- train_perf %>%
  mutate_if(is.character, replace_na, replace = "not_available") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(customerid = as.character(customerid))

test_perf <- test_perf %>%
  mutate_if(is.character, replace_na, replace = "not_available") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(customerid = as.character(customerid))

# Create target variable
train_perf <- train_perf %>%
  mutate(Good_Bad_flag = ifelse(good_bad_flag == "Good", 1, 0)) %>%
  select(-good_bad_flag)

# 3. FEATURE ENGINEERING ---------------------------------------------------------------------------
# Eg:
# - Age of customer at time of loan application
# - Number of previous loans and time period
# - Use GPS to get location - classify as urban or rural?
# - Was customer referred or not?

train_perf <- train_perf %>%
  # Referred or not?
  mutate(referred = ifelse(is.na(referredby), 0, 1)) %>%
  select(-referredby)

test_perf <- test_perf %>%
  # Referred or not?
  mutate(referred = ifelse(is.na(referredby), 0, 1)) %>%
  select(-referredby)

# 4. CROSS VALIDATION ------------------------------------------------------------------------------
# Split train_perf set into test and train
set.seed(123)
in_train <- createDataPartition(train_perf$Good_Bad_flag, p = 0.9, list = FALSE)
training <- train_perf[ in_train, ]
testing <- train_perf[ -in_train, ]

# 5. MODEL -----------------------------------------------------------------------------------------
# Simple logistic regression
model <- glm(Good_Bad_flag ~ loannumber + loanamount + totaldue + termdays + bank_account_type +
               employment_status_clients + level_of_education_clients,
             data = training, family = binomial)

summary(model)

# 6. EVALUATE ---------------------------------------------------------------------------------------

importance <- data.frame(varImp(model, scale = TRUE))
importance

prediction <- predict(model, newdata = testing, type = "response")
accuracy <- table(prediction, testing[, "Good_Bad_flag"])
confusionMatrix(data = table(prediction, testing$Good_Bad_flag))

prob <- predict(model, newdata = testing)
pred <- prediction(prob, testing$Good_Bad_flag)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# 7. PREDICT ---------------------------------------------------------------------------------------
# Run model on final test data
final_prediction <- predict(model, test_perf)

Good_Bad_flag <- ifelse(final_prediction >= 0.6, 1, 0)

# 8. PREPARE SUBMISSION FILE -----------------------------------------------------------------------
submission <- cbind(test_perf, Good_Bad_flag) %>%
  select(customerid, Good_Bad_flag)

write_csv(submission, "submission.csv")

