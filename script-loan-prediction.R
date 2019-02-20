# 0. SET UP ----------------------------------------------------------------------------------------
# LOAD LIBRARIES
library(dplyr)
library(here)
library(readr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(caret)

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
# - Age of customer at time of loan application (seems to be issue with dates in test files though?)
# - Number of previous loans and time period
# - Use GPS to get location - classify as urban or rural?
# - Was customer referred or not?

train_perf <- train_perf %>%
  left_join(train_demo, by = "customerid") %>%
  # Referred or not?
  mutate(referred = ifelse(is.na(referredby), 0, 1)) %>%
  select(-referredby)

test_perf <- test_perf %>%
  left_join(train_demo, by = "customerid") %>%
  # Referred or not?
  mutate(referred = ifelse(is.na(referredby), 0, 1)) %>%
  select(-referredby)

# 4. CROSS VALIDATION ------------------------------------------------------------------------------
# Split training set into test and train

# 5. MODEL -----------------------------------------------------------------------------------------


# 6. PREDICT ---------------------------------------------------------------------------------------


# 7. PREPARE SUBMISSION FILE -----------------------------------------------------------------------


