rm(list = ls())

load_lb <- function()
{
  library(readxl)
  library(tidyr)
  library(dplyr)
  library(caret)
  library(rpart)
  library(tree)
  library(MASS)
  require(xgboost)
  require(data.table)
  require(Matrix)
}

load_lb()

## Loading data

#challenge_data.csv: Contains attributes related to each challenge


challenge <- read.csv(file.choose())
head(challenge)

## Column checks

countMissing(challenge$challenge_ID)
#           TOTAL ROWS: 5606 (100.0%)
# Missing Values (NAs): 0 (0.0%)
# Empty Strings (""): 0 (0.0%)
# Non-missing Value: 5606 (100.0%)  

countDups(challenge$challenge_ID)

#  Rows of Data: 5606
# Unique Values: 5606
# Duplicate Values: 0
# Missing Values: 0 (0.0%)
#  Mode & Class: numeric, factor

countMissing(challenge$programming_language)
#           TOTAL ROWS: 5606 (100.0%)
# Missing Values (NAs): 0 (0.0%)
# Empty Strings (""): 0 (0.0%)
# Non-missing Value: 5606 (100.0%)

countDups(challenge$programming_language)
#  Rows of Data: 5606
# Unique Values: 3
# Duplicate Values: 5603
# Missing Values: 0 (0.0%)
#  Mode & Class: numeric, integer

## changing programming language to factor

challenge$programming_language <- as.factor(challenge$programming_language)
mode(challenge$programming_language)
class(challenge$programming_language)

challenge$challenge_series_ID <- as.character(challenge$challenge_series_ID)
countMissing(challenge$challenge_series_ID)
#           TOTAL ROWS: 5606 (100.0%)
# Missing Values (NAs): 0 (0.0%)
# Empty Strings (""): 12 (0.2%)
# Non-missing Value: 5594 (99.8%)

countDups(challenge$challenge_series_ID)
#  Rows of Data: 5606
# Unique Values: 436
# Duplicate Values: 5170
# Missing Values: 0 (0.0%)
#  Mode & Class: numeric, factor


countMissing(challenge$total_submissions)
#           TOTAL ROWS: 5606 (100.0%)
# Missing Values (NAs): 352 (6.3%)
# Empty Strings (""): 0 (0.0%)
# Non-missing Value: 5254 (93.7%)

countDups(challenge$total_submissions)
#  Rows of Data: 5606
# Unique Values: 1068
# Duplicate Values: 4538
# Missing Values: 352 (6.3%)
#  Mode & Class: numeric, integer


countMissing(challenge$publish_date)
#           TOTAL ROWS: 5606 (100.0%)
# Missing Values (NAs): 0 (0.0%)
# Empty Strings (""): 0 (0.0%)
# Non-missing Value: 5606 (100.0%)

challenge$author_ID <- as.character(challenge$author_ID)
countMissing(challenge$author_ID)
#           TOTAL ROWS: 5606 (100.0%)
# Missing Values (NAs): 0 (0.0%)
# Empty Strings (""): 39 (0.7%)
# Non-missing Value: 5567 (99.3%)

challenge$author_gender <- as.character(challenge$author_gender)
countMissing(challenge$author_gender)
#           TOTAL ROWS: 5606 (100.0%)
# Missing Values (NAs): 0 (0.0%)
# Empty Strings (""): 97 (1.7%)
# Non-missing Value: 5509 (98.3%)


challenge$author_org_ID <- as.character(challenge$author_org_ID)
countMissing(challenge$author_org_ID)
#TOTAL ROWS: 5606 (100.0%)
# Missing Values (NAs): 0 (0.0%)
# Empty Strings (""): 248 (4.4%)
# Non-missing Value: 5358 (95.6%)

countDups(challenge$author_org_ID)
#  Rows of Data: 5606
# Unique Values: 1718
# Duplicate Values: 3888
# Missing Values: 0 (0.0%)
#  Mode & Class: character, character

countMissing(challenge$category_id)
#           TOTAL ROWS: 5606 (100.0%)
# Missing Values (NAs): 1841 (32.8%)
# Empty Strings (""): 0 (0.0%)
# Non-missing Value: 3765 (67.2%)

countDups(challenge$category_id)
#  Rows of Data: 5606
# Unique Values: 195
# Duplicate Values: 5411
# Missing Values: 1841 (32.8%)
#  Mode & Class: numeric, integer

##################################################################################
#train.csv: It contains the set of 13 challenges that were
#attempted by the same user in a sequence

train <- read.csv(file.choose())
head(train)

head(challenge)

## column checks

countMissing(train$challenge)
#           TOTAL ROWS: 903916 (100.0%)
# Missing Values (NAs): 0 (0.0%)
# Empty Strings (""): 0 (0.0%)
# Non-missing Value: 903916 (100.0%)

countDups(train$challenge)
#  Rows of Data: 903916
# Unique Values: 5348
# Duplicate Values: 898568
# Missing Values: 0 (0.0%)
#  Mode & Class: numeric, factor

train$challenge_sequence <- as.factor(train$challenge_sequence)
countMissing(as.character(train$challenge_sequence))
#           TOTAL ROWS: 903916 (100.0%)
# Missing Values (NAs): 0 (0.0%)
# Empty Strings (""): 0 (0.0%)
# Non-missing Value: 903916 (100.0%)

countMissing(train$user_id)
#           TOTAL ROWS: 903916 (100.0%)
# Missing Values (NAs): 0 (0.0%)
# Empty Strings (""): 0 (0.0%)
# Non-missing Value: 903916 (100.0%)

countDups(train$user_id)
#  Rows of Data: 903916
# Unique Values: 69532
# Duplicate Values: 834384
# Missing Values: 0 (0.0%)
#  Mode & Class: numeric, integer


head(train)
head(challenge)

#Joining the tables

#Modifying the column name
New_names <- c("User_seq","User_id","chal_seq","challenge_ID")
colnames(train) <- New_names

combined <- left_join(train,challenge,by="challenge_ID")
head(combined)






