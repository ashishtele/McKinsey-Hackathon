
rm(list = ls())

load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(anomalize))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(require(lubridate))
  suppressPackageStartupMessages(require(forecast))
  suppressPackageStartupMessages(require(tseries))
}

load_lb()
library(data.table)
library(catboost)
library(recipes)
library(corrplot)
library(ggthemes)

# File import

df <- fread("E:\\Study\\R Projects\\Common files\\Sales\\train.csv",
            sep = "|")
glimpse(df)

# datatype change

df %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         sales = as.double(sales),
         closed = as.factor(closed),
         is_national = as.factor(is_national),
         is_local = as.factor(is_local),
         promotions = as.integer(promotions)) -> df

# train and test creation

train <- df %>% 
  filter(!is.na(sales))
test <- df %>% 
  filter(is.na(sales))

# Trend check for sales


# plot the series
train %>%
  ggplot(aes(date, sales)) + 
  geom_line()

# trend of oil price
train %>%
  ggplot(aes(date, oil_price)) + 
  geom_line()

glimpse(df)

train %>%
  filter(date > "2015-01-01", date < '2016-01-01') %>% 
  ggplot(aes(date, comp_prom)) + 
  geom_line()

## seems some seasonality and trend

## No. of promotions

train %>%
  ggplot(aes(date, promotions)) + 
  geom_line()
## trend and seasonality

# trend check (train and test)

train_1 <- train[1:(nrow(train)-180),]
test_1 <- train[(nrow(train)-180+1):nrow(train),]

#train_1 <- train[1:606,]
#test_1 <- train[607:807,]


df_1 <- bind_rows(
  train_1 %>% add_column(type = "train"),
  test_1 %>% add_column(type = "test")
)

df_1 %>% 
  ggplot(aes(date, sales, color = type))+
  geom_point(alpha = 0.15)+
  geom_line(alpha = 0.5)


df_comb <- df_1

test_new <- test 

# new features

# christmas date
chris_dates <-  data.frame(date = c("2014-12-22","2014-12-23","2014-12-24",
                "2015-12-22","2015-12-23","2015-12-24",
                "2016-12-22","2016-12-23","2016-12-24",
                "2017-12-22","2017-12-23","2017-12-24")) %>% 
  mutate(date = as.Date(date),
         chris_flg = 1)
# join the tables

df_comb %>% 
  left_join(chris_dates, by = c("date")) -> df_comb
df_comb %>% 
  mutate(chris_flg = as.factor(ifelse(is.na(chris_flg),0,chris_flg)),
         leap_yr = ifelse(year==2016,1,0)) -> df_comb

test_new %>% 
  left_join(chris_dates, by = c("date")) -> test_new
test_new %>% 
  mutate(chris_flg = as.factor(ifelse(is.na(chris_flg),0,chris_flg))) -> test_new
test_new %>% 
  mutate(leap_yr = 0) -> test_new


train_1 <- df_comb %>% 
  filter(type == "train") %>% 
  select(-type)

test_1 <- df_comb %>% 
  filter(type == "test") %>% 
  select(-type)


glimpse(train_1)

train_1 %>% recipe(sales~.) %>%
  step_BoxCox(oil_price, limits = c(-2,2)) %>%
  step_BoxCox(comp_prom, limits = c(-2,2)) %>%
  prep(training = train_1) -> t
t
train_2 <- bake(t,newdata = train_1)
test_2 <- bake(t, newdata = test_1)
test_fnl <- bake(t, newdata = test_new)

glimpse(train_2)

train_fnl <- 
  bind_rows(train_2, test_2)

test_fnl %>% 
  mutate(year = 2017)-> test_fnl


# Modeling

####################################### Linear ######################################
ctrl1 <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 10,
                     verboseIter = FALSE)


mod_lm <- caret::train(sales~.,
                        data = train_fnl,
                        method = "lm",
                        preProcess = c("scale", "center"),
                        trControl = ctrl1)

pred_1$pred_lm <- predict(mod_lm, newdata = test_2[,-c(19)])
ggplot(pred_1) +
  geom_line(aes(x = c(1:180),original, color = "red"))+
  geom_line(aes(x = c(1:180),pred_lm, color = "blue"))

rmse(pred_1, original, pred_lm)

#################################### Arima ##########################################

library(forecast)
# create some artifical data
modelfitsample <- data.frame(oil_price,
                             comp_prom,
                             chris_flg)

# Variable to be modelled
sl <- ts(modelfitsample$sales, frequency=7)

# Find ARIMAX model
modArima <- auto.arima(sl, xreg= modelfitsample)