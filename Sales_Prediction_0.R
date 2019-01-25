
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
head(df)

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
  geom_line()+
  theme_economist()+
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.background = element_rect(fill = "#f9f5f1"),
    #text = element_text(size = 7),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(vjust = 0.4, size = 7),
    axis.ticks.x = element_blank()
  )+
  labs(title = "Sales Trend",
       x = "Year",
       y = "sales price (log)")

# check the dates of few seasonal peaks

train %>% 
  filter(sales > 9.75) -> train_peaks
# it seems it is beacause of christmas holiday
train_peaks

# trend of oil price
train %>%
  ggplot(aes(date, oil_price)) + 
  geom_line()+
  theme_economist()+
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.background = element_rect(fill = "#f9f5f1"),
    #text = element_text(size = 7),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(vjust = 0.4, size = 7),
    axis.ticks.x = element_blank()
  )+
  labs(title = "Oil Price Trend",
       x = "Year",
       y = "Price per barrel")

glimpse(df)

train %>%
  filter(date > "2015-01-01", date < '2016-01-01') %>% 
  ggplot(aes(date, comp_prom)) + 
  geom_line()+
  theme_economist()+
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.background = element_rect(fill = "#f9f5f1"),
    #text = element_text(size = 7),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(vjust = 0.4, size = 7),
    axis.ticks.x = element_blank()
  )+
  labs(title = "Competitor Promotion Trend",
       x = "Year",
       y = "Avg. no. of iters")
## seems some seasonality and trend

## No. of promotions

train %>%
  ggplot(aes(date, promotions)) + 
  geom_line()+
  theme_economist()+
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.background = element_rect(fill = "#f9f5f1"),
    #text = element_text(size = 7),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(vjust = 0.4, size = 7),
    axis.ticks.x = element_blank()
  )+
  labs(title = "Own Promotion Trend",
       x = "Year",
       y = "no. of iters")
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
  geom_line(alpha = 0.5)+
  theme_economist()+
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.background = element_rect(fill = "#f9f5f1"),
    #text = element_text(size = 7),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(vjust = 0.4, size = 7),
    axis.ticks.x = element_blank()
  )

## checking for anomaly
df_2 <- ts(df_1$sales)
df_1$sales_cln <- tsclean(df_2)

library(anomalize)


# price and clean price check
ggplot(df_1) + 
  geom_line(aes(date, sales, color = "blue")) +
  geom_line(aes(date, sales_cln, color = "red"))

df_1 %>% 
  as.tibble() %>% 
  time_decompose(sales) %>% 
  anomalize(remainder) %>% 
  time_recompose() %>% 
  plot_anomalies(time_recomposed = TRUE, ncol = 3)
## few are shown are anomalies


## Imputing the lowest value
df_1 %>% 
  filter(sales == min(sales)) %>% 
  select(date) -> min_date   # 2015-07-07

df_1 %>% 
  filter(date == min_date$date - 365) %>% 
  select(sales) -> pre_sales

df_1 %>% 
  filter(year(date) == year(min_date$date),month(date) == month(min_date$date),
         date != min_date$date) %>% 
  group_by(type) %>% 
  summarise(avg = mean(sales)) -> impute_val

df_1[df_1$date == min_date$date,"sales"] <- impute_val$avg

library(timetk)
df_1 %>% 
  select(type, date, sales) %>% 
  tk_augment_timeseries_signature() -> df_new

test %>% 
  select(date) %>% 
  tk_augment_timeseries_signature() -> test_new


df_new$diff[is.na(df_new$diff)] <- 86400
test_new$diff[is.na(test_new$diff)] <- 86400


library(caret)
df_new <- df_new[,!colnames(df_new) %in% names(df_new)[caret::nearZeroVar(df_new)]]
test_new <- test_new[,!colnames(test_new) %in% names(test_new)[caret::nearZeroVar(test_new)]]
# columns with non-zero variance

# correlation
library(corrplot)
glimpse(df_new)
cor <- cor(df_new[,sapply(df_new, is.numeric)])
corrplot::corrplot(cor, method = "ellipse", type = "upper")

# test data
cor <- cor(test_new[,sapply(test_new, is.numeric)])
corrplot::corrplot(cor, method = "ellipse", type = "upper")


cor_cut <- findCorrelation(cor, cutoff = 0.9)
colnames(df_new)[cor_cut]
colnames(test_new)[cor_cut]

df_comb <- df_1 %>% 
  left_join(df_new, by = c("date"))
glimpse(df_comb)

test_new <- test %>% 
  left_join(test_new, by = c("date"))
test_new %>% 
  mutate(sales = NULL) -> test_new

# new features

df_comb %>% 
  group_by(year, month,week) %>% 
  mutate(oil_week = mean(oil_price)) %>% 
  group_by(year, month) %>% 
  mutate(oil_mon = mean(oil_price)) %>% 
  ungroup() -> df_comb

test_new %>% 
  group_by(month,week) %>% 
  mutate(oil_week = mean(oil_price)) %>% 
  group_by(month) %>% 
  mutate(oil_mon = mean(oil_price)) %>% 
  ungroup() -> test_new

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

cor_del <- c("year.iso","yday","mday","wday.xts","half","quarter","qday","date","month.xts","month","index.num","mday7")

dub_del <- c("sales_cln","type.y","sales.y")

df_comb  <- df_comb %>% 
  select(-one_of(cor_del))

df_comb  <- df_comb %>% 
  select(-one_of(dub_del))

test_new <- test_new %>% 
  select(-one_of(cor_del))


df_comb <- df_comb %>% 
  mutate(sales = sales.x,
         type = type.x) %>% 
  mutate(sales.x = NULL,
         type.x = NULL) 


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
  step_BoxCox(oil_week, limits = c(-2,2)) %>% 
  step_BoxCox(oil_mon, limits = c(-2,2)) %>% 
  prep(training = train_1) -> t
t
train_2 <- bake(t,newdata = train_1)
test_2 <- bake(t, newdata = test_1)
test_fnl <- bake(t, newdata = test_new)

glimpse(train_2)

col_rm <- c("wday","leap_yr","is_local")
train_2 %>% 
  select(-one_of(col_rm)) -> train_2
test_2 %>% 
  select(-one_of(col_rm)) -> test_2

train_fnl <- 
  bind_rows(train_2, test_2)

test_fnl %>% 
  select(-one_of(col_rm)) %>% 
  mutate(year = 2017)-> test_fnl

#setdiff(names(test_2),names(test_fnl))

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

# ensemble of XGB and Linear

pred_1 %>% 
  mutate(pred_lm_xgb = 0.5*pred_xgb + 0.5*pred_lm) -> pred_1
rmse(pred_1, original, pred_lm_xgb) 

pred_2$fnl_lm <- (predict(mod_lm, newdata = test_fnl))
pred_2 %>% 
  mutate(pred_lm_xgb = 0.5*fnl_xgb + 0.5*fnl_lm) -> pred_2
###################################### random forest ###############################
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     verboseIter = FALSE)


mod_rf <- caret::train(sales~.,
                        data = train_2[,-c(1,7)],
                        method = "rf",
                        preProcess = c("center","scale"),
                        trControl = ctrl)

pred_1 <- data.frame(original = test_2$sales)
pred_1$pred_rf <- predict(mod_rf, newdata = test_2[,-c(1,20)])
ggplot(pred_1) +
  geom_line(aes(x = c(1:180),original, color = "red"))+
  geom_line(aes(x = c(1:180),pred_rf, color = "blue"))

rmse(pred_1, original, pred_rf)
# 0.067

pred_1 %>% 
  mutate(pred_rf_xgb = 0.5*pred_xgb + 0.5*pred_rf) -> pred_1

rmse(pred_1, original, pred_rf_xgb)
# 0.0920 - glm
# 0.0697 - rf
# 0.058 - rf_xgb


###################################### XGB ###########################################
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     verboseIter = FALSE)

mod_xgb <- caret::train(sales~.,
                           data = train_fnl,
                           method = "xgbLinear",
                           preProcess = c("scale", "center"),
                           trControl = ctrl)

mod_xgb$bestTune
plot(varImp(mod_xgb))

pred_1 <- data.frame(original = test_2$sales)
pred_1$pred_xgb <- predict(mod_xgb, newdata = test_2[,-c(19)])


ggplot(pred_1) +
  geom_line(aes(x = c(1:180),original, color = "red"))+
  geom_line(aes(x = c(1:180),pred_xgb, color = "blue"))

library(yardstick)
rmse(pred_1, original, pred_xgb)
# 0.06035 - removed is_local
# 0.05666 - removed - ("wday","leap_yr","is_local")

glimpse(test_fnl)
pred_2 <- data.frame(fnl_xgb = predict(mod_xgb, newdata = test_fnl))


train %>% 
  filter(date >= "2015-01-27", date <= "2015-08-14") %>% 
  select(sales) -> yr_15

train %>% 
  filter(date >= "2016-01-29", date <= "2016-08-15") %>% 
  select(sales) -> yr_16

pred_2$yr_15 <- yr_15$sales
pred_2$yr_16 <- yr_16$sales

glimpse(pred_2)

ggplot(pred_2) +
  geom_line(aes(x = c(1:200),fnl_xgb, color = "red"))+
  geom_line(aes(x = c(1:200),fnl_lm, color = "blue"))+
  geom_line(aes(x = c(1:200),pred_lm_xgb, color = "black"))

write.csv(pred_2, "E:\\Study\\R Projects\\Common files\\Sales\\final_1.csv")
########################################### catboost ################################

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     verboseIter = FALSE)

grid <- expand.grid(depth = c(4, 5, 6),
                    learning_rate = c(0.15,0.1),
                    iterations = c(500,800),
                    l2_leaf_reg = c(0.1,0.2,0.3),  #gamma
                    rsm = c(0.8,0.90),           #colsample_bytree
                    border_count = 64)

mod_cat <- train(train_2, 
               y = train_2$sales,
               method = catboost.caret,
               logging_level = 'Silent', 
               preProc = NULL,
               tuneGrid = grid, 
               trControl = ctrl)

print(mod_cat)
pred_1$pred_cat <- predict(mod_cat,test_2)
ggplot(pred_1) +
  geom_line(aes(x = c(1:180),original, color = "red"))+
  geom_line(aes(x = c(1:180),pred_cat, color = "blue"))
rmse(pred_1, original, pred_cat)
# .235


