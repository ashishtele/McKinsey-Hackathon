
rm(list = ls())

load_lb <- function()
{
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(caret))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(GGally))
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(ggpubr))
}

load_lb()

# Importing the train and test files

train <- fread("E:\\Study\\R Projects\\Common files\\Dengue\\train_values.csv", sep = "|")
test <- fread("E:\\Study\\R Projects\\Common files\\Dengue\\test_values.csv", sep = "|")

glimpse(train)

train %>% 
  select(-patient_id) %>% 
  mutate(thal = as.factor(thal),
         slope_of_peak_exercise_st_segment = as.factor(slope_of_peak_exercise_st_segment),
         chest_pain_type = as.factor(chest_pain_type),
         num_major_vessels = as.factor(num_major_vessels),
         fasting_blood_sugar_gt_120_mg_per_dl = as.factor(fasting_blood_sugar_gt_120_mg_per_dl),
         resting_ekg_results = as.factor(resting_ekg_results),
         sex = as.factor(sex),
         exercise_induced_angina = as.factor(exercise_induced_angina)) %>% 
  rename(sugar_level = fasting_blood_sugar_gt_120_mg_per_dl,
         peak_ex = slope_of_peak_exercise_st_segment,
         angina = exercise_induced_angina,
         serum_chol = serum_cholesterol_mg_per_dl)-> train

glimpse(train)

library(GGally)
ggpairs(train)

train %>% 
  ggplot(aes(Target, fill = Target))+
  geom_bar()

train %>%
mutate(Target = ifelse(Target == 1, "yes","no")) -> train

train %>% 
  mutate(Target = as.factor(factor(Target, levels = unique(train$Target)))) -> train

set.seed(123)
library(rsample)
index <- createDataPartition(train$Target, p = 0.7, list = FALSE)
df_train <- train[index,]
df_test <- train[-index,]

table(df_train$Target) %>% 
  prop.table()

table(df_test$Target) %>% 
  prop.table()

rec_obj <- df_train %>% 
  recipe(Target~.) %>%
  step_BoxCox(all_numeric(),-all_nominal(),-all_outcomes()) %>% 
  step_center(all_numeric(),-all_nominal(),-all_outcomes()) %>% 
  step_scale(all_numeric(),-all_nominal(),-all_outcomes()) %>% 
  step_dummy(all_nominal(),-all_outcomes()) %>%
  prep(training = df_train)

rec_obj
df_train_1 <- bake(rec_obj, newdata = df_train)
df_test_1 <- bake(rec_obj, newdata = df_test)  

levels(df_train_1$Target) <- make.names(levels(factor(df_train_1$Target)))
levels(df_test_1$Target) <- make.names(levels(factor(df_test_1$Target)))

glimpse(df_train_1)
df_train_1 %>% 
  ggplot(aes(x = max_heart_rate_achieved, fill = Target))+
  geom_density(alpha = 0.3)


## XGBoost 

# Bayesian Optimization
library(MlBayesOpt)
set.seed(123)
res0 <- xgb_cv_opt(data = df_train_1,
                   label = Target,
                   objectfun = "binary:logistic",
                   evalmetric = "logloss",
                   n_folds = 5,
                   acq = "ucb",
                   init_points = 10,
                   n_iter = 20)

# update the grid
xgbGrid <- expand.grid(nrounds = 70,
                       max_depth = 6,
                       eta = 1,
                       gamma = 0,
                       colsample_bytree = 1,
                       min_child_weight = 1,
                       subsample = 1)

rm(train_ctrl)
train_ctrl1 <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 3,
                            classProbs = TRUE)

library(doParallel)
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

mod_xgb <- caret::train(Target~.,
                        data = df_train_1,
                        trControl = train_ctrl1,
                        method = "xgbTree",
                        metric = "logLoss",
                        tuneGrid = xgbGrid)
mod_xgb
stopCluster(cl)

mod_xgb$bestTune
plot(varImp(mod_xgb))

varImp(mod_xgb, scale = FALSE)$importance %>% 
  mutate(names = row.names(.)) %>% 
  filter(Overall > 0) %>% 
  arrange(-Overall) -> var_xgb


pred_xgb_p <- predict(mod_xgb, newdata = df_test_1[,-1], type = "prob")
pred_xgb <- predict(mod_xgb, newdata = df_test_1[,-1])

confusionMatrix(pred_xgb, df_test_1$Target)



