
rm(list = ls())

# platform = "windows"
# rfhome = "C:/RuleFit"
# source("C:/RuleFit/rulefit.r")
# #install.packages("akima", lib=rfhome)
# library(akima, lib.loc=rfhome)

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

train <- fread("E:\\Study\\R Projects\\Common files\\Blood Donation\\train.csv")
test <- fread("E:\\Study\\R Projects\\Common files\\Blood Donation\\test.csv")

glimpse(train)
glimpse(test)
  
colnames(train) <- c('ID','Last_don','no_of_don','tot_vol','first_don','Target')
colnames(test) <- c('ID','Last_don','no_of_don','tot_vol','first_don')

ggpairs(train)
# correlation: tot_vol and no_of_don

train %>% 
  mutate(Target = ifelse(Target == 1, "yes","no")) -> train 

train %>% 
  mutate(Target = as.factor(factor(Target, levels = unique(train$Target)))) -> train

glimpse(train)

# new column creation in train and test
train %>% 
  mutate(rate = no_of_don/first_don,
         lst_don = Last_don/no_of_don,
         retrn = 1/rate - Last_don,
         stop = Last_don/first_don) %>% 
  select(-c(1,4)) -> new_train

test %>% 
  mutate(rate = no_of_don/first_don,
         lst_don = Last_don/no_of_don,
         retrn = 1/rate - Last_don,
         stop = Last_don/first_don) %>% 
  select(-c(1,4)) -> new_test

glimpse(new_train)

new_train %>% 
  ggdensity(x = "first_don",
            facet.by = "Target",
            add = "median",
            fill = "Target",
            color = "Target"
            ) %>% 
  ggpar(legend = "none") -> p1

new_train %>% 
  ggdensity(x = "no_of_don",
            facet.by = "Target",
            add = "median",
            fill = "Target",
            color = "Target") %>% 
  ggpar(legend = "none") -> p2

new_train %>% 
  ggdensity(x = "Last_don",
            facet.by = "Target",
            add = "median",
            fill = "Target",
            color = "Target") %>% 
  ggpar(legend = "none") -> p3

gridExtra::grid.arrange(p1,p2,p3)

## transform and scale 

library(recipes)
new_train %>% 
  recipe(Target~.) %>% 
  step_BoxCox(all_numeric(), limits = c(-2,2)) %>% 
  step_center(all_numeric(),-all_outcomes()) %>% 
  step_scale(all_numeric(),-all_outcomes()) %>% 
  prep(training = new_train) -> rec_obj
rec_obj


train_new <- bake(rec_obj, newdata = new_train)
test_new <- bake(rec_obj, newdata = new_test)

test_new$Target <- NULL

ggplot(train_new, aes(x=no_of_don)) +
  geom_density()


train_new %>% 
  ggdensity(x = "first_don",
            facet.by = "Target",
            fill = "Target",
            color = "Target"
  ) %>% 
  ggpar(legend = "none") -> p1

train_new %>% 
  ggdensity(x = "no_of_don",
            facet.by = "Target",
            add = "median",
            fill = "Target",
            color = "Target") %>% 
  ggpar(legend = "none") -> p2

train_new %>% 
  ggdensity(x = "Last_don",
            facet.by = "Target",
            add = "median",
            fill = "Target",
            color = "Target") %>% 
  ggpar(legend = "none") -> p3

gridExtra::grid.arrange(p1,p2,p3)


# Rulefit

library(pre)

df <- train %>% 
  select(-c(1))

ens <- pre(Target~ .,
           data = df)
ens
imp <- importance(ens)
imp

train %>% 
  mutate(rule1 = as.factor(ifelse(Last_don <= 9 & tot_vol > 1000 & first_don <= 53,1,0)),
         rule2 = as.factor(ifelse(no_of_don > 1 & Last_don <= 6,1,0)),
         rule3 = as.factor(ifelse( Last_don <= 9 & no_of_don > 3,1,0)),
         rule4 = as.factor(ifelse( no_of_don > 4 & Last_don <= 9 & first_don <= 51,1,0))) -> train

test %>% 
  mutate(rule1 = as.factor(ifelse(Last_don <= 9 & tot_vol > 1000 & first_don <= 53,1,0)),
         rule2 = as.factor(ifelse(no_of_don > 1 & Last_don <= 6,1,0)),
         rule3 = as.factor(ifelse( Last_don <= 9 & no_of_don > 3,1,0)),
         rule4 = as.factor(ifelse( no_of_don > 4 & Last_don <= 9 & first_don <= 51,1,0))) -> test

# Distibution check

glimpse(train)
train %>% 
  ggplot(aes(Target))+
  geom_bar()

train %>% 
  group_by(Target) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(Target,n, fill = Target))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n, vjust = -0.4))
# 69:31


index <- createDataPartition(train$Target, p = 0.30, list = FALSE)

df_train <- train_new[index,]
df_test <- train_new[-index,]


table(df_train$Target) %>% 
  prop.table()
table(df_test$Target) %>% 
  prop.table()

#colnames(df_train) <- make.names(colnames(df_train))
#colnames(df_test) <- make.names(colnames(df_test))

set.seed(9)
rm(train_ctrl)
train_ctrl <- trainControl( classProbs = TRUE,
                            allowParallel = TRUE,
                            summaryFunction = mnLogLoss)

library(doParallel)
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)


mod_gbm <- caret::train(Target~.,
                        data = df_train,
                        method = "gbm",
                        preProcess = c("pca"),
                        trControl = train_ctrl,
                        metric = "logLoss",
                        maximize = FALSE)


mod_nnet <- caret::train(Target~.,
                        data = df_train,
                        method = "nnet",
                        preProcess = c("pca"),
                        trControl = train_ctrl,
                        metric = "logLoss",
                        maximize = FALSE)


mod_svm <- caret::train(Target~.,
                         data = df_train,
                         method = "svmRadial",
                         preProcess = c("pca"),
                         trControl = train_ctrl,
                         metric = "logLoss",
                         maximize = FALSE)

mod_ada <- caret::train(Target~.,
                        data = df_train,
                        method = "ada",
                        preProcess = c("pca"),
                        trControl = train_ctrl,
                        metric = "logLoss",
                        maximize = FALSE)

mod_gam <- caret::train(Target~.,
                        data = df_train,
                        method = "gamSpline",
                        preProcess = c("pca"),
                        trControl = train_ctrl,
                        metric = "logLoss",
                        maximize = FALSE)


result <- resamples(list(gbm = mod_gbm,
                         ada = mod_ada,
                         svm = mod_svm,
                         net = mod_nnet,
                         gam = mod_gam))

stopCluster(cl)
bwplot(result)

# tune search
gbmGrid <- expand.grid(n.trees=500,
                       interaction.depth=2,
                       shrinkage=0.005,
                       n.minobsinnode=10)

nnetGrid <- expand.grid(.decay=c(0.1, 0.5), .size=c(3, 4, 5))

svmGrid <- expand.grid(sigma=c(0.5),
                       C=c(0.3))

adaGrid <- expand.grid(iter=100, maxdepth=3, 
                       nu=0.05)


cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

set.seed(32)
train_ctrl1 <- trainControl( method = "repeatedcv",
                            number = 10,
                            repeats = 3,
                            classProbs = TRUE,
                            allowParallel = TRUE,
                            summaryFunction = mnLogLoss)

mod_gbm_1 <- caret::train(Target~.,
                        data = train_new,
                        method = "gbm",
                        preProcess = c("pca"),
                        trControl = train_ctrl1,
                        metric = "logLoss",
                        maximize = FALSE,
                        tuneGrid = gbmGrid)


mod_nnet_1 <- caret::train(Target~.,
                         data = train_new,
                         method = "nnet",
                         preProcess = c("pca"),
                         trControl = train_ctrl1,
                         metric = "logLoss",
                         maximize = FALSE,
                         tuneGrid = nnetGrid)


mod_svm_1 <- caret::train(Target~.,
                        data = train_new,
                        method = "svmRadial",
                        preProcess = c("pca"),
                        trControl = train_ctrl1,
                        metric = "logLoss",
                        maximize = FALSE,
                        tuneGrid = svmGrid)

mod_ada_1 <- caret::train(Target~.,
                        data = train_new,
                        method = "ada",
                        preProcess = c("pca"),
                        trControl = train_ctrl1,
                        metric = "logLoss",
                        maximize = FALSE,
                        tuneGrid = adaGrid)

mod_gam_1 <- caret::train(Target~.,
                        data = train_new,
                        method = "gamSpline",
                        preProcess = c("pca"),
                        trControl = train_ctrl1,
                        metric = "logLoss",
                        maximize = FALSE)


result_1 <- resamples(list(gbm = mod_gbm,
                         ada = mod_ada,
                         svm = mod_svm,
                         net = mod_nnet,
                         gam = mod_gam)) %>% as.data.frame()

stopCluster(cl)

ggpairs(result_1[,-6])

## Ensemble results
pred1V <- predict(mod_gbm_1, train_new, "prob")
pred2V <- predict(mod_nnet_1, train_new, "prob")
pred3V <- predict(mod_svm_1, train_new, "prob")
pred4V <- predict(mod_ada_1, train_new, "prob")
pred5V <- predict(mod_gam_1, train_new, "prob")

combined.data <- data.frame(pred1=pred1V$yes, pred2=pred2V$yes, pred3=pred3V$yes, 
                            pred4=pred4V$yes, pred5=pred5V$yes, 
                            Target = train_new$Target)


## XGBoost on Ensemble



# Bayesian Optimization
library(MlBayesOpt)
set.seed(123)
res0 <- xgb_cv_opt(data = combined.data,
                   label = Target,
                   objectfun = "binary:logistic",
                   evalmetric = "logloss",
                   n_folds = 5,
                   acq = "ucb",
                   init_points = 10,
                   n_iter = 20)

# update the grid
xgbGrid <- expand.grid(nrounds = 88,
                       max_depth = 6,
                       eta = 0.8109,
                       gamma = 0,
                       colsample_bytree = 1,
                       min_child_weight = 1,
                       subsample = 0.9401)

rm(train_ctrl)
train_ctrl <- trainControl( method = "repeatedcv",
                             number = 10,
                             repeats = 5,
                             classProbs = TRUE,
                             allowParallel = TRUE,
                             summaryFunction = mnLogLoss)

library(doParallel)
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

rm(mod_xgb)
mod_xgb <- caret::train(Target~.,
                        data = combined.data,
                        method = "xgbTree",
                        trControl = train_ctrl,
                        metric = "logLoss",
                        tuneGrid = xgbGrid)
stopCluster(cl)

mod_xgb$bestTune
plot(varImp(mod_xgb))

varImp(mod_xgb, scale = FALSE)$importance %>% 
  mutate(names = row.names(.)) %>% 
  filter(Overall > 0) %>% 
  arrange(-Overall) -> var_xgb


pred_xgb_p <- predict(mod_xgb, newdata = combined.data, type = "prob")
pred_xgb_p %>% 
  rename(pred = yes) -> pred_xgb_p
pred_xgb_p$obs <- train$Target


mnLogLoss(pred_xgb_p, lev = levels(pred_xgb_p$obs))


## Ensemble results: final
pred1V <- predict(mod_gbm_1, test_new, "prob")
pred2V <- predict(mod_nnet_1, test_new, "prob")
pred3V <- predict(mod_svm_1, test_new, "prob")
pred4V <- predict(mod_ada_1, test_new, "prob")
pred5V <- predict(mod_gam_1, test_new, "prob")

combined.data <- data.frame(pred1=pred1V$yes, pred2=pred2V$yes, pred3=pred3V$yes, 
                            pred4=pred4V$yes, pred5=pred5V$yes)

final_res <- predict(mod_xgb, newdata = combined.data, type = "prob")

fnl_tbl <- data.frame(ID = test$ID, prob = final_res$yes)


xlsx::write.xlsx(fnl_tbl,"E:\\Study\\R Projects\\Common files\\Blood Donation\\xgb4.xlsx")











