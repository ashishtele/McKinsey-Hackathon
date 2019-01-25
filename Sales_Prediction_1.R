
library(mlr)

glimpse(train_1)
glimpse(test_1)

## create mlr task and convert factors to dummy features
trainTask = makeRegrTask(data = train_1, target = "sales")
trainTask = createDummyFeatures(trainTask)
testTask = makeRegrTask(data = test_1, target = "sales", fixup.data = "no", check.data = FALSE)
testTask = createDummyFeatures(testTask)

## create mlr learner
set.seed(931992)
lrn = makeLearner("regr.xgboost")
lrn$par.vals = list(
  print.every.n       = 50,
  objective           = "reg:linear"
)


# 1) Define the set of parameters you want to tune
ps = makeParamSet(
  makeIntegerParam("nthread",lower=12,upper=15),
  makeIntegerParam("nrounds",lower=100,upper=500),
  makeIntegerParam("max_depth",lower=10,upper=12),
  makeNumericParam("lambda",lower=0.1,upper=0.15),
  makeNumericParam("lambda_bias",lower=0.70,upper=0.75),
  makeNumericParam("gamma",lower=0,upper=1),
  makeNumericParam("eta", lower = 0.25, upper = 0.30),
  makeNumericParam("colsample_bytree", lower = 0.75,upper=0.90),
  makeNumericParam("subsample", lower = 0.75, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=10)
)

# Op. pars: nthread=13; nrounds=493; 
# max_depth=11; lambda=0.103; 
# lambda_bias=0.721; gamma=0.0264; eta=0.25;
# colsample_bytree=0.893; subsample=0.758; min_child_weight=7.87


# # 2) Use 5-fold Cross-Validation to measure improvements
rdesc = makeResampleDesc("CV", iters = 5L)
# # 3) Here we use Random Search (with 10 Iterations) to find the optimal hyperparameter
ctrl =  makeTuneControlRandom(maxit = 5L)


library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())
# https://github.com/berndbischl/parallelMap/issues/21
parallelStop()
parallelStartSocket(4)

# # 4) now use the learner on the training Task with the 5-fold CV to optimize your set of parameters and evaluate it with SQWK
res = tuneParams(lrn,
                 task = trainTask, 
                 resampling = rdesc,
                 par.set = ps, 
                 control = ctrl, 
                 measures = rmse)
x <- res$x
# # 5) set the optimal hyperparameter

best <- list(nthread=13, nrounds=493, 
   max_depth=11, lambda=0.103, 
   lambda_bias=0.721, gamma=0.0264, eta=0.25,
   colsample_bytree=0.893, subsample=0.758,
   min_child_weight=7.87)
best

lrn = setHyperPars(lrn, 
                   par.vals = best)

# perform crossvalidation in parallel
cv = crossval(lrn, 
              trainTask, 
              iter = 5, 
              measures = rmse, 
              show.info = TRUE)

## now train the model on all training data
tr = train(lrn, trainTask)

## predict using the optimal cut-points 
pred = predict(tr, testTask)
pred_1$pred_mlr <-  pred$data$response

# residuals
ggplot(pred_1)+
  geom_line(aes(x = c(1:180), (original - pred_mlr)))

resd <- (pred_1$original - pred_1$pred_mlr) %>% 
  ts()
#Box.test(resd, lag = 1, type = "Lj")

yardstick::rmse(pred_1,original, pred_mlr)

ggplot(pred_1) +
  geom_line(aes(x = c(1:180),original, color = "red"))+
  geom_line(aes(x = c(1:180),pred_mlr, color = "blue"))


## Prophet

library(prophet)

offdays <- prophet_df %>% 
  filter(type == "train") %>% 
  filter(is_national == 1) %>% 
  mutate(ds = date) %>% 
  mutate(holiday = paste0("off_day_", seq_along(1:length(ds)))) %>% 
  select(ds, holiday) %>% 
  data.frame()

glimpse(offdays)

tr_1 <- prophet_df %>% 
  filter(type == "train") %>% 
  select(date, sales) %>% 
  rename(ds = date, y = sales)
te_1 <- prophet_df %>% 
  filter(type == "test") %>% 
  select(date, sales) %>% 
  rename(ds = date)


prop_model <- prophet(tr_1,
                      growth = "linear",
                      n.changepoints = 150,
                      weekly.seasonality = TRUE,
                      yearly.seasonality = TRUE,
                      daily.seasonality = TRUE,
                      holidays = offdays,
                      seasonality.mode = "multiplicative")
forecast_test <- predict(prop_model, te_1)


forecast_test <- forecast_test %>% 
  mutate(ds = as.Date(ds))


data <- te_1 %>% 
  left_join(forecast_test, by = c("ds")) %>% 
  select(ds, sales, yhat)
yardstick::rmse(data, sales, yhat)

#linear: 0.101
#multiplicative: 0.0967

ggplot(data)+
  geom_line(aes(ds, sales, color = "black"))+
  geom_line(aes(ds, yhat, color = "red"))



## ARIMA models

sls <- ts(tr_1$y, frequency = 12)
autoplot(sls)

ndiffs(sls)
nsdiffs(sls)

sls %>% diff(lag = 6) %>% diff(lag = 4) %>% ggtsdisplay()

fir_auto <- auto.arima(sls)
checkresiduals(fir_auto)

sls %>% 
  Arima(order = c(3,1,3), seasonal = c(0,1,1)) -> fit1
fit2 <- auto.arima(sls)
fit2 %>% forecast(h=180) %>% autoplot()

sls %>% 
  Arima(order = c(3,0,3), seasonal = c(1,0,2)) %>% 
  forecast(h=180) %>% 
  autoplot()




