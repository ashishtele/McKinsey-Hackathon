### Classification using Keras 
rm(list = ls())

# Load libraries
library(keras)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)

# Importing the train and test files

train_1 <- fread("E:\\Study\\R Projects\\Common files\\Blood Donation\\train.csv")
test_1 <- fread("E:\\Study\\R Projects\\Common files\\Blood Donation\\test.csv")

glimpse(train_1)
glimpse(test_1)

colnames(train_1) <- c('ID','Last_don','no_of_don','tot_vol','first_don','Target')
colnames(test_1) <- c('ID','Last_don','no_of_don','tot_vol','first_don')

###### Investigate and explore the training set

ggpairs(train_1) # need to remove one varible due to high correlation

names(train_1)

train_1 %>% 
  mutate(rule1 = (ifelse(Last_don <= 9 & tot_vol > 1000 & first_don <= 53,1,0)),
         rule2 = (ifelse(no_of_don > 1 & Last_don <= 6,1,0)),
         rule3 = (ifelse( Last_don <= 9 & no_of_don > 3,1,0)),
         rule4 = (ifelse( no_of_don > 4 & Last_don <= 9 & first_don <= 51,1,0))) -> train_1

train_1 %>% 
  select(-ID,-tot_vol) %>% 
  select(Target, everything()) -> train_1

train_1 %>% 
  mutate(rate = no_of_don/first_don,
         lst_don = Last_don/no_of_don,
         retrn = 1/rate - Last_don,
         stop = Last_don/first_don) -> train_1

glimpse(train_1)
ggpairs(train_1[,-1])

# test data set changes
test_1 %>% 
  mutate(rule1 = (ifelse(Last_don <= 9 & tot_vol > 1000 & first_don <= 53,1,0)),
         rule2 = (ifelse(no_of_don > 1 & Last_don <= 6,1,0)),
         rule3 = (ifelse( Last_don <= 9 & no_of_don > 3,1,0)),
         rule4 = (ifelse( no_of_don > 4 & Last_don <= 9 & first_don <= 51,1,0))) -> test_1

test_1 %>% 
  select(-ID,-tot_vol) %>% 
  select(everything()) -> test_1

test_1 %>% 
  mutate(rate = no_of_don/first_don,
         lst_don = Last_don/no_of_don,
         retrn = 1/rate - Last_don,
         stop = Last_don/first_don) -> test_1


# Split test/training sets
set.seed(100)
train_test_split <- initial_split(train_1, prop = 0.8)
train_test_split

# Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)

## recipe steps

rcp_obj <- recipe(Target~., data = train_tbl) %>% 
  step_BoxCox(all_numeric(),limits = c(-2,2)) %>% 
  step_center(all_predictors(),-all_outcomes()) %>% 
  step_scale(all_predictors(),-all_outcomes()) %>% 
  prep(data = train_tbl)
rcp_obj

# baked items

x_train_tbl <- bake(rcp_obj, newdata = train_tbl)
x_test_tbl <- bake(rcp_obj, newdata = test_tbl)

x_complete <- bind_rows(x_train_tbl, x_test_tbl)

y_train_vec <- x_train_tbl$Target  
y_test_vec <- x_test_tbl$Target  

y_complete <- c(y_train_vec, y_test_vec)

# delete the original columns

x_test_tbl$Target <- NULL
x_train_tbl$Target <- NULL  
x_complete$Target <- NULL
##################################### MLP build ####################################

model_keras <- keras_model_sequential()

model_keras %>% 
  layer_dense(
    units = 32,
    kernel_initializer = "uniform",   # way to set the initial random weights of keras layers
    activation = "relu",
    input_shape = ncol(x_train_tbl)
  ) %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(
    units = 16,
    kernel_initializer = "uniform",   # way to set the initial random weights of keras layers
    activation = "relu"
  ) %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(
    units = 1,
    kernel_initializer = "uniform",   # way to set the initial random weights of keras layers
    activation = "sigmoid"
  ) %>% 
  compile(
    optimizer = 'adam',
    loss = 'binary_crossentropy',
    metrics = c('accuracy')
  )

model_keras

# model fitting to training data

fit_keras <- fit(
  object = model_keras,
  x = as.matrix(x_complete),
  y = y_complete,
  batch_size = 10,
  epochs = 50,
  validation_split = 0.2
)

plot(fit_keras)

fit_keras

# preodict class

yhat_class <- predict_classes(object = model_keras,
                              x = as.matrix(test_1)) %>%  as.vector()

yhat_class_p <- predict_proba(object = model_keras,
                              x = as.matrix(test_1)) %>%  as.vector()

# performance check

new_comp <- tibble(
  ID = test_1$ID,
  prob = yhat_class_p
)

xlsx::write.xlsx(new_comp,"E:\\Study\\R Projects\\Common files\\Blood Donation\\keras.xlsx")













