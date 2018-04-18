# make a clean environment ----
rm(list=ls(all=TRUE))


# load packages ----
library(tidyverse)
library(caret)
library(caretEnsemble)


# find the best model -----

# set seed for reproducibility
seed_n <- 14
set.seed(seed_n)

# import the dataset
main_df <- read_rds("main_df_for_modelling.rds")

# some manipulations
main_df <-
  main_df %>%
  # remove result == d as there are very few draw results
  filter(result != "d") %>%
  # convert characters to factors
  mutate_if(is.character, as.factor) %>%
  #remove the rows with all numerical variables == NA
  anti_join(
    main_df %>%
      filter_at(vars(received.goals:odds), all_vars(is.na(.)))
    ) %>%
  # make a variable flagging test or train (as advised in the email)
  mutate(train_test = ifelse(date < as.Date("2017/01/01"), "train", "test")) %>%
  #remove draws, need to think about it later on
  select(-date, -rank, -last_date)

# have a look at the structure of the dataframe  
str(main_df)

# constructing test and train datasets
# build train dataframe
train_df <- 
  main_df %>%
  filter(train_test == "train") %>%
  # remove all remaining na
  select(-train_test)

# build test dataframe
test_df <- 
  main_df %>%
  filter(train_test == "test") %>%
  # remove train_test feature
  select(-train_test)

# preprocessing
pre_process_train <-
  # remove variables with near zero varaince, impute using knn, center and scale
  preProcess(
    train_df,
    method  = c("nzv", "knnImpute", "center", "scale")
)
  # apply the preprocess and make a new transformed dataframe
train_df_transformed <- predict(pre_process_train, train_df)
test_df_transformed <- predict(pre_process_train, test_df)

# remove all na
train_df_transformed <- 
  train_df_transformed %>% 
  drop_na()
test_df_transformed <- 
  test_df_transformed %>% 
  drop_na()

# model building
  # create custom indices: folds

folds <- createFolds(train_df_transformed$result, k = 5)

  # create reusable trainControl object: control
control <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = folds
)

  # fit glmnet model: model_glmnet
model_glmnet <- train(
  factor(result) ~ .,
  data = train_df_transformed,
  method = "glmnet",
  trControl = control
)

  # fit random forest: model_rf
model_rf <- train(
  factor(result) ~ .,
  data = train_df_transformed,
  metric = "ROC",
  method = "ranger",
  trControl = control,
  respect.unordered.factors = TRUE
)

  # fit svm model: model_svm
model_svm <- train(
  as.factor(result) ~ .,
  data = train_df_transformed,
  method = "svmPoly",
  trControl = control
)

  # fit nn model: model_nn
model_nn <- train(
  as.factor(result) ~ .,
  data = train_df_transformed,
  method = "nnet",
  trControl = control
)

  # fit nb model: model_nb
model_nb <- train(
  as.factor(result) ~ .,
  data = train_df_transformed,
  method = "nb",
  trControl = control
)

  # fit rpart model: model_rpart
model_rpart <- train(
  as.factor(result) ~ .,
  data = train_df_transformed,
  method = "rpart",
  trControl = control
)

# fit logistic model: model_logit
model_logit <- train(
  as.factor(result) ~ .,
  data = train_df_transformed,
  method = "LogitBoost",
  trControl = control
)

# fit xgboost model: model_xgboost
model_xgboost <- train(
  as.factor(result) ~ .,
  data = train_df_transformed,
  method = "xgbDART",
  trControl = control
)

 # create model_list
model_list <- 
  list(
    glmnet    = model_glmnet,
    rf        = model_rf,
    svm       = model_svm,
    nn        = model_nn,
    nb        = model_nb,
    rpart     = model_rpart,
    logistic  = model_logit,
    xgboost   = model_xgboost)

  # pass model_list to resamples(): resamples
resamples <- resamples(model_list)
resamples

  # summarize the results
summary(resamples)

  # plot the model performance
dotplot(resamples, metric = "ROC")

  # predict using the best model
best_model_pred <- predict(model_glmnet, test_df_transformed)
plot(model_glmnet)

  # compare predicted outcome and true outcome
confusionMatrix(best_model_pred, test_df_transformed$result)

importance <- varImp(model_glmnet, scale = FALSE)
plot(importance)


# tune the best model ------
# set seed for reproducibility
set.seed(seed_n)

# Tune the glmnet model
glm_grid <- 
  expand.grid(
    alpha = 0:1,
    lambda = seq(0.0001, 0.2, length = 100)
  )

model_glmnet_tuned <- train(
  factor(result) ~ .,
  data = train_df_transformed,
  method = "glmnet",
  tuneGrid = glm_grid,
  trControl = control
)

# create model_list
model_list_2 <- 
  list(
    glmnet       = model_glmnet,
    rf           = model_rf,
    svm          = model_svm,
    nn           = model_nn,
    nb           = model_nb,
    rpart        = model_rpart,
    logistic     = model_logit,
    xgboost      = model_xgboost,
    glmnet_tuned = model_glmnet_tuned)

# pass model_list_2 to resamples(): resamples_2
resamples_2 <- resamples(model_list_2)
resamples_2

# summarize the results
summary(resamples_2)

# plot the model performance
dotplot(resamples_2, metric = "ROC")

plot(model_glmnet_tuned)

tuned_best_model_pred <- predict(model_glmnet_tuned, test_df_transformed)
confusionMatrix(tuned_best_model_pred, test_df_transformed$result)

# tuned glmnet model performs the same as the one we found previously!

# build ensemble models ----
# set seed for reproducibility
set.seed(seed_n)

# Ensemble
ensemble_model_list <- caretList(
  as.factor(result) ~ .,
  data = train_df_transformed,
  trControl = control,
  methodList = 
    c(
    "ranger",
    "glmnet"
    )
)

xyplot(resamples(ensemble_model_list))
modelCor(resamples(ensemble_model_list))

# find a good linear combination of rf and glmnet model: linear greedy optimization
greedy_ensemble <- caretEnsemble(
  ensemble_model_list, 
  metric = "ROC",
  trControl =
    trainControl(
    number = 2,
    summaryFunction = twoClassSummary,
    classProbs = TRUE
  ))

summary(greedy_ensemble)
# ensemble model is a bit better than the glmnet!

# predict using the ensemble model
ensemble_pred <- predict(greedy_ensemble, test_df_transformed)
# compare predicted outcome and true outcome
confusionMatrix(ensemble_pred, test_df_transformed$result)

# ensemble model predict 70% of the AFL results in 2017 correctly!
