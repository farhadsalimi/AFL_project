# make a clean environment
rm(list=ls(all=TRUE))

# load packages----
library(tidyverse)
library(caret)
library(caretEnsemble)

# modelling -----
# set seed for reproducibility
set.seed(14)

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
  respect.unordered.factors = TRUE,
  tuneLength = 10
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

 # create model_list
model_list <- 
  list(
    glmnet = model_glmnet,
    rf     = model_rf,
    svm    = model_svm,
    nn     = model_nn,
    nb     = model_nb,
    rpart  = model_rpart)

  # pass model_list to resamples(): resamples
resamples <- resamples(model_list)
resamples

  # summarize the results
summary(resamples)

  # plot the model performance
dotplot(resamples, metric = "ROC")

  # predict using the best model
best_model_pred <- predict(model_rf, test_df_transformed)

  # compare predicted outcome and true outcome
confusionMatrix(best_model_pred, test_df_transformed$result)

importance <- varImp(model_glmnet, scale = FALSE)
plot(importance)

#Ensemble
# model_list <- caretList(
#   as.factor(result) ~ .,
#   data = train_df,
#   trControl = control,
#   methodList = c("ranger", "svmPoly")
# )
# 
# xyplot(resamples(model_list))
# modelCor(resamples(model_list))
# 
# glm_ensemble <- caretStack(
#   model_list,
#   method="glm",
#   metric="ROC",
#   trControl=trainControl(
#     method="boot",
#     number=10,
#     savePredictions="final",
#     classProbs=TRUE,
#     summaryFunction=twoClassSummary
#   )
# )
# 
# # predict using the best model
# ensemble_pred <- predict(glm_ensemble, test_df)
# # compare predicted outcome and true outcome
# confusionMatrix(ensemble_pred, test_df$result)
