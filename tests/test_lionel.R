require(swag)
require(caret)
require(doParallel)

swagcon <- swagControl(pmax = 4L, # nombre de dimension
                       alpha = 0.01, # quantile
                       m = 4e4, # nombre de modèles
                       seed = 163L, #for replicability
                       verbose = T #keeps track of completed dimensions
)


cl <- makeCluster(6L)
registerDoParallel(cl)

t1 <- Sys.time()
train_swag_svmr <- swag(
  # arguments for swag
  x = x_train,
  y = y_train,
  control = swagcon,
  auto_control = FALSE,
  # arguments for caret
  trControl = caret::trainControl(method = "repeatedcv", number = 10,
                                  repeats = 1, allowParallel = TRUE),
  metric = "Accuracy",
  method = "svmRadial",
  preProcess = c("center", "scale")
)
t2 <- Sys.time()

stopCluster(cl)

difftime(t2,t1,units="secs")

train_pred <- predict(train_swag_svmr,
                      newdata = x_test,
                      type="cv_performance",
                      cv_performance = 0.035)

pred_best <- predict(train_swag_svmr,
                      newdata = x_test)
