data(BreastCancer, package = "mlbench")
y <- BreastCancer$Class
x <- BreastCancer[setdiff(names(BreastCancer),c("Id","Class"))]
# remove missing values
id <- which(apply(x,1,function(z) sum(is.na(z)))>0)
y <- y[-id]
x <- as.matrix(x[-id,])
x <- apply(x,2,as.numeric)

require(caret)

# Test with glmnet
test_swag_glmnet <- swag(
  # arguments for swag
  x = x, y = y, control = swagControl(alpha=.5,verbose=TRUE),
  # arguments for caret
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F),
  metric = "Accuracy",
  method = "glmnet",
  tuneGrid=expand.grid(alpha = 1, lambda = seq(0,.35,length.out=10)),
  family="binomial",
  # dynamically modify arguments for caret
  caret_args_dyn = function(list_arg,iter){
    if(iter==1){
      list_arg$method = "glm"
      list_arg$tuneGrid = NULL
    }
    list_arg
  }
)
# best prediction
predict(test_swag_glmnet)
# predictions below a given CV error
predict(test_swag_glmnet,type="cv_performance",cv_performance = 0.05)
# predictions for a given dimension
predict(test_swag_glmnet,type="attribute",attribute = 4)


# Test with random forest
test_swag_rf <- swag(
  # arguments for swag
  x = x, y = y, control = swagControl(alpha=.5,verbose=TRUE),
  # arguments for caret
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F),
  metric = "Accuracy",
  method = "rf",
  # dynamically modify arguments for caret
  caret_args_dyn = function(list_arg,iter){
    list_arg$tuneGrid = expand.grid(.mtry=sqrt(iter))
    list_arg
  }
)

# ### RF case ###
#
# trial <- swagControl(pmax = 3,alpha = 0.3,m = 100,seed = 163L, verbose = T)
#
# metric <- "Accuracy"
#
# trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#
# # Hyperparameters for RF
#
# mtry <- sqrt(ncol(x)) #usual parameter
#
# #mtry <- 1 for first dimension
#
# tunegrid <- expand.grid(.mtry=mtry)
#
# # Comment: mtry should increase at every iteration by calculating the sqrt(ncol(x)) --> also tunegrid
#
# # must change accordingly. For example mtry <- 1 at dimension 1 and tunegrid expand the grid around.
#
# try_obj <- swag(x = as.matrix(x), y = y,control = trial, auto_control = F,method = "rf", metric = metric, trControl=trctrl, tuneGrid=tunegrid )
#
#
#
#
#
#
#
