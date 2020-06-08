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

# Test with linear svm

test_swag_svml <- swag(
  # arguments for swag
  x = x, y = y, control = swagControl(alpha=.5,verbose=TRUE),
  # arguments for caret
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F),
  metric = "Accuracy",
  method = "svmLinear", preProcess = c("center", "scale"),tuneLength = 10
)

# Test with radial svm

test_swag_svmr <- swag(
  # arguments for swag
  x = x, y = y, control = swagControl(alpha=.5,verbose=TRUE),
  # arguments for caret
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F),
  metric = "Accuracy",
  method = "svmRadial", preProcess = c("center", "scale"),tuneLength = 10
)


