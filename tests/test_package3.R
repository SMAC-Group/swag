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



#To Cesare
# the above does not work, you can look at code on vignette to split train/test the dataset

# 80/20 train/test split
set.seed(180) # for replication
ind <- sample(length(y),round(length(y) * 0.2)) # 80/20 split
y_test <- y[ind]
y_train <- y[-ind]
x_test <- x[ind,]
x_train <-x[-ind,]

# !This is not the same as in the vignette!
swagcon <- swagControl(pmax = 3,
                       alpha = 0.5,
                       m = 10,
                       seed = 163L, #for replicability
                       verbose = T #keeps track of completed dimensions
)

train_swag_svml <- swag(
  # arguments for swag
  x = x_train,
  y = y_train,
  control = swagcon,
  # arguments for caret
  trControl = caret::trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F),
  metric = "Accuracy",
  method = "svmLinear",
  preProcess = c("center", "scale")
)

predict(train_swag_svml,newdata=x_test)
