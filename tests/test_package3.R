data(BreastCancer, package = "mlbench")
y <- BreastCancer$Class
x <- BreastCancer[setdiff(names(BreastCancer),c("Id","Class"))]
# remove missing values
id <- which(apply(x,1,function(z) sum(is.na(z)))>0)
y <- y[-id]
x <- as.matrix(x[-id,])
x <- apply(x,2,as.numeric)

require(caret)

test_swag <- swag(
  # arguments for swag
  x = x, y = y, control = swagControl(alpha=.5,verbose=TRUE),
  # arguments for caret
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F),
  metric = "Accuracy",
  method = "glmnet",
  tuneGrid=expand.grid(alpha = 1, lambda = seq(0,.35,length.out=10)),
  family="binomial",
  # modify arguments for caret
  caret_args_dyn = function(list_arg,iter){
    if(iter==1){
      list_arg$method = "glm"
      list_arg$tuneGrid = NULL
    }
    list_arg
  }
)
