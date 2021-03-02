## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE,include=FALSE,eval=TRUE---------------------------------------
# devtools::install_github("SMAC-Group/SWAG-R-Package")

library(swag) #load the new package

## ---- eval=F,echo=TRUE--------------------------------------------------------
#  remotes::install_github("SMAC-Group/SWAG-R-Package")
#  
#  library(swag) #load the new package

## ----BreastCancer, eval=T-----------------------------------------------------
# After having installed the mlbench package

data(BreastCancer, package = "mlbench")

# Pre-processing of the data
y <- BreastCancer$Class # response variable
x <- as.matrix(BreastCancer[setdiff(names(BreastCancer),c("Id","Class"))]) # features

# remove missing values and change to 'numeric'
id <- which(apply(x,1,function(x) sum(is.na(x)))>0)
y <- y[-id]
x <- x[-id,]
x <- apply(x,2,as.numeric)

# Training and test set
set.seed(180) # for replication
ind <- sample(1:dim(x)[1],dim(x)[1]*0.2)  
y_test <- y[ind]
y_train <- y[-ind]
x_test <- x[ind,]
x_train <-x[-ind,]

## ----caret, warning=FALSE-----------------------------------------------------
## if not installed
## install.packages("caret")
library(caret)

## ----control-swag, eval=T, cache = T------------------------------------------
# Meta-parameters chosen for the breast cancer dataset
swagcon <- swagControl(pmax = 4L, 
                       alpha = 0.5, 
                       m = 20L,
                       seed = 163L, #for replicability
                       verbose = T #keeps track of completed dimensions
                       )

# Given the low dimensional dataset, we can afford a wider search by fixing alpha = 0.5 as a smaller alpha may also stop the training procedure earlier than expected.

## ---- eval=FALSE, message=FALSE,warning=FALSE,echo=FALSE----------------------
#  library(caret) # swag is build around caret and uses it to train each learner

## ----SVM, eval=TRUE, warning=FALSE,message=FALSE, cache = T-------------------
## SVM Linear Learner
## `kernlab` is needed
## if not installed, install.packages("kernlab")
train_swag_svml <- swag(
  # arguments for swag
  x = x_train, 
  y = y_train, 
  control = swagcon,
  auto_control = FALSE,
  # arguments for caret
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F), # trainControl is from caret package
  metric = "Accuracy",
  method = "svmLinear",  # Use method = "svmRadial" to train this specific learner
  preProcess = c("center", "scale")
)

## ----CVs, eval=T--------------------------------------------------------------
train_swag_svml$CVs  

# A list which contains the cv training errors of each learner explored in a given dimension

## ----VarMat, eval=T-----------------------------------------------------------
train_swag_svml$VarMat 

# A list which contrains a matrix, for each dimension, with the attributes tested at that step 

## ----cv-alpha, eval= T--------------------------------------------------------
train_swag_svml$cv_alpha 

# The cut-off cv training error, at each dimension, determined by the choice of alpha

## ----lasso, eval=TRUE, cache =T-----------------------------------------------
## Lasso Learner
## `glmnet` is needed
## if not installed, install.packages("glmnet")
train_swag_lasso <- swag(
  # arguments for swag
  x = x, 
  y = y, 
  control = swagcon,
  auto_control = FALSE,
  # arguments for caret
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F), # trainControl is from caret package
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

## ----random-forest, eval=TRUE, cache = T--------------------------------------
## Random Forest Learner
## `randomForest` is needed
## if not installed, install.packages("randomForest")
train_swag_rf <- swag(
  # arguments for swag
  x = x, 
  y = y, 
  control = swagcon,
  auto_control = FALSE,
  # arguments for caret
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F), # trainControl is from caret package
  metric = "Accuracy",
  method = "rf",
  # dynamically modify arguments for caret
  caret_args_dyn = function(list_arg,iter){
    list_arg$tuneGrid = expand.grid(.mtry=sqrt(iter))
    list_arg
  }
)

## ---- eval=F, echo=FALSE------------------------------------------------------
#  # IN-SAMPLE
#  
#  # predictions below a given CV error in-sample
#  train_pred <- predict(train_swag_svml,
#                        newdata = x_train,
#                        type="cv_performance",
#                        cv_performance = 0.05)
#  
#  # predictions for a given dimension in-sample
#  train_pred_att <- predict(train_swag_svml,newdata = x_train,type="attribute",attribute = 4)
#  

## ----predictions, eval=T------------------------------------------------------
# best learner predictions 
# if `newdata` is not specified, then predict gives predictions based on the training sample

sapply(predict(object = train_swag_svml), function(x) head(x))

# best learner predictions 
best_pred <- predict(object = train_swag_svml, 
                     newdata = x_test)

sapply(best_pred, function(x) head(x))

# predictions for a given dimension 

dim_pred <-  predict(
  object = train_swag_svml, 
  newdata = x_test, 
  type = "attribute",
  attribute = 4L)


sapply(dim_pred,function(x) head(x))

# predictions below a given CV error

cv_pred <-  predict(
  object = train_swag_svml, 
  newdata = x_test, 
  type = "cv_performance",
  cv_performance = 0.04)

sapply(cv_pred,function(x) head(x))


## ----confusion-matrix, eval=T-------------------------------------------------
# transform predictions into a data.frame of factors with levels of `y_test`
best_learn <- factor(levels(y_test)[best_pred$predictions])
confusionMatrix(best_learn,y_test) # from caret package

