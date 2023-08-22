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

## ----control-swag, eval=T-----------------------------------------------------
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

