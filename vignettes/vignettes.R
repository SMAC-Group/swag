## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=F------------------------------------------------------------------
#  
#  install.packages("devtools", repos = "http://cran.us.r-project.org")
#  
#  library(devtools)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  install_github("SMAC-Group/SWAG-R-Package")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  library(SWAG-R-Package)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  install.packages("mlbench")
#  
#  library(mlbench)
#  
#  data(BreastCancer, package = "mlbench")
#  
#  y <- BreastCancer$Class # response variable
#  
#  X <- as.matrix(BreastCancer[setdiff(names(BreastCancer),c("Id","Class"))]) # matrix of predictors
#  
#  # remove missing values
#  
#  id <- which(apply(X,1,function(x) sum(is.na(x)))>0)
#  y <- y[-id]
#  X <- X[-id,]
#  
#  

