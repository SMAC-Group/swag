# Copyright (C) 2020 Gaetan Bakalli, Samuel Orso
#
# This file is part of swag R Methods Package
#
# The `swag` R package is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# The `swag` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title swag wrapper algorithm for ML method.
#'
#' @description swag algo
#' @param y A \code{vector} of binary response variable.
#' @param X A \code{matrix} or \code{data.frame} of attributes
#' @param learner A \code{string} defining the learner type (method available: \code{"logistic"}, \code{"svmLinear"},
#'  \code{"svmRadial"}, \code{"lasso"} and \code{"rf"})
#' @param dmax A \code{double} representing the maximum number of attributes per learner.
#' @param m A \code{integer} representing the maximum number of learners per dimension explored.
#' @param q0 A \code{double} representing the proportion of screening.
#' @param parallel_comput  An \code{boolean} to allow for parallel computing.
#' @param seed  An \code{integer} that controls the reproducibility.
#' @param nc An \code{double} that specify the number of core for parallel computation.
#' @param verbose A \code{boolean} for printing current progress of the algorithm.
#' @return A \code{swag} object with the structure:
#' \describe{
#' \item{}{}
#' }
#' @author Gaetan Bakalli and Samuel Orso
#' @import caret
#' @import doParallel
#' @import parallel
#' @export
#' @examples
#' swag()
swag <- function(y, X, learner = "logistic", dmax = NULL, m = NULL, q0=0.01, seed = 163,
                 parallel_comput = T, nc = NULL, verbose=FALSE, ...){


  if(is.null(learner)){
    stop("No learning method specified. Please specify a `learner`")
  }
  if(match(learner,c("rf","lasso","svmLinear","svmRadial"),nomatch=FALSE)){
      if(learner == "rf"){
        leaner_screen = learner
        family_screen = family = NULL
        metric = "Accuracy"
        family = NULL
        preprocess = NULL
        tuneLength = NULL
      }else if(learner == "lasso"){
        leaner_screen =  "glm"
        family_screen =  binomial()
        learner = "glmnet"
        family = "binomial"
        metric = "Accuracy"
        family = NULL
        preprocess = NULL
        tuneLength = NULL
      }else if(learner == "svmLinear"){
        leaner_screen = learner
        family_screen = family = NULL
        metric = "Accuracy"
        family = NULL
        preprocess = NULL
        tuneLength = NULL
      }else if(learner == "svmRadial"){
        leaner_screen = learner
        family_screen = family = NULL
        metric = "Accuracy"
        family = NULL
        preprocess = NULL
        tuneLength = NULL
      }
    }else{
    stop("This `learner` is not implemented.")
    }
  # Tunegrid for random forest
    if(learner == "rf"){
      mtry <- 1
      tunegrid = expand.grid(.mtry=mtry)
    }else{
      tunegrid = NULL
    }

  if(is.null(y)){
    stop("Please provide a response vector `y`")
  }

  if(is.null(X)){
    stop("Please provide an attributes matrix `X`")
  }else{
    if(!is.matrix(X)){
      stop("X must be a matrix")
    }
  }

  # Check missing observations (not supported currently)
  if(sum(is.na(y)) > 0 || sum(is.na(X)) > 0){
    stop("Please provide data without missing values")
  }

  ## object dimension
  # Number of attributes
  p <- ncol(X)
  # Number of observations
  n <- length(y)

  # is y a factor
  if(!is.factor(y)){
    y <- as.factor(y)
  }

  # verify y is binary
  if(nlevels(y)>2){
    stop("Please provide a binary response `y`")
  }

  # Define parallelisation parameter
  if(isTRUE(parallel_comput)){
    if(is.null(nc)){
      nc = parallel::detectCores()
    }
    cl <- parallel::makePSOCKcluster(nc)
    doParallel::registerDoParallel(cl)
  }

  # Maximum number of attributes per learner
  if(is.null(m)){
    m = 40000
  }

  # Maximum number of attributes per learner
  # If not user-defined, dmax is such that EPV is approx 5
  if(is.null(dmax)){
    event <- table(y)
    dmax <- ceiling(min(event[1],n-event[2])/5)

    # Explore at minima dmax=3 (if not specified by the user)
    if(dmax == 1){
      dmax <- 3
    }
  }

  # dmax should not exceed p
  if(dmax > p) dmax <- p


  ## Seed
  set.seed(seed)
  graine <- sample.int(1e6,dmax)

  ## Object storage
  CVs <- vector("list",dmax)
  IDs <- vector("list",dmax)
  VarMat <- vector("list",dmax)

  ## Screening step
  cv_errors <- rep(NA,p)
  #10 fold CV repeated 10 times as
  trctrl <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 10)

  # compute CV errors
  for(i in seq_len(p)){
    x <- as.matrix(X[,i])
    df <- data.frame(y,x)
    set.seed(graine[1]+i)
    learn <- caret::train(y ~., data = df, method = learner, metric = metric,
                          family = family, trControl=trctrl, preProcess = preprocess,
                          tuneLength = tuneLength, tuneGrid=tunegrid)
    cv_errors[i] = 1 - max(learn$results$Accuracy)
  }

  # Store results
  CVs[[1]] <- cv_errors
  VarMat[[1]] <- seq_along(cv_errors)
  dim(VarMat[[1]]) <- c(p,1)

  # Remove NA's
  cv_errors <- cv_errors[!is.na(cv_errors)]


  ## Dimension from 2 to dmax
  ## Meta-parameter decision rule.
  # Quantile for attributes selection
  cv1 <- quantile(cv_errors,q0)
  IDs[[1]] <- which(cv_errors <= cv1)
  id_screening <- IDs[[1]]
  if(verbose){
    print(paste0("Dimension explored: ",1," - CV errors at q0: ",round(cv1,4)))
  }
  # Compute for d>1 to dmax

  for(d in 2:dmax){
    # Tunegrid for random forest
    if(learner == "rf"){
      mtry <- d
      tunegrid = expand.grid(.mtry=mtry)
    }

    # cv0 <- cv1
    idRow <- IDs[[d-1]]
    idVar <- VarMat[[d-1]][idRow,]
    nrv <- nrow(idVar)
    if(is.null(nrv)) nrv <- length(idVar)

    # build all possible combinations
    A <- matrix(nr=nrv*length(id_screening),nc=d)
    A[,1:(d-1)] <- kronecker(cbind(rep(1,length(id_screening))),idVar)
    A[,d] <- rep(id_screening,each=nrv)
    B <- unique(t(apply(A,1,sort)))
    id_ndup <- which(apply(B,1,anyDuplicated) == 0)
    var_mat <- B[id_ndup,]
    rm(list=c("A","B"))

    if(is.null(dim(var_mat))) dim(var_mat) <- c(1,length(var_mat))

    # Reduce number of model if exceeding `m`
    if(nrow(var_mat)>m){
      set.seed(graine[d]-1)
      VarMat[[d]] <- var_mat[sample.int(nrow(var_mat),m),]
    }else{
      VarMat[[d]] <- var_mat
    }
    var_mat <- VarMat[[d]]

    cv_errors <- rep(NA,nrow(var_mat))

    for(i in seq_len(nrow(var_mat))){
      rc <- var_mat[i,]
      x <- as.matrix(X[,rc])
      mtry <- sqrt(ncol(x))
      df = data.frame(y,x)
      set.seed(graine[d] + i)
      learn <- caret::train(y ~., data = df, method = learner, metric = metric,
                            family = family, trControl=trctrl, preProcess = preprocess,
                            tuneLength = tuneLength, tuneGrid=tunegrid)
      cv_errors[i] <- 1 - max(learn$results$Accuracy)
    }

    CVs[[d]] <- cv_errors
    cv1 <- quantile(cv_errors,probs=q0,na.rm=T)
    IDs[[d]] <- which(cv_errors<=cv1)

    if(verbose){
      print(paste0("Dimension explored: ",d," - CV errors at q0: ",round(cv1,4)))
    }
  }

  parallel::stopCluster(cl)
  ## Define the swag sets of models
  # Dimension which minimize the median cv error at each dimesion
  mod_size_min_med = which.min(sapply(CVs, median))
  #quantile of the 1% most predictive models
  treshold_swag_set = quantile(CVs[[mod_size_min_med]],probs=0.01)

  # Vector of models dimension
  dim_model = 1:dmax

  # Find the index of model selected
  index_model_select = vector("list",dmax)
  for(d in seq_len(dmax)){
    if(sum(CVs[[d]] <= treshold_swag_set) == 0){
      index_model_select[[d]] = "empty"
    }else{
      index_model_select[[d]] = which(CVs[[d]] <= treshold_swag_set)
    }
  }

  # vector of model dimension selected
  model_dim_selected = which(index_model_select != "empty")

  ###### swag subset of model ######

  ## create output for swag subset
  ## index of variable selected and respective cv error
  swag_model = list()
  swag_cv_error = list()

  for(d in seq_along(model_dim_selected)){
    index_mod = model_dim_selected[[d]]
    swag_model[[d]] <- VarMat[[index_mod]][index_model_select[[index_mod]],]
    swag_cv_error[[d]] <- CVs[[index_mod]][index_model_select[[index_mod]]]
  }

  #
  table_variable = table(unlist(swag_model))
  variable_index = as.numeric(names(table_variable))
  names(table_variable) = colnames(X[,variable_index])

  obj = list(pred_cv = CVs,
             model_evaluated = VarMat,
             model_selected = IDs,
             model_swag_set = index_model_select,
             table_variable = table_variable,
             variable_index = variable_index,
             swag_model = swag_model,
             swag_cv_error = swag_cv_error,
             learner = learner,
             y_train = y,
             x_train = X)

  class(obj) = "swag"
  invisible(obj)
}


#' @title swag validation for ML method.
#'
#' @description swag algo
#' @param obj A \code{object} of of class \code{'swag'}.
#' @param X A \code{matrix} or \code{data.frame} of attributes
#' @param parallel_comput  An \code{boolean} to allow for parallel computing.
#' @param seed  An \code{integer} that controls the reproducibility.
#' @param nc An \code{double} that specify the number of core for parallel computation.
#' @return A \code{swag} object with the structure:
#' \describe{
#' \item{}{}
#' }
#' @author Gaetan Bakalli and Samuel Orso
#' @import caret
#' @import doParallel
#' @export
#' @examples
#' swag()

swag_valid <- function(obj, y_valid, X_valid, seed = 163){

  if(learner == "rf"){
    leaner_screen = learner
    family_screen = family = NULL
    metric = "Accuracy"
    family = NULL
    preprocess = NULL
    tuneLength = NULL
  }else if(learner == "glmnet"){
    leaner_screen =  "glm"
    family_screen =  binomial()
    learner = glmnet
    family = "binomial"
    metric = "Accuracy"
    family = NULL
    preprocess = NULL
    tuneLength = NULL
  }else if(learner == "svmLinear"){
    leaner_screen = learner
    family_screen = family = NULL
    metric = "Accuracy"
    family = NULL
    preprocess = NULL
    tuneLength = NULL
  }else if(learner == "svmRadial"){
    leaner_screen = learner
    family_screen = family = NULL
    metric = "Accuracy"
    family = NULL
    preprocess = NULL
    tuneLength = NULL
  }

  # Counting error list
  ce = list()
  # y for training and valid sample sample
  y_train <- as.factor(obj$y_train)
  y_valid <- as.factor(y_valid)

  # Options for learner
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

  pred_error_list = list()
  for(d in seq_along(obj$model_dim_selected)){
    # Matrix of selected variables at dimention d
    var_mat_select <- obj$swag_model[[d]]
    # Initialize vector of counting errors
    if(is.null(dim(obj$swag_model[[d]])) ){
      counting_error_svm = rep(NA,1)
      test_pred = matrix(NA,length(y_test),1)
      pb1 = 1
      n_mod_dim = 1
    }else{
      counting_error_svm = rep(NA,dim(var_mat_select)[1])
      test_pred = matrix(NA,length(y_test),dim(var_mat_select)[1])
      pb1 = dim(var_mat_select)[1]
      n_mod_dim = dim(var_mat_select)[1]
    }
    # Store the vector of prediction
    nc = detectCores()
    cl <- makeCluster(nc)
    registerDoParallel(cl)

    pb <- txtProgressBar(min = 0, max = pb1, style = 3)
    for(j in 1:n_mod_dim){
      # Index of selected covariate of model j

      rc <- var_mat_select[j,]
      # New X train matrix and create data frame for svm computation
      X1 <- as.matrix(obj$x_train[,rc])
      df = data.frame(y_train,X1)
      # Svm object
      learn = train(y ~., data = df, method = learner, metric = metric, family = family,
                         trControl=trctrl, preProcess = preprocess, tuneLength = tuneLength,
                         tuneGrid=tunegrid)

      ## Counting errors and confusion matrix
      # X test matrix and create data frame for svm computation
      X = X_valid[,rc]
      df1 <- data.frame(yte, X2)
      # create the data matrix for prediction storage
      df2 <- data.frame(X2)
      # make precition out-of-sample
      test_pred[,j] <- predict(svm_radial, newdata = df2)
      # Store confusion matrix
      obj2 = confusionMatrix(table(predict(svm_radial, newdata = df2), df1$yte))
      # Compute the counting error
      counting_error_svm[j] = sum(as.vector(obj2$table)[c(2,3)])
      setTxtProgressBar(pb, j)
    }
    stopCluster(cl)
    ## Sort the signs and extract the quantile when it changes
    # List of counting error for various model dimention
    ce[[d]] = counting_error_svm
    # List of matrices of prediction
    pred_error_list[[d]] =  test_pred
    print(d)
  }

  count_error = unlist(ce)
  # Majority rule svm
  mat_test_pred = do.call(cbind,test_pred_list)
  #mat_test_pred[mat_test_pred == 1] = 0
  #mat_test_pred[mat_test_pred == 2] = 1
  pred_mod_av = (round(apply(mat_test_pred,1,mean)))
  # counting error model averaging
  mode_av_ce = abs(sum(pred_mod_av-as.numeric(y_test)))
  out = structure(list(ma_ce = mode_av_ce,
                       mat_test_pred = mat_test_pred,
                       ce_all = count_error))
  invisible(out)
}
