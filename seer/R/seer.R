# Copyright (C) 2018 Gaetan Bakalli, Samuel Orso
#
# This file is part of seer R Methods Package
#
# The `seer` R package is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# The `seer` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title SEER wrapper algorithm for ML method.
#'
#' @description SEER algo
#' @param y A \code{vector} of binary response variable.
#' @param X A \code{matrix} or \code{data.frame} of attributes
#' @param learner A \code{string} defining the learner type (method available: \code{"logistic", "svmLinear","svmRadial"} and \code{"rf"})
#' @param dmax A \code{double} representing the maximum number of attributes per learner.
#' @param m A \code{integer} representing the maximum number of learners per dimension explored.
#' @param q0 A \code{double} representing the proportion of screening.
#' @param parallel_comput  An \code{boolean} to allow for parallel computing.
#' @param seed  An \code{integer} that controls the reproducibility.
#' @param nc An \code{double} that specify the number of core for parallel computation.
#' @return A \code{seer} object with the structure:
#' \describe{
#' \item{}{}
#' }
#' @author Gaetan Bakalli and Samuel Orso
#' @import caret
#' @import doParallel
#' @export
#' @examples
#' seer()
seer <- function(y, X, learner = NULL, dmax = NULL, m = NULL, q0=0.01, seed = 163,
                 parallel_comput = T, nc = NULL, ...){


  if(is.null(learner)){
    stop("No learning method specified. Please specify a `learner`")
  }
  if(match(learner,c("logistic","rf","lasso","svmLinear","svmRadial"),unmatch=FALSE)){
      if(learner == "logistic"){
        learner = "glm"
        family = "binomial"
        metric = "Accuracy"
        tunegrid = NULL
        preprocess = NULL
        tuneLength = NULL
      }else if(learner == "rf"){
        family = NULL
        metric = "Accuracy"
        family = NULL
        preprocess = NULL
        tuneLength = NULL
      }else if(learner == "lasso"){
        family = NULL
        metric = "Accuracy"
        family = NULL
        preprocess = NULL
        tuneLength = NULL
      }else if(learner == "svmLinear"){
        family = NULL
        metric = "Accuracy"
        family = NULL
        preprocess = NULL
        tuneLength = NULL
      }else if(learner == "svmRadial"){
        family = NULL
        metric = "Accuracy"
        family = NULL
        preprocess = NULL
        tuneLength = NULL
      }
    }else{
    stop("This `learner` is not implemented.")
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

  ## object dimension
  # Number of attributes
  p <- ncol(X)
  # Number of observations
  n <- length(y)

  # is y a factor
  if(!is.factor(y)){
    y <- as.factor(y)
  }

  # Define parallelisation parameter
  if(isTRUE(parallel_comput)){
    if(is.null(nc)){
      nc = detectCores()
    }else{
      nc = nc
    }
    cl <- makePSOCKcluster(nc)
    registerDoParallel(cl)
  }

  # Maximum number of attributes per learner
  if(is.null(m)){
    m = 40000
  }

  # Maximum number of attributes per learner
  if(is.null(dmax)){
    event <- as.numeric(as.character(y))
    dmax <- ceiling(min(sum(event),n-sum(event))/p)
    if(dmax == 1){
      dmax <- 3
    }
  }

  ## Seed
  set.seed(seed)
  graine <- sample.int(1e6,dmax)

  ## Object storage
  CVs <- vector("list",dmax)
  IDs <- vector("list",dmax)
  VarMat <- vector("list",dmax)

  ## Screening step
  cv_errors <- times <- vector("numeric",p)
  #10 fold CV repeated 10 times as
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

  for(i in seq_len(p)){
    if(learner == "rf"){
      mtry <- 1
      tunegrid = expand.grid(.mtry=mtry)
    }

    x <- as.matrix(X[,i])
    df <- data.frame(y,x)
    learn <- train(y ~., data = df, method = learner,metric = metric, family = family,
                   trControl=trctrl, preProcess = preprocess, tuneLength = tuneLength,
                   tuneGrid=tunegrid)
    cv_errors[i] = 1 - max(learn$results$Accuracy)
  }


  CVs[[1]] <- cv_errors
  VarMat[[1]] <- seq_along(cv_errors)

  cv_errors <- cv_errors[!is.na(cv_errors)]


  ## Dimension from 2 to dmax
  ## Meta-parameter decision rule.
  # Quantile for attributes selection
  IDs[[1]] <- which(cv_errors <= quantile(cv_errors,q0))
  id_screening <- IDs[[1]]

  # Compute for d>1 to dmax

  for(d in 2:dmax){

    # cv0 <- cv1
    idRow <- IDs[[d-1]]
    if(d==2){
      idVar <- VarMat[[d-1]][idRow]
      nrv <- length(idVar)
    }else{
      idVar <- VarMat[[d-1]][idRow,]
      nrv <- nrow(idVar)
    }
    # build all possible
    A <- matrix(nr=nrv*length(id_screening),nc=d)
    A[,1:(d-1)] <- kronecker(cbind(rep(1,length(id_screening))),idVar)
    A[,d] <- rep(id_screening,each=nrv)
    B <- unique(t(apply(A,1,sort)))
    id_ndup <- which(apply(B,1,anyDuplicated) == 0)
    var_mat <- B[id_ndup,]
    rm(list=c("A","B"))

    if(nrow(var_mat)>m){
      set.seed(graine[d]+1)
      VarMat[[d]] <- var_mat[sample.int(nrow(var_mat),m),]
    }else{
      VarMat[[d]] <- var_mat
    }

    var_mat <- VarMat[[d]]

    cv_errors <- rep(NA,nrow(var_mat))
    if(learner == "rf"){
      mtry <- d
      tunegrid = expand.grid(.mtry=mtry)
    }

    for(i in seq_len(nrow(var_mat))){
      rc <- var_mat[i,]
      seed <- graine[d] + i
      x <- as.matrix(X[,rc])
      mtry <- sqrt(ncol(x))
      breast_1 = data.frame(y,x)
      learn = train(y ~., data = df, method = learner, metric = metric, family = family,
                    trControl=trctrl, preProcess = preprocess, tuneLength = tuneLength,
                    tuneGrid=tunegrid)
      cv_errors[i] <- 1 - max(learn$results$Accuracy)
    }

    CVs[[d]] <- cv_errors
    cv1 <- quantile(cv_errors,probs=q0,na.rm=T)
    IDs[[d]] <- which(cv_errors<=cv1)
  }

  stopCluster(cl)
  ## Define the seer sets of models
  # Dimension which minimize the median cv error at each dimesion
  mod_size_min_med = which.min(unlist(lapply(CVs[1:dmax], median)))
  #quantile of the 1% most predictive models
  treshold_seer_set = quantile(CVs[[mod_size_min_med]],seq(0, 1, 0.01))[2]

  # Vector of models dimension
  dim_model = 1:dmax

  # Find the index of model selected
  index_model_select = list()
  for(d in seq_len(dmax)){
    if(sum(CVs[[dim_model[d]]] <=treshold_seer_set) == 0){
      index_model_select[[d]] = "empty"
    }else{
      index_model_select[[d]] = which(CVs[[dim_model[d]]] <=treshold_seer_set)
    }
  }

  # vector od model dimension selected
  model_dim_selected = which(index_model_select != "empty")

  ###### SEER subset of model ######

  ## create output for SEER subset
  ## index of variable selected and respective cv error
  seer_model = list()
  seer_cv_error = list()

  for(d in seq_along(model_dim_selected)){
    index_mod = model_dim_selected[[d]]
    if(d == 1){
      seer_model[[d]] <- VarMat[[index_mod]][index_model_select[[index_mod]]]
    }else{
      seer_model[[d]] <- VarMat[[index_mod]][index_model_select[[index_mod]],]
    }
    seer_cv_error[[d]] <- CVs[[index_mod]][index_model_select[[index_mod]]]
  }

  #
  table_variable = table(unlist(seer_model))
  variable_index = as.numeric(names(table_variable))
  names(table_variable) = colnames(X[,variable_index])


  obj = list(pred_cv = CVs,
             model_evaluated = VarMat,
             model_selected = IDs,
             model_seer_set = index_model_select,
             table_variable = table_variable,
             variable_index = variable_index,
             seer_model = seer_model,
             seer_cv_error = seer_cv_error)

  class(obj) = "seer"
  obj
}
