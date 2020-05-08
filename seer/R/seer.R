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
#' @description
#' @param y A \code{vector} of binary response variable.
#' @param X A \code{matrix} or \code{data.frame} of attributes
#' @param q0 A \code{numeric} value for the quantile of variable screening.
#' @param dmax A \code{double} representing the maximum number of attributes per learner.
#' @param m A \code{double} representing the maximum number learner per dimension explored
#' @param seed  An \code{integer} that controls the reproducibility.
#' @param tot_time An \code{integer} that gives the approximate total time allowed for computation (in seconds)
#' @return A \code{seer} object with the structure:
#' \describe{
#' \item{}{}
#' @author Gaetan Bakalli and Samuel Orso
#' @import caret
#' @import doParallel
#' @export
#' @examples
#'
seer <- function(y, X, learner = NULL, q0 = NULL, dmax = NULL, m = NULL,seed = 666,
                 parallel_comput = T, nc = NULL, tot_time = ){

  if(is.null(learner)){
    stop("No learning method specified. Please specify a `learner`")
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
  # Number of observatio
  n <- length(y)


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

  ## Screening step

  cv_errors <- times <- vector("numeric",p)
  #10 fold CV repeated 10 times as PANNING
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

  for(i in seq_len(p)){
    X <- as.matrix(x_train[,i])
    df <- data.frame(y,X)
    t1 <- Sys.time()
    learn <- train(y ~., data = df, method = learner, trControl=trctrl, preProcess = c("center", "scale"),tuneLength = 10)
    t2 <- Sys.time()
    cv_errors[i] = 1 - max(learn$results$Accuracy)
    times <- diff(t1,t2,units="secs")

  # is y a factor
  if(!is.factor(y)){
    y = is.factor(y)
  }



  CVs[[1]] <- cv_errors
  VarMat[[1]] <- seq_along(cv_errors)

  cv_errors <- cv_errors[!is.na(cv_errors)]

  IDs[[1]] <- which(cv_errors <= quantile(cv_errors,q0))

  ## Dimension from 2 to dmax
  ## Meta-parameter decision rule.
  # Quantile for attributes selection

  # Maximum number of attributes per learner
  if(is.null(dmax)){
    dmax <- ceiling(min(sum(y),n-sum(y))/p)
  }

  # Maximum number of learner per dimension
  if(is.null(m)){
    m <- tot_time / mean(times,na.rm=T) / dmax
  }

  if(is.null(q0)){
    # q0 is such that (approx) all models of dimension 2 are explored
    q0 <- (1 + sqrt(1 + 8 * m)) / 0.2e1 / ncol(X)
  }


  ## Seed
  set.seed(seed)
  graine <- sample.int(1e6,dmax)

  ## Object storage
  CVs <- vector("list",dmax)
  IDs <- vector("list",dmax)
  VarMat <- vector("list",dmax)





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

    if(nrow(var_mat)>mod_max){
      set.seed(graine[d]+1)
      VarMat[[d]] <- var_mat[sample.int(nrow(var_mat),mod_max),]
    }else{
      VarMat[[d]] <- var_mat
    }

    var_mat <- VarMat[[d]]

    cv_errors <- rep(NA,nrow(var_mat))

    for(i in seq_len(p)){
      rc <- var_mat[i,]
      seed <- graine[d] + i
      X <- as.matrix(x_train[,rc])
      breast_1 = data.frame(y,X)
      learn = train(y ~., data = df, method = learner, trControl=trctrl, preProcess = c("center", "scale"),tuneLength = 10)
      cv_errors[i] = 1 - max(learn$results$Accuracy)
    }
    stopCluster(cl)


    attr(cv_errors,"rng") <- NULL

    CVs[[d]] <- cv_errors
    cv1 <- quantile(cv_errors,probs=q0,na.rm=T)
    IDs[[d]] <- which(cv_errors<=cv1)
  }

  ## Define the seer sets of models
  # Dimension which minimize the median cv error at each dimesion
  mod_size_min = which.min(unlist(lapply(CVs[1:dmax], median)))
  #quantile of the 1% most predictive models
  treshold_seer_set = quantile(CVs[[mod_size_min]],seq(0, 1, 0.01))[2]

  # Vector of dimention of the model selected
  dim_model = 1:dmax

  # Find the index of model selected
  index_model_select = list()
  for(i in seq_along(dim_model)){
    index_model_select[[i]] = which(((CVs[[dim_model[i]]] <=treshold_seer_set)))
  }

  obj = list(pred_cv = CVs,
             model_evaluated = VarMat,
             model_selected = IDs)

  class(obj) = "seer"
  obj
  }
}
