# Copyright (C) 2020 Gaetan Bakalli, Samuel Orso and Cesare Miglioli
#
# This file is part of SWAG-R Package

#' @title Predict method for SWAG
#'
#' @description Gives predictions for different \code{\link[caret]{train}}
#' learners obtained by \code{swag}.
#' @param object An object of class \code{\link{swag}}.
#' @param newdata an optional set of data to predict on. If \code{NULL}
#' the original training data are used.
#' @param type type of prediction required. The default is "best", it takes
#' the best model (with lowest CV errors). The option "cv_performance"
#' (which requires \code{cv_performance}) allows
#' to set a level of CV errors under which models are predicted. The option
#' "attribute" (which requires \code{attribute}) allows to specify an attribute
#' at which models are predicted.
#' @param cv_performance a level of CV errors (between 0 and 1) combines with
#' \code{type} "cv_performance".
#' @param attribute an attribute combines with \code{type} "attribute".
#' @param ... Not used for the moment.
#' @return Predictions .
#' @details Currently the different \code{\link[caret]{train}}
#' learners are trained (again) to make the predictions.
#' @author Gaetan Bakalli, Samuel Orso and Cesare Miglioli
#' @import caret
#' @importFrom stats predict
#' @method  predict swag
#' @export
"predict.swag" <- function(object,
                         newdata = NULL,
                         type = c("best","cv_performance","attribute"),
                         cv_performance = NULL,
                         attribute = NULL,
                         ...){

  #---------------------
  ## Argument verification
  #---------------------
  type <- match.arg(type)
  if(!missing(newdata)){
    dim_x <- dim(newdata)
    if(is.null(dim_x) || length(dim_x) != 2) stop("`newdata` must be a two-dimensional object")
    if(dim_x[2] != ncol(object$x)) stop("dimensions of `x` and `newdata` does not match")
  }

  #---------------------
  ## Model construction
  #---------------------
  # Determine which model to predict
  # construct a list "var_mat" where each element
  # of the list is a model
  if(type == "best"){
    id_attr <- which.min(sapply(object$CVs,min))
    id_lear <- which.min(object$CVs[[id_attr]])
    var_mat <- list(object$VarMat[[id_attr]][,id_lear])
  }

  if(type == "cv_performance"){
    if(missing(cv_performance)) stop("Please specifiy a CV errors level.")
    if(cv_performance > 1 || cv_performance <= 0) stop("Please choose a CV error level
                                                       between 0 and 1.")
    id_attr <- which(sapply(object$CVs,function(x)any(x<=cv_performance)))
    var_mat <- list()
    for(i in seq_along(id_attr)){
      id_lear <- which(object$CVs[[id_attr[i]]]<=cv_performance)
      var_mat <- c(var_mat,
                   lapply(seq_along(id_lear), function(j)
                     object$VarMat[[id_attr[i]]][,id_lear[j]])
                   )
    }
  }

  if(type == "attribute"){
    if(missing(attribute)) stop("Please specifiy an attribute.")
    if(length(object$CVs)<attribute) stop("Please choose an appropriate attribute.")
    var_mat <- lapply(seq_len(ncol(object$VarMat[[attribute]])), function(j) object$VarMat[[attribute]][,j])
  }

  #---------------------
  ## Predictions
  #---------------------
  if(missing(newdata)) n <- length(object$y) else n <- nrow(newdata)
  predictions <- matrix(nrow=n,ncol=length(var_mat))

  ## Seed
  set.seed(object$control$seed)
  graine <- sample.int(1e6,object$control$pmax)

  ## Caret arguments
  args_caret <- object$args_caret
  args_caret$y <- object$y
  if(!is.null(object$caret_args_dyn)) args_caret2 <- args_caret

  for(i in seq_along(var_mat)){
    d <- length(var_mat[[i]])

    # Modify dynamically arguments for `caret::train`
    if(!is.null(object$caret_args_dyn)) args_caret <- object$caret_args_dyn(args_caret2,d)

    # select the variable
    args_caret$x <- as.data.frame(object$x[,var_mat[[i]]])

    # train learner
    j <- which(colSums(var_mat[[i]] == object$VarMat[[d]])==d)
    set.seed(graine[d]+j)
    learn <- do.call(train,args_caret)

    # save prediction
    x_test <- NULL
    if(!missing(newdata)) x_test <- as.data.frame(newdata[,var_mat[[i]]])
    predictions[,i] <- predict(learn,newdata=x_test)
  }

  list(predictions=predictions,models=var_mat)
}
