# Copyright (C) 2020 Gaetan Bakalli, Samuel Orso and Cesare Miglioli
#
# This file is part of SWAG-R Package
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

#' @title Spare Wrapper AlGorithm (swag)
#'
#' @description \code{swag} trains a meta-learning procedure that combines
#' screening and wrapper methods to find a set of extremely low-dimensional attribute
#' combinations. \code{swag} works on top of the \pkg{caret} package and proceeds in a
#' forward-step manner. More specifically, it builds and tests learners starting
#' from very few attributes until it includes a maximal number of attributes by
#' increasing the number of attributes at each step. Hence, for each fixed number
#' of attributes, the algorithm tests various (randomly selected) learners and
#' picks those with the best performance in terms of training error. Throughout,
#' the algorithm uses the information coming from the best learners at the previous
#' step to build and test learners in the following step. In the end, it outputs
#' a set of strong low-dimensional learners.
#' @param x A \code{matrix} or \code{data.frame} of attributes
#' @param y A \code{vector} of binary response variable.
#' @param control see \code{\link[swag]{swagControl}}
#' @param auto_control A \code{boolean}, whether some control parameters
#' are adjusted depending on \code{x} and \code{y} (see \code{\link[swag]{swagControl}}).
#' @param caret_args_dyn If not null, a function that can modify arguments for
#' \code{\link[caret]{train}} dynamically (see the details).
#' @param ... Arguments to be passed to \code{\link[caret]{train}} functions (see the details).
#' @return A \code{swag} object.
#' @details Currently we expect the user to replace \code{...} with the arguments one would
#' use for \code{\link[caret]{train}}. This requires to know how to use \code{\link[caret]{train}}
#' function. If \code{...} is left unspecified, default values of \code{\link[caret]{train}}
#' are used. But this might lead to unexpected results.
#'
#' The function \code{caret_args_dyn} is expected to take as a first
#' argument a \code{list} with all arguments for \code{\link[caret]{train}}
#' and as a second argument the number of attributes (see examples).
#' @author Gaetan Bakalli, Samuel Orso and Cesare Miglioli
#' @importFrom caret train
#' @importFrom caret trainControl
#' @import caret
#' @export swag
swag <- function(x,
                 y,
                 control = swagControl(),
                 auto_control = TRUE,
                 # arguments for `caret::train()`
                 caret_args_dyn = NULL,
                 ...){

  #---------------------
  ## Verify the arguments
  #---------------------
  if(missing(x)) stop("Please provide a `x`")
  if(missing(y)) stop("Please provide a response vector `y`")

  # Check missing observations (not supported currently)
  if(sum(is.na(y)) > 0 || sum(is.na(x)) > 0)
    stop("Please provide data without missing values")

  # is y a factor
  if(!is.factor(y)) y <- as.factor(y)

  # verify y is binary
  if(nlevels(y)>2) stop("Please provide a binary response `y`")

  ## object dimension
  # Number of attributes
  dim_x <- dim(x)
  if(is.null(dim_x) || length(dim_x) != 2) stop("`x` must be a two-dimensional object")
  if(dim_x[1] != length(y)) stop("dimensions of `x` and `y` does not match")
  p <- dim_x[2]
  n <- dim_x[1]

  if(class(control) != "swagControl")
    stop("`control` must be of class `swagControl`")

  if(isTRUE(auto_control)) control <- auto_swagControl(x,y,control)

 # Existence of arguments for `caret::train()`
  # with default values
  args_caret <- list(...)
  if(is.null(args_caret$method)) args_caret$method = "rf"
  if(is.null(args_caret$preProcess)) args_caret$preProcess = NULL
  if(is.null(args_caret$weights)) args_caret$weights = NULL
  if(is.null(args_caret$metric)) args_caret$metric = ifelse(is.factor(y), "Accuracy", "RMSE")
  if(is.null(args_caret$maximize)) args_caret$maximize = ifelse(args_caret$metric %in% c("RMSE", "logLoss", "MAE"), FALSE, TRUE)
  if(is.null(args_caret$trControl)) args_caret$trControl = trainControl()
  if(is.null(args_caret$tuneGrid)) args_caret$tuneGrid = NULL
  if(is.null(args_caret$tuneLength)) args_caret$tuneLength = ifelse(args_caret$trControl$method == "none", 1, 3)

  # Add `y` to the list of arguments
  args_caret$y <- y

  # Make a copy
  if(!missing(caret_args_dyn)) args_caret2 <- args_caret
  #---------------------
  ## General parameters
  #---------------------
  ## Seed
  set.seed(control$seed)
  graine <- sample.int(1e6,control$pmax)

  ## Object storage
  CVs <- vector("list",control$pmax)
  IDs <- vector("list",control$pmax)
  VarMat <- vector("list",control$pmax)
  cv_alpha <- rep(NA,control$pmax)

  #---------------------
  ## SWAG
  #---------------------
  for(d in seq_len(control$pmax)){
    # Build all combinations
    if(d == 1){
      var_mat <- seq_len(p)
      dim(var_mat) <- c(1,p)
    }else{
      var_mat <- model_combination(id_screening,subset(VarMat[[d-1L]],select=IDs[[d-1]]))
    }

    # Reduce number of model if exceeding `m`
    if(d>1 && ncol(var_mat)>control$m){
      set.seed(graine[d]-1)
      var_mat <- var_mat[,sample.int(ncol(var_mat),control$m)]
    }

    # Modify dynamically arguments for `caret::train`
    if(!missing(caret_args_dyn)) args_caret <- caret_args_dyn(args_caret2,d)

    # Compute CV errors
    cv_errors <- rep(NA,ncol(var_mat))
    for(i in seq_len(ncol(var_mat))){
      # select the variable
      args_caret$x <- as.data.frame(x[,var_mat[,i]])

      # learner
      set.seed(graine[1]+i)
      learn <- do.call(train,args_caret)

      # save performance
      cv_errors[i] <- 0.1e1 - max(learn$results$Accuracy)
    }

    # Store results
    CVs[[d]] <- cv_errors
    VarMat[[d]] <- var_mat
    cv_alpha[d] <- quantile(cv_errors,control$alpha,na.rm=T)
    IDs[[d]] <- which(cv_errors <= cv_alpha[d])

    if(d == 1) id_screening <- IDs[[d]]

    if(control$verbose) print(paste0("Dimension explored: ",d," - CV errors at alpha: ",round(cv_alpha[d],4)))
    if(ncol(var_mat)==1) break
  }

  #---------------------
  ## Return
  #---------------------
  # Remove `x` and `y` from `args_caret` before returning it
  args_caret$y <- NULL
  args_caret$x <- NULL

  structure(
    list(
      x=x,
      y=y,
      control=control,
      CVs=CVs[1:d],
      VarMat=VarMat[1:d],
      cv_alpha=cv_alpha[1:d],
      IDs=IDs[1:d],
      args_caret=args_caret,
      caret_args_dyn=caret_args_dyn
    ),
    class="swag"
  )
}


# build all possible combinations
model_combination <- function(
  id_screening,
  var_mat
){
  # Generate all combinations of var_mat and id_screening
  A <- rbind(
    matrix(rep(var_mat,length(id_screening)),nr=nrow(var_mat)),
    rep(id_screening,each=ncol(var_mat))
  )

  # Remove duplicates:
  # remove same model
  A <- unique(apply(A,2,sort), MARGIN = 2)

  # remove same attributes
  id <- apply(A,2,anyDuplicated)>0
  if(sum(id)==0){
    return(A)
  }else{
    return(subset(A,select=!id))
  }
}
