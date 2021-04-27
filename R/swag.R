# Copyright (C) 2020 Gaetan Bakalli, Samuel Orso and Cesare Miglioli
#
# This file is part of SWAG-R Package

# build all possible combinations
model_combination <- function(
  id_screening,
  var_mat
){
  # Generate all combinations of var_mat and id_screening
  A <- rbind(
    matrix(rep(var_mat,length(id_screening)),nrow=nrow(var_mat)),
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




#' @title Spare Wrapper AlGorithm (swag)
#'
#' @description \code{swag} is used to trains a meta-learning procedure that combines
#' screening and wrapper methods to find a set of extremely low-dimensional attribute
#' combinations. \code{swag} works on top of the \pkg{caret} package and proceeds in a
#' forward-step manner.
#' @param x A \code{matrix} or \code{data.frame} of attributes
#' @param y A \code{vector} of binary response variable.
#' @param control see \code{\link{swagControl}}
#' @param auto_control A \code{boolean}, whether some control parameters
#' are adjusted depending on \code{x} and \code{y} (see \code{\link{swagControl}}).
#' @param caret_args_dyn If not null, a function that can modify arguments for
#' \code{\link[caret]{train}} dynamically (see the details).
#' @param metric A \code{string} that indicates the measure of predictive performance to be used. Supported measure are RMSE and Accuracy.
#' @param ... Arguments to be passed to \code{\link[caret]{train}} functions (see the details).
#' @return
#' \code{swag} returns an object of class "\code{swag}". It is a \code{list}
#' with the following components:
#' \tabular{ll}{
#'    \code{x} \tab same as \code{x} input \cr
#'    \code{y} \tab same as \code{y} input \cr
#'    \code{control} \tab the \code{control} used (see \code{\link{swagControl}}) \cr
#'    \code{CVs} \tab a \code{list} containing cross-validation errors from all trained models \cr
#'    \code{VarMat} \tab a \code{list} containing information about which models are trained \cr
#'    \code{cv_alpha} \tab a \code{vector} of size \code{pmax} containing the
#'    cross-validation error at \code{alpha} (see \code{\link{swagControl}}) \cr
#'    \code{IDs} \tab a \code{list} containing information about trained model
#'    that performs better than corresponding \code{cv_alpha} error \cr
#'    \code{args_caret} \tab arguments used for \code{\link[caret]{train}} \cr
#'    \code{args_caret_dyn} \tab same as \code{args_caret_dyn} input \cr
#' }
#' @details
#' Currently we expect the user to replace \code{...} with the arguments one would
#' use for \code{\link[caret]{train}}. This requires to know how to use \code{\link[caret]{train}}
#' function. If \code{...} is left unspecified, default values of \code{\link[caret]{train}}
#' are used. But this might lead to unexpected results.
#'
#' The function \code{caret_args_dyn} is expected to take as a first
#' argument a \code{list} with all arguments for \code{\link[caret]{train}}
#' and as a second argument the number of attributes (see examples in the vignette).
#'
#' More specifically, \code{swag} builds and tests learners starting
#' from very few attributes until it includes a maximal number of attributes by
#' increasing the number of attributes at each step. Hence, for each fixed number
#' of attributes, the algorithm tests various (randomly selected) learners and
#' picks those with the best performance in terms of training error. Throughout,
#' the algorithm uses the information coming from the best learners at the previous
#' step to build and test learners in the following step. In the end, it outputs
#' a set of strong low-dimensional learners. See \insertCite{molinari2020swag;textual}{swag} for
#' more details.
#' @author Gaetan Bakalli, Samuel Orso and Cesare Miglioli
#' @importFrom caret train
#' @importFrom caret trainControl
#' @importFrom stats quantile
#' @importFrom Rdpack reprompt
#' @import lattice
#' @import caret
#' @references
#' \insertAllCited{}
#' @export swag
swag <- function(x,
                 y,
                 control = swagControl(),
                 auto_control = T,
                 # arguments for `caret::train()`
                 caret_args_dyn = NULL,
                 metric = NULL,
                 ...){


  # Existence of arguments for `caret::train()`
  # with default values
  args_caret <- list(...)

  # append metric to args caret
  args_caret[["metric"]] = metric


  # default learner rf
  if(is.null(args_caret$method)) args_caret$method = "rf"

  # default arg for args_caret
  if(is.null(args_caret$preProcess)) args_caret$preProcess = NULL
  if(is.null(args_caret$weights)) args_caret$weights = NULL

  # stop if no metric provided
  if(is.null(args_caret$metric)) stop("Please provide a loss metric for your problem. Supported loss functions are RMSE or Accuracy")

  # define if max loss function based on loss
  if(is.null(args_caret$maximize)) args_caret$maximize = ifelse(args_caret$metric %in% c("RMSE", "logLoss", "MAE"), FALSE, TRUE)
  if(is.null(args_caret$trControl)) args_caret$trControl = trainControl()
  if(is.null(args_caret$tuneGrid)) args_caret$tuneGrid = NULL
  if(is.null(args_caret$tuneLength)) args_caret$tuneLength = ifelse(args_caret$trControl$method == "none", 1, 3)

  # define procedure
  if(args_caret$metric == "RMSE"){
    procedure = "reg"
  }else if(args_caret$metric == "Accuracy"){
    procedure = "class"
  }

  #---------------------
  ## Verify the arguments
  #---------------------
  if(missing(x)) stop("Please provide a `x`")
  if(missing(y)) stop("Please provide a response vector `y`")

  # Check missing observations (not supported currently)
  if(sum(is.na(y)) > 0 || sum(is.na(x)) > 0){    stop("Please provide data without missing values")}

  # transform y to factor if class task
  if(procedure == "class"){
    if(!is.factor(y)) y <- as.factor(y)

  }

  # verify y is binary
  if(procedure == "class"){
    if(nlevels(y)>2) stop("Please provide a binary response `y`")
  }


  ## object dimension
  # Number of attributes
  dim_x <- dim(x)
  if(is.null(dim_x) || length(dim_x) != 2) stop("`x` must be a two-dimensional object")
  if(dim_x[1] != length(y)) stop("dimensions of `x` and `y` does not match")
  p <- dim_x[2]
  n <- dim_x[1]

  if(class(control) != "swagControl"){
    stop("`control` must be of class `swagControl`")
  }

  # define swag control if auto
  if(isTRUE(auto_control)) control <- auto_swagControl(x = x,y = y, control = control, procedure = procedure)



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
  # explore d dimension
  for(d in seq_len(control$pmax)){

    # Build all combinations
    if(d == 1){
      var_mat <- seq_len(p)
      dim(var_mat) <- c(1,p)
    }else{
      var_mat <- model_combination(id_screening,subset(VarMat[[d-1L]],select=IDs[[d-1]]))
    }

    # Reduce number of model if exceeding `m` and if dimension is greater than 1
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

      # save performance of best model


      if(procedure == "reg"){
        cv_errors[i] <- min(learn$results$RMSE)

      }else if(procedure == "class"){
        cv_errors[i] <- 0.1e1 - max(learn$results$Accuracy)

      }

      # print variable for first dimension
      if(d == 1 && control$verbose_dim_1){
        print(paste("Dimension 1: completed variable:", i))
      }



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
