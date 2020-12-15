# Copyright (C) 2020 Gaetan Bakalli, Samuel Orso and Cesare Miglioli
#
# This file is part of SWAG-R Package

#' @title Method for selecting meta-parameters for \code{swag}
#' @description The Sparse Wrapper AlGorithm depends on some meta-parameters that are
#'  selected by this function given the learning ingredients described below.
#' @param x A \code{matrix} or \code{data.frame} of attributes
#' @param y A \code{vector} of binary response variable.
#' @param space_exp A \code{double} representing the percentage of the space of all possible learners of size two to explore.
#' @param pmax A \code{integer} representing the maximum number of attributes per learner.
#'  @param caret_args_dyn If not null, a function that can modify arguments for
#' \code{\link[caret]{train}} dynamically (see the details).
#' @return
#' \code{meta_select} returns a \code{list} with the following components:
#' \tabular{ll}{
#'    \code{control} \tab the \code{control} that contains the suggested \code{swag} meta-parameters (see \code{\link{swagControl}}) \cr
#'    \code{max_eta} \tab a \code{vector} containing the \code{swag} maximum expected time of arrival (eta) for each dimension up to \code{pmax} \cr
#' }
#' @details
#' @author Cesare Miglioli and Samuel Orso
#' @importFrom caret train
#' @importFrom microbenchmark microbenchmark
#' @export
meta_select <- function(x,
                        y,
                        space_exp,
                        pmax,
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

  n <- dim(x)[1]

  p <- dim(x)[2]

  ### 1st step --> find m ###

  # Passages: evaluate first the binomial coefficients (p 2) then multiply by the desired percentage to get number of learners

  # at size 2 that you need to train. This is the meta_parameter m in SWAG.

  m <- floor(space_exp * choose(p,2))

  ### 2nd step --> find alpha ###

  # Passages: first find p_star (see paper SWAG) by imposing that at least you train all possible learners of size 2. Finally

  # retrieve alpha by understanding the percentage to select that implies a threshold for the features at the first step.

  p_star <- floor((1 + sqrt(1 + 8*m))/2)

  alpha <- p_star/p

  ### 3rd step --> find the maximal ETA for each dimension up to p_max ###

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

  eta <- sapply(1:pmax, FUN = function(i) {
       args_caret$x <- as.data.frame(x[,1L:i])
       mean(microbenchmark(do.call(train,args_caret),times = 5L)$time)})


  # Multiplication of eta and m learners at each dimension

  max_eta <- cumsum(c(p*eta[1],m*eta[2:pmax]))

  names(max_eta) <- sapply(1:p_max, function(x) paste("Max_ETA_dim_", x, sep = ""))

  #---------------------
  ## Return
  #---------------------

  control <- swagControl(pmax = pmax, m = m, alpha = alpha)
  list(control = control,
       max_eta = max_eta)
}
