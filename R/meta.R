# Copyright (C) 2020 Gaetan Bakalli, Samuel Orso and Cesare Miglioli
#
# This file is part of SWAG-R Package

#' @title Method for selecting meta-parameter for \code{swag}
#' @description
#' @param x A \code{matrix} or \code{data.frame} of attributes
#' @param y A \code{vector} of binary response variable.
#' @param space_exp
#' @param ... Arguments to be passed to \code{\link[caret]{train}} functions (see the details).
#' @return
#' @details
#' @author Cesare Miglioli
#' @export
meta_select <- function(x,
                        y,
                        space_exp,
                        pmax,
                        # arguments for `caret::train()`
                        caret_args_dyn = NULL,
                        ...){
  # check arguments validity

  # This function takes as inputs the feature matrix X, the target y, the desired percentage of model space of size 2 to explore,

  # the maximum size of learners features and the type of learner chosen.


  # The output is a matrix with suggested alpha, m (both meta-parameters in swag) and maximal ETA for each dimension up to p_max.


  n <- dim(X)[1]

  p <- dim(X)[2]

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

  # trctrl <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 1) #10 fold CV repeated 1 times

  # FIXME:
  # * remove caret::
  # * use pkg microbenchmark
  # * use `learn <- do.call(train,args_caret)` : args_caret is list of arguments
  #   and args_caret$x <- as.data.frame(x[,var_mat[,i]])
  # for example:
  # sapply(1:pmax, FUN = function(i) {
  #   args_caret$x <- as.data.frame(x[,var_mat[,1L:i]])
  #   do.call(train,args_caret)})


  metric <- "Accuracy"

  if (method == "glmnet") {

    eta_1 <- system.time(caret::train(y ~ ., data = data.frame(y,X[,1]), method = "glm",family = binomial()))[3]

    # From 2 up to p_max

    eta_2 <- sapply(2:p_max, function(x) system.time(caret::train(y ~., data = data.frame(y,X[,1:x]), method = "glmnet", metric = metric,tuneGrid=tunegrid, trControl=trctrl,family="binomial"))[3])

    eta <- c(eta_1,eta_2)

  } else if (method == "rf") {

    eta <- sapply(1:p_max, function(x) system.time(caret::train(y ~ ., data = data.frame(y,X[,1:x]), method = "rf", metric = metric,tuneGrid=expand.grid(.mtry= 1:sqrt(x)), trControl=trctrl))[3])

  } else if (method == "svmLinear") {

    eta <- sapply(1:p_max, function(x) system.time(caret::train(y ~ ., data = data.frame(y,X[,1:x]), method = "svmLinear", trControl=trctrl, preProcess = c("center", "scale"),tuneLength = tunelength))[3])

  } else if (method == "svmRadial") {

    eta <- sapply(1:p_max, function(x) system.time(caret::train(y ~ ., data = data.frame(y,X[,1:x]), method = "svmRadial", trControl=trctrl, preProcess = c("center", "scale"),tuneLength = tunelength))[3])

  } else {

    print("Choose a valid method")
  }

  # Multiplication of eta and m learners at each dimension

  max_eta <- cumsum(c(p*eta[1],m*eta[2:p_max]))

  names(max_eta) <- sapply(1:p_max, function(x) paste("Max_ETA_dim_", x, sep = ""))

  # out = structure(list(alpha = alpha,
  #                      m = m,
  #                      max_eta = max_eta ))
  # invisible(out)

  control <- swagControl(pmax = pmax, m = m, alpha = alpha)
  list(control = control,
       max_eta = max_eta)
}
