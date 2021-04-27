# Copyright (C) 2020 Gaetan Bakalli, Samuel Orso and Cesare Miglioli
#
# This file is part of SWAG-R Package

#' @title Control for swag function
#'
#' @description The Spare Wrapper AlGorithm depends on some meta-parameters that are
#' described below.
#' @param pmax A \code{integer} representing the maximum number of attributes per learner.
#' @param m A \code{integer} representing the maximum number of learners per dimension explored.
#' @param alpha A \code{double} representing the proportion of screening.
#' @param seed  An \code{integer} that controls the reproducibility.
#' @param verbose A \code{boolean} for printing current progress of the algorithm.
#' @seealso \code{\link[swag]{swag}}
#' @export swagControl
swagControl <- function(
  pmax = 3,
  m = 100,
  alpha = 0.05,
  seed = 163L,
  verbose = FALSE,
  verbose_dim_1 = FALSE
){
  if(!is.numeric(pmax) || pmax <= 0) stop("value of `pmax` > 0")
  if(!is.numeric(m) || m <= 0) stop("value of `m` > 0")
  if(!is.numeric(alpha) || alpha > 1 || alpha <=0 ) stop("value of 0<`alpha`<=1")
  if(!is.numeric(seed) || seed <= 0) stop("value of `seed` > 0")
  if(!is.logical(verbose)) stop("verbose must be a logical")
  structure(list(pmax=pmax,m=m,alpha=alpha,seed=seed,verbose=verbose, verbose_dim_1 = verbose_dim_1),class="swagControl")
}

auto_swagControl <- function(
  x,
  y,
  control,
  procedure = "class"
){
  if(!procedure %in% c("class", "reg")){stop("Please provide a procedure for your problem. Supported procedure are class and reg")}

  if(procedure == "class"){
    n <- dim(x)

    # Data-dependent `pmax`: corresponds to approximatively
    # an EPV of 5
    event <- table(y)
    pmax <- ceiling(min(event[1],n[1]-event[2])/5)

    # At minima explore 3 attributes
    if(pmax == 1) pmax <- 3

    # But do not exceed p
    if(pmax > n[2]) pmax <- n[2]

    # Define `m` such that all learners of two attributes
    # are explored.
    m <- choose(ceiling(control$alpha * n[2]), 2)

    return(swagControl(pmax,m,control$alpha,control$seed,control$verbose))

  }
  else if(procedure == "reg"){
    return(swagControl(control$pmax, control$m,control$alpha,control$seed,control$verbose))
  }


}
