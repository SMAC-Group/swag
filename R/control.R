# Copyright (C) 2020 Gaetan Bakalli, Samuel Orso
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
  verbose = FALSE
){
  if(!is.numeric(pmax) || pmax <= 0) stop("value of `pmax` > 0")
  if(!is.numeric(m) || m <= 0) stop("value of `m` > 0")
  if(!is.numeric(alpha) || abs(alpha) > 1) stop("value of 0<`alpha`<=1")
  if(!is.numeric(seed) || seed <= 0) stop("value of `seed` > 0")
  if(!is.logical(verbose)) stop("verbose must be a logical")
  structure(list(pmax=pmax,m=m,alpha=alpha,seed=seed,verbose=verbose),class="swagControl")
}

auto_swagControl <- function(
  x,
  y,
  control
){
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

  swagControl(pmax,m,control$alpha,control$seed,control$verbose)
}
