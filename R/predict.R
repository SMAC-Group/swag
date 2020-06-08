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
#' @author Gaetan Bakalli, Samuel Orso and Cesare Miglioli
#' @import caret
#' @method predict.swag
#' @export predict.swag
predict.swag <- function(object,
                         newdata = NULL,
                         type = c("best","cv_performance","attribute"),
                         cv_performance = NULL,
                         attribute = NULL,
                         ...){
  type <- match.arg(type)

  # Determine which model to predict
  if(type == "best"){
    object$CVs
  }

  if(type == "cv_performance"){
    if(missing(cv_performance)) stop("Please specifiy a CV errors level.")
    if(cv_performance > 1 || cv_performance <= 0) stop("Please choose a CV error level
                                                       between 0 and 1.")
  }

  if(type == "attribute"){
    if(missing(attribute)) stop("Please specifiy an attribute.")
    if(length(object$CVs)<attribute) stop("Please choose an appropriate attribute.")
  }


}
