# Copyright (C) 2020 Gaetan Bakalli, Samuel Orso, Cesare Miglioli and Lionel Voirol

#' @title Summary method for SWAG
#' @description Method `summary` that returns the number and proportion of appearance of each variables on a subset of selected model.
#' The selection procedure of models proceed in two steps. First we select an explored dimension in which the `mean`, `min` or `median` is the lowest.
#'  We then compute the selected percentile of the CV error on this dimension.
#'  We then select all models in all explored dimensions that have a lower CV error than the CV value set by this two-steps procedure.
#' @param object A \code{object}.
#' @param min_dim_method A \code{string} that specify the method to identify the dimension on which to compute the quantile to set the minimal CV to select model.
#' @param min_dim_min_cv_error_quantile The quantile of CV error in the selected dimension to specify the minimum CV value for selected models.
#' @param ... additional arguments affecting the summary produced.
#' @author Gaetan Bakalli, Samuel Orso, Cesare Miglioli and Lionel Voirol
#' @importFrom stats median
#' @export
#' @method  summary swag
#' @export
"summary.swag" <- function(object,
                           min_dim_method = "median",
                           min_dim_min_cv_error_quantile = 0.01,
                           ...){



  # check for object class
  if(class(object) != "swag"){stop("Please provide a swag object")}

  # define CV and varmat
  CVs <- object$CVs
  VarMat <- object$VarMat

  # compute maximum dimension explored
  dmax <- length(object$CVs)
  dim_model <- seq_len(dmax)

  # find_dimension with lowest (min, mean, median)
  if(!min_dim_method  %in% c("mean", "median", "min")){
    stop("Please provided a supported method for selecting the minimum dimension for selecting models. \n Supported functions are min, median, mean")
  }

  if(min_dim_method == "min"){
    mod_size_min <- which.min(unlist(lapply(CVs[1:dmax], min)))
  }else if(min_dim_method == "median"){
    mod_size_min <- which.min(unlist(lapply(CVs[1:dmax], median)))
  }else if(min_dim_method == "mean"){
    mod_size_min <- which.min(unlist(lapply(CVs[1:dmax], mean)))
  }

  # get the quantile in  dimensions with the lowest (min, median, mean)
  quantile_value <- quantile(CVs[[mod_size_min]], min_dim_min_cv_error_quantile, na.rm=TRUE)

  # save all models in all dimensions for which the cv error is below the selected quantile
  index_model_select <- vector("list",dmax)
  for(i in dim_model){
    # save in these dimensions
    index_model_select[[i]]  <- which(((CVs[[dim_model[i]]] <= quantile_value)))
  }

  n_models_selected = length(unlist(index_model_select))

  # extract variables from selected models
  var_mat_select_list <- vector("list",dmax)
  for(d in dim_model){
    var_mat_select_list[[d]] <- VarMat[[dim_model[d]]][, index_model_select[[d]]]
  }

  # tabulate variables per appearance in selected models
  table_variable <- table(unlist(var_mat_select_list))
  variable_index <- as.numeric(rownames(table_variable))

  # get name of variables from variable index
  variable_name <- colnames(object$x[,variable_index])

  # compute proportion of the variables on selected models
  table_prop <- table_variable / n_models_selected

  # set name to table_prop
  names(table_prop) <- variable_name

  # order table prop
  table_prop <- sort(table_prop, decreasing = T)

  # return out
  out <- structure(list(model_select = var_mat_select_list,
                       n_models_selected = n_models_selected,
                       variable_table = table_variable,
                       variable_table_prop = table_prop,
                       variable_index = variable_index,
                       variable_name = variable_name,
                       x = object$x,
                       y = object$y
                       ),
                  class="summary.swag"
  )
  out
}
