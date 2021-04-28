# Copyright (C) 2020 Gaetan Bakalli, Samuel Orso, Cesare Miglioli and Lionel Voirol

#' @title Returned estimated linear regression coefficients for each selected model in a summary.swag object
#' @description The function return a list that contains beta_models_df and the swag_summary object.
#' The beta_models_df is a dataframe where the columns are all of the selected variables from the summary.swag objects,
#' and where each row are the estimated coefficients from a selected models using the classical lm procedure.
#' @param swag_summary A \code{swag_summary} object.
#' @author Gaetan Bakalli, Samuel Orso, Cesare Miglioli and Lionel Voirol
#' @importFrom stats lm
#' @export
return_lm_beta_selected_models <- function(swag_summary) {

  # stop if not object summary.swag
  if (class(swag_summary) != "summary.swag") {
    stop("Please provide a summary.swag object")
  }

  # define max dim explored
  max_dim_explored <- length(swag_summary$model_select)

  # create matrix to store betas
  beta_models_df <- as.data.frame(matrix(nrow = swag_summary$n_models_selected, ncol = length(swag_summary$variable_name) + 1))
  colnames(beta_models_df) <- c("(Intercept)", swag_summary$variable_name)

  # fill up matrix
  model_index <- 1
  # iterate over all dimensions
  for (dim_i in seq(max_dim_explored)) {
    # if selected model in this dimension
    if (length(swag_summary$model_select[[dim_i]]) != 0) {

      # define nbr of model differently if only one or multiple selected models for that dimension
      if (is.vector(swag_summary$model_select[[dim_i]])) {
        nbr_model <- 1
      } else if (is.matrix(swag_summary$model_select[[dim_i]])) {
        nbr_model <- dim(swag_summary$model_select[[dim_i]])[2]
      }
      # iterate over selected model in this dimension
      for (model_i_dim_i in seq(nbr_model)) {

        # define selected variables differently if only one or multiple selected models for that dimension
        if (nbr_model == 1) {
          selected_variables <- swag_summary$model_select[[dim_i]]
        } else {
          selected_variables <- swag_summary$model_select[[dim_i]][, model_i_dim_i]
        }
        # get selected variables in the model
        x_mat <- as.matrix(swag_summary$x[, selected_variables])
        df_model_i <- cbind(swag_summary$y, x_mat)

        # set colnames
        colnames(df_model_i) <- c("y", colnames(df_model_i)[-1])
        df_model_i <- as.data.frame(df_model_i)
        fit <- lm(y ~ ., data = df_model_i)
        coeff_lm <- fit$coefficients

        # save fit coefficients
        beta_models_df[model_index, names(fit$coefficients)] <- coeff_lm

        # update model index
        model_index <- model_index + 1
      }
    }
  }

  # reorder based on table of model
  beta_models_df <- beta_models_df[, names(swag_summary$variable_table_prop)]

  out <- structure(list(
    beta_models_df = beta_models_df,
    swag_summary = swag_summary
  ))
  return(out)
}



##############

#' @title Returned estimated logistic regression coefficients for each selected model in a summary.swag object
#' @description The function return a list that contains beta_models_df and the swag_summary object.
#' The beta_models_df is a dataframe where the columns are all of the selected variables from the summary.swag objects,
#' and where each row are the estimated coefficients from a selected models using the classical lm procedure.
#' @param swag_summary A \code{swag_summary} object.
#' @author Gaetan Bakalli, Samuel Orso, Cesare Miglioli and Lionel Voirol
#' @importFrom stats glm
#' @export
return_glm_beta_selected_models <- function(swag_summary) {

  # stop if not object summary.swag
  if (class(swag_summary) != "summary.swag") {
    stop("Please provide a summary.swag object")
  }

  # define max dim explored
  max_dim_explored <- length(swag_summary$model_select)

  # create matrix to store betas
  beta_models_df <- as.data.frame(matrix(nrow = swag_summary$n_models_selected, ncol = length(swag_summary$variable_name) + 1))
  colnames(beta_models_df) <- c("(Intercept)", swag_summary$variable_name)

  # fill up matrix
  model_index <- 1
  # iterate over all dimensions
  for (dim_i in seq(max_dim_explored)) {
    # if selected model in this dimension
    if (length(swag_summary$model_select[[dim_i]]) != 0) {

      # define nbr of model differently if only one or multiple selected models for that dimension
      if (is.vector(swag_summary$model_select[[dim_i]])) {
        nbr_model <- 1
      } else if (is.matrix(swag_summary$model_select[[dim_i]])) {
        nbr_model <- dim(swag_summary$model_select[[dim_i]])[2]
      }
      # iterate over selected model in this dimension
      for (model_i_dim_i in seq(nbr_model)) {

        # define selected variables differently if only one or multiple selected models for that dimension
        if (nbr_model == 1) {
          selected_variables <- swag_summary$model_select[[dim_i]]
        } else {
          selected_variables <- swag_summary$model_select[[dim_i]][, model_i_dim_i]
        }
        # get selected variables in the model
        x_mat <- as.matrix(swag_summary$x[, selected_variables])
        df_model_i <- as.data.frame(x_mat)
        df_model_i$y = swag_summary$y

        # fit
        fit <- glm(y ~ ., data = df_model_i, family = binomial)
        coeff_lm <- fit$coefficients

        # save fit coefficients
        beta_models_df[model_index, names(fit$coefficients)] <- coeff_lm

        # update model index
        model_index <- model_index + 1
      }
    }
  }

  # reorder based on table of model
  beta_models_df <- beta_models_df[, names(swag_summary$variable_table_prop)]

  out <- structure(list(
    beta_models_df = beta_models_df,
    swag_summary = swag_summary
  ))
  return(out)
}
