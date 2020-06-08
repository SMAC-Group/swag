
###########################
# Old code
###########################
# @title swag validation for ML method.
#
# @description swag algo
# @param obj A \code{object} of of class \code{'swag'}.
# @param X A \code{matrix} or \code{data.frame} of attributes
# @param parallel_comput  An \code{boolean} to allow for parallel computing.
# @param seed  An \code{integer} that controls the reproducibility.
# @param nc An \code{double} that specify the number of core for parallel computation.
# @return A \code{swag} object with the structure:
# \describe{
# \item{}{}
# }
# @author Gaetan Bakalli and Samuel Orso
# @import caret
# @import doParallel
# @export
# @examples
# swag()
#
# swag_valid <- function(obj, y_valid, X_valid, seed = 163){
#
#   if(learner == "rf"){
#     leaner_screen = learner
#     family_screen = family = NULL
#     metric = "Accuracy"
#     family = NULL
#     preprocess = NULL
#     tuneLength = NULL
#   }else if(learner == "glmnet"){
#     leaner_screen =  "glm"
#     family_screen =  binomial()
#     learner = glmnet
#     family = "binomial"
#     metric = "Accuracy"
#     family = NULL
#     preprocess = NULL
#     tuneLength = NULL
#   }else if(learner == "svmLinear"){
#     leaner_screen = learner
#     family_screen = family = NULL
#     metric = "Accuracy"
#     family = NULL
#     preprocess = NULL
#     tuneLength = NULL
#   }else if(learner == "svmRadial"){
#     leaner_screen = learner
#     family_screen = family = NULL
#     metric = "Accuracy"
#     family = NULL
#     preprocess = NULL
#     tuneLength = NULL
#   }
#
#   # Counting error list
#   ce = list()
#   # y for training and valid sample sample
#   y_train <- as.factor(obj$y_train)
#   y_valid <- as.factor(y_valid)
#
#   # Options for learner
#   trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
#
#   pred_error_list = list()
#   for(d in seq_along(obj$model_dim_selected)){
#     # Matrix of selected variables at dimention d
#     var_mat_select <- obj$swag_model[[d]]
#     # Initialize vector of counting errors
#     if(is.null(dim(obj$swag_model[[d]])) ){
#       counting_error_svm = rep(NA,1)
#       test_pred = matrix(NA,length(y_test),1)
#       pb1 = 1
#       n_mod_dim = 1
#     }else{
#       counting_error_svm = rep(NA,dim(var_mat_select)[1])
#       test_pred = matrix(NA,length(y_test),dim(var_mat_select)[1])
#       pb1 = dim(var_mat_select)[1]
#       n_mod_dim = dim(var_mat_select)[1]
#     }
#     # Store the vector of prediction
#     nc = detectCores()
#     cl <- makeCluster(nc)
#     registerDoParallel(cl)
#
#     pb <- txtProgressBar(min = 0, max = pb1, style = 3)
#     for(j in 1:n_mod_dim){
#       # Index of selected covariate of model j
#
#       rc <- var_mat_select[j,]
#       # New X train matrix and create data frame for svm computation
#       X1 <- as.matrix(obj$x_train[,rc])
#       df = data.frame(y_train,X1)
#       # Svm object
#       learn = train(y ~., data = df, method = learner, metric = metric, family = family,
#                          trControl=trctrl, preProcess = preprocess, tuneLength = tuneLength,
#                          tuneGrid=tunegrid)
#
#       ## Counting errors and confusion matrix
#       # X test matrix and create data frame for svm computation
#       X = X_valid[,rc]
#       df1 <- data.frame(yte, X2)
#       # create the data matrix for prediction storage
#       df2 <- data.frame(X2)
#       # make precition out-of-sample
#       test_pred[,j] <- predict(svm_radial, newdata = df2)
#       # Store confusion matrix
#       obj2 = confusionMatrix(table(predict(svm_radial, newdata = df2), df1$yte))
#       # Compute the counting error
#       counting_error_svm[j] = sum(as.vector(obj2$table)[c(2,3)])
#       setTxtProgressBar(pb, j)
#     }
#     stopCluster(cl)
#     ## Sort the signs and extract the quantile when it changes
#     # List of counting error for various model dimention
#     ce[[d]] = counting_error_svm
#     # List of matrices of prediction
#     pred_error_list[[d]] =  test_pred
#     print(d)
#   }
#
#   count_error = unlist(ce)
#   # Majority rule svm
#   mat_test_pred = do.call(cbind,test_pred_list)
#   #mat_test_pred[mat_test_pred == 1] = 0
#   #mat_test_pred[mat_test_pred == 2] = 1
#   pred_mod_av = (round(apply(mat_test_pred,1,mean)))
#   # counting error model averaging
#   mode_av_ce = abs(sum(pred_mod_av-as.numeric(y_test)))
#   out = structure(list(ma_ce = mode_av_ce,
#                        mat_test_pred = mat_test_pred,
#                        ce_all = count_error))
#   invisible(out)
# }

# Update: next section will be removed to `predict()`
#---------------------
## Final model selection
#---------------------
#
#   ## Define the swag sets of models
#   # Dimension which minimize the median cv error at each dimesion
#   mod_size_min_med = which.min(sapply(CVs, median))
#   #quantile of the 1% most predictive models
#   treshold_swag_set = quantile(CVs[[mod_size_min_med]],probs=0.01)
#
#   # Vector of models dimension
#   dim_model = 1:pmax
#
#   # Find the index of model selected
#   index_model_select = vector("list",pmax)
#   for(d in seq_len(pmax)){
#     if(sum(CVs[[d]] <= treshold_swag_set) == 0){
#       index_model_select[[d]] = "empty"
#     }else{
#       index_model_select[[d]] = which(CVs[[d]] <= treshold_swag_set)
#     }
#   }
#
#   # vector of model dimension selected
#   model_dim_selected = which(index_model_select != "empty")
#
#   ###### swag subset of model ######
#
#   ## create output for swag subset
#   ## index of variable selected and respective cv error
#   swag_model = list()
#   swag_cv_error = list()
#
#   for(d in seq_along(model_dim_selected)){
#     index_mod = model_dim_selected[[d]]
#     swag_model[[d]] <- VarMat[[index_mod]][index_model_select[[index_mod]],]
#     swag_cv_error[[d]] <- CVs[[index_mod]][index_model_select[[index_mod]]]
#   }
#
#   #
#   table_variable = table(unlist(swag_model))
#   variable_index = as.numeric(names(table_variable))
#   names(table_variable) = colnames(X[,variable_index])
#
#   obj = list(pred_cv = CVs,
#              model_evaluated = VarMat,
#              model_selected = IDs,
#              model_swag_set = index_model_select,
#              table_variable = table_variable,
#              variable_index = variable_index,
#              swag_model = swag_model,
#              swag_cv_error = swag_cv_error,
#              learner = learner,
#              y_train = y,
#              x_train = X)
#
#   class(obj) = "swag"
#   invisible(obj)
#
