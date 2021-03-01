# # swag fo regression predictive modelling
#
# # clean ws
# rm(list=ls())
#
# # library
# source("R/control.R")
# library(swag)
#
# # load data
# data("BostonHousing", package = "mlbench")
#
# # inspect str
# str(BostonHousing)
#
# # define x and y
# x_train = BostonHousing[, c(1:13)]
# y_train = BostonHousing[,14]
#
# # define swag controls
# swag_con = swagControl(pmax = 6L,
#             alpha = 0.5,
#             m = 10L,
#             seed = 163L, #for replicability
#             verbose = T #keeps track of completed dimensions
# )
#
# # run swag
# results_swag_continuous_y = swag(x = x_train,
#      y = y_train,
#      control =  swag_con,
#      trControl = caret::trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F),
#      metric = "RMSE",
#      method = "lm"  # Use method = "svmRadial" to train this alternative learner
#      )
#
#
#
