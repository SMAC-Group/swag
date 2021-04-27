# clean ws
rm(list=ls())
library(swag)
# run swag cancer no cancer
# source(file = "R/swag/swag.R")
# source(file = "R/swag/control.R")
# library(swag)
# load data
load("~/github_repo/liver_cancer_project/data/df_men_women.rda")
# load("df_men_women.rda")

# # load libraries
# library(dplyr)
# # library(swag)
# library(magrittr)

# encode to 0 and 1
unique(df_men_women$donor_sex)
df_men_women$y = ifelse(df_men_women$donor_sex =="male", 1, 0) %>% as.factor()
table(df_men_women$y)

# remove the 36 observations with missing variables
df_men_women_2 = na.omit(df_men_women)

# remove variables with no variance
# apparently, there are some gene with 0 variance, let's identify them
# mean_col = apply(df_men_women_2, 2, var)
# mean_col = colMeans(df_men_women_2)
# id_col_no_var = which(var_col == 0)
# test = df_cancer_no_cancer_2[,id_col_no_var]

# remove them
# df_men_women_3 = df_men_women_2[, -c(id_col_no_var)]
x_train = df_men_women_2 %>% select(-c(y, icgc_specimen_id, specimen_type, icgc_donor_id, donor_sex))

# check proportion of zero per column
prop_zero = function(vec){sum(vec == 0) / length(vec)}
prop_zero_vec = apply(x_train, 2, prop_zero)

# identify column with majority of 0
id_selected = which(prop_zero_vec < .6)

# remove these columns from xtrain
x_train = x_train[, id_selected]




any_obs_smaller_than_min = apply(x_train, 1, function (x) sum(x < .Machine$double.xmin))
any_var_smaller_than_min = apply(x_train, 2, function (x) sum(x < .Machine$double.xmin))

test = apply(x_train, 2, function (x) sum(x < .Machine$double.xmin))
test[5186]
sum(is.na(x_train[,5186]))
sum(test>24)
sum(any_var_smaller_than_min)
sum(any_obs_smaller_than_min)

# get quantile
# remove all column which have a mni values below min possible value
min_per_col =  apply(x_train, 2, min)
sum(min_per_col<.Machine$double.xmin)
5186 %in% which(min_per_col<.Machine$double.xmin)


# remove column 5186 (bug)

x_train = x_train[,-c(5186)]

head(colnames(x_train))
tail(colnames(x_train))

y_train = df_men_women_2$y

# check baseline accuracy
max(table(y_train)) / length(y_train)


# # subset x train fro debug
# set.seed(123)
# sample_var = sample(1:dim(x_train)[1], 30)
# x_train_red = x_train[, sample_var]



# define swagcon
swagcon <- swagControl(pmax = 15L,
                       alpha = 0.05,
                       m = 50L,
                       seed = 163L, #for replicability
                       verbose = T, #keeps track of completed dimensions,
                       verbose_dim_1=T
)

### SVM radial Learner ###
swag_men_women_svm_rdl <- swag(
  # arguments for swag
  x = x_train,
  y = y_train,
  control = swagcon,
  auto_control = FALSE,
  # arguments for caret
  trControl = caret::trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = T),
  metric = "Accuracy",
  method = "svmRadial",  # Use method = "svmRadial" to train this alternative learner
  preProcess = c("center", "scale")
)

# [1] "Dimension 1: completed variable: 5185"
# Error in votematrix[i, ret < 0] <- votematrix[i, ret < 0] + 1 :
#   NAs are not allowed in subscripted assignments
# In addition: There were 22 warnings (use warnings() to see them)
# Timing stopped at: 0.078 0.008 0.089



# save(swag_men_women_svm_rdl, file = "swag_men_women_svm_rdl.rda")

x_sub = as.data.frame(x_train[, colnames(x_train)[5186]])
problem_var = x_train[, 5186]
summary(problem_var)
boxplot(problem_var)
# just try simple fit
trControl = caret::trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = T)
svm_test <- train(x = x_sub, y = y_train,
                  method = "svmRadial", trControl = trControl,
                  preProcess = c("center","scale"), tuneLength = 10)
# Print the best tuning parameter sigma and C that maximizes model accuracy
svm_test$bestTune
svm_test$results

