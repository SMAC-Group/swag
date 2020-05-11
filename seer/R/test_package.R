# test code
require(caret)
require(doParallel)
y = round(runif(30))
X = matrix(rnorm(30*50),nrow = 30, ncol = 50)
learner = "svmLinear"
parallel_comput = T
seed = 666

tot_time = 100000000
nc = NULL
q0 = NULL
dmax = NULL
m = NULL





