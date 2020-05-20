# test code
require(caret)
require(doParallel)
y = round(runif(30))
X = matrix(rnorm(30*50),nrow = 30, ncol = 50)
test_logistis = seer(y, X, learner = "logistic")



