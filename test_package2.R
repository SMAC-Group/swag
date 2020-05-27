data(BreastCancer, package = "mlbench")
y <- BreastCancer$Class
X <- as.matrix(BreastCancer[setdiff(names(BreastCancer),c("Id","Class"))])
# remove missing values
id <- which(apply(X,1,function(x) sum(is.na(x)))>0)
y <- y[-id]
X <- X[-id,]


test_logistic <- seer(y, X, q0 = .5, nc = 4, verbose = TRUE) # default for logistic regression
test_svmLinear <- seer(y, X, learner = "svmLinear", dmax = 3, q0 = .5, nc = 4, verbose = TRUE)
test_svmRadial <- seer(y, X, learner = "svmRadial", dmax = 3, q0 = .5, nc = 4, verbose = TRUE)
test_lasso <- seer(y, X, learner = "lasso", dmax = 3, q0 = .5, nc = 4, verbose = TRUE)
test_rf <- seer(y, X, learner = "rf", dmax = 3, q0 = .5, nc = 4, verbose = TRUE)

