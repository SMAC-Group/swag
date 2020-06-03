data(BreastCancer, package = "mlbench")
y <- BreastCancer$Class
x <- BreastCancer[setdiff(names(BreastCancer),c("Id","Class"))]
# remove missing values
id <- which(apply(x,1,function(z) sum(is.na(z)))>0)
y <- y[-id]
x <- x[-id,]

learner <- caret::train(x,y,method="knn",preProcess=c("center","scale"),tuneLength=10,trControl=trainControl(method="cv"))

# test_logistic <- swag(y, X, q0 = .5, nc = 4, verbose = TRUE) # default for logistic regression

# To me the logistic is not interesting as there are a lot of packages who can do stepwise like logistics (e.g. Mumin, Leaps etc.)

# test_svmLinear <- swag(y, X, learner = "svmLinear", dmax = 3, q0 = .5, nc = 4, verbose = TRUE)
# test_svmRadial <- swag(y, X, learner = "svmRadial", dmax = 3, q0 = .5, nc = 4, verbose = TRUE)
# test_lasso <- swag(y, X, learner = "lasso", dmax = 3, q0 = .5, nc = 4, verbose = TRUE)
# test_rf <- swag(y, X, learner = "rf", dmax = 3, q0 = .5, nc = 4, verbose = TRUE)
