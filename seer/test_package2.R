data(BreastCancer, package = "mlbench")
y <- BreastCancer$Class
X <- as.matrix(BreastCancer[setdiff(names(BreastCancer),c("Id","Class"))])
fit <- seer(y, X, learner = "svmLinear", q0 = 0.3, nc = 4)

