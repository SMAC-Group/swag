data(BreastCancer, package = "mlbench")
y <- BreastCancer$Class
x <- BreastCancer[setdiff(names(BreastCancer),c("Id","Class"))]
# remove missing values
id <- which(apply(x,1,function(z) sum(is.na(z)))>0)
y <- y[-id]
x <- x[-id,]
# x <- matrix(as.numeric(as.matrix(x)),nc=9)

learner <- caret::train(as.data.frame(x$Cl.thickness),y,method="knn",preProcess=c("center","scale"),tuneLength=10,trControl=trainControl(method="cv"))

# test_logistic <- swag(y, X, q0 = .5, nc = 4, verbose = TRUE) # default for logistic regression

# To me the logistic is not interesting as there are a lot of packages who can do stepwise like logistics (e.g. Mumin, Leaps etc.)

# test_svmLinear <- swag(y, X, learner = "svmLinear", dmax = 3, q0 = .5, nc = 4, verbose = TRUE)
# test_svmRadial <- swag(y, X, learner = "svmRadial", dmax = 3, q0 = .5, nc = 4, verbose = TRUE)
# test_lasso <- swag(y, X, learner = "lasso", dmax = 3, q0 = .5, nc = 4, verbose = TRUE)
# test_rf <- swag(y, X, learner = "rf", dmax = 3, q0 = .5, nc = 4, verbose = TRUE)

require("swag")

require(caret)

# First step: control for swag

trial <- swagControl(pmax = 3,alpha = 0.3,m = 100,seed = 163L, verbose = T)

# error: Error in structure(pmax = pmax, m = m, alpha = alpha, seed = seed, class = "swagControl") : argument ".Data" is missing, with no default

trial_2 <- auto_swagControl(x = x,y = y,control = swagControl()) 

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


# Second step: choose a learner

# RF case

metric <- "Accuracy"

mtry <- sqrt(ncol(x)) #usual parameter

mtry <- 1

tunegrid <- expand.grid(.mtry=mtry)

try_obj <- swag(x = as.matrix(x), y = y,control = trial, auto_control = F,method = "rf", metric = metric, trControl=trctrl, tuneGrid=tunegrid )



# SVM linear case


try_obj <- swag(x = x, y = y,control = trial, auto_control = F,method = "svmLinear", metric = metric, trControl=trctrl, preProcess = c("center", "scale"),tuneLength = 10 )

#try_obj <- swag(x = as.matrix(x), y = y,control = trial,auto_control = F,method = "svmLinear", metric = metric, trControl=trctrl, preProcess = c("center", "scale"),tuneLength = 10)

#try_obj <- swag(x = as.matrix(x), y = y,auto_control = T,method = "svmLinear", metric = metric, trControl=trctrl, preProcess = c("center", "scale"),tuneLength = 10)


# Update sam:

fit_swag <- swag(x = x, y = y, control=swagControl(alpha=.2), method = "svmLinear", metric = metric, trControl=trctrl, preProcess = c("center", "scale"),tuneLength = 10)

# Error in if (control$verbose) print(paste0("Dimension explored: ", 1,  :argument is of length zero

