
load("/Users/gaetan/GitHub/meta-learning/Datasets/Meter A/data_split.rda")
require(doParallel)
require(caret)

y_train = as.factor(df_metera$y_train)
y_test = as.factor(df_metera$y_test)

x_train = df_metera$x_train
x_test = df_metera$x_test

x_train <- as.matrix(model.matrix( ~.^2, data=df_metera$x_train))
x_train <- x_train[,-1]
x_test = as.matrix(model.matrix( ~.^2, data=df_metera$x_test))
x_test <- x_test[,-1]

y = y_train
X = x_train
learner = "svmLinear"
dmax = 6
m = 40000
parallel_comput = T
nc = NULL
seed = 163

metera_in_logistic = seer(y = y_train, X = x_train,learner = "svmLinear",dmax = 6,m = 40000,q0=0.05)

save(metera_in_logistic , file = "metera_in_logistic.rda")

CVs = metera_in_logistic$pred_cv


m_vector <- sapply(metera_in_logistic$pred_cv[c(1:dmax)], function(x) summary(x)[4])
l_vector <- sapply(metera_in_logistic$pred_cv[c(1:dmax)], function(x) summary(x)[1])
u_vector <- sapply(metera_in_logistic$pred_cv[c(1:dmax)], function(x) summary(x)[6])
require(plotrix)


## 1152 model below 1%

plotCI(1:dmax, m_vector, ui=u_vector, li=l_vector, scol = "grey", col="red"
       , pch = 16, main = "Set of Highly Predictive Models - III Cartesian Quadrant",ylab = "Range CV Error",xlab = "Model Size")
mod_size_min = which.min(unlist(lapply(CVs[1:dmax], min)))
abline(v = mod_size_min, col="blue",lwd=2)
ab = quantile(CVs[[mod_size_min]],seq(0, 1, 0.01))[2]
abline(h = ab, col="blue",lwd=2)

