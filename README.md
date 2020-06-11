
`swag` package
==============

**swag** is a package that trains a meta-learning procedure which combines screening and wrapper methods to find a set of extremely low-dimensional attribute combinations.

Installing the package with GitHub
----------------------------------

First install the **devtools** package. Then **swag** with the following code:

``` r
devtools::install_github("SMAC-Group/SWAG-R-Package")

library(swag) #load the new package
```

Quick start
-----------

We propose to use the **breastcancer** dataset readily available from the package **mlbench** to give an overview of **swag**.

``` r
# After having installed the mlbench package

data(BreastCancer, package = "mlbench")

# Pre-processing of the data
y <- BreastCancer$Class # response variable
x <- as.matrix(BreastCancer[setdiff(names(BreastCancer),c("Id","Class"))]) # features

# remove missing values and change to 'numeric'
id <- which(apply(x,1,function(x) sum(is.na(x)))>0)
y <- y[-id]
x <- x[-id,]
x <- apply(x,2,as.numeric)

# Training and test set
set.seed(180) # for replication
ind <- sample(1:dim(x)[1],dim(x)[1]*0.2)  
y_test <- y[ind]
y_train <- y[-ind]
x_test <- x[ind,]
x_train <-x[-ind,]
```

Now we are ready to train with **swag**! The first step is to define the meta-parameters of the **swag** procedure: *p*<sub>*m**a**x*</sub> the maximum dimension of attributes, *α* a performance quantile which represents the percentage of learners which are selected at each dimension and *m*, the maximum numbers of learners trained at each dimension. We can set all these meta-parameters, together with a seed for replicability purposes and `verbose = TRUE` to get a message as each dimension is completed, thanks to the *swagcontrol()* function which behaves similarly to the `trControl =` argument of **caret**.

``` r
# Meta-parameters chosen for the breast cancer dataset
swagcon <- swagControl(pmax = 4L, 
                       alpha = 0.5, 
                       m = 20L,
                       seed = 163L, #for replicability
                       verbose = T #keeps track of completed dimensions
                       )

# Given the low dimensional dataset, we can afford a wider search 
# by fixing alpha = 0.5 as a smaller alpha may also stop the 
# training procedure earlier than expected.
```

Having set-up the meta-parameters as explained above, we are now ready to train the **swag**. We start with the linear Support Vector Machine learner:

``` r
### SVM Linear Learner ###
train_swag_svml <- swag(
  # arguments for swag
  x = x_train, 
  y = y_train, 
  control = swagcon,
  auto_control = FALSE,
  # arguments for caret
  trControl = caret::trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F),
  metric = "Accuracy",
  method = "svmLinear",  # Use method = "svmRadial" to train this alternative learner
  preProcess = c("center", "scale")
)
```

    ## [1] "Dimension explored: 1 - CV errors at alpha: 0.115"
    ## [1] "Dimension explored: 2 - CV errors at alpha: 0.0549"
    ## [1] "Dimension explored: 3 - CV errors at alpha: 0.0403"
    ## [1] "Dimension explored: 4 - CV errors at alpha: 0.0394"

The only difference with respect to the classic **caret** train function, is the specification of the **swag** arguments which have been explained previously. In the above chunk for the *svmLinear* learner, we define the estimator of the out-of-sample accuracy as 10-fold cross-validation repeated 1 time. For this specific case, we have chosen to center and rescale the data, as usually done for SVMs, and, the parameter that controls the margin in SVMs is automatically fixed at unitary value (i.e. *c* = 1).

Let's have a look at the typical output of a **swag** training object for the *svmLinear* learner:

``` r
train_swag_svml$CVs  
```

    ## [[1]]
    ## [1] 0.14094276 0.06959836 0.07499399 0.15157407 0.10811688 0.08592593 0.11502886
    ## [8] 0.12070707 0.22122896
    ## 
    ## [[2]]
    ##  [1] 0.05107744 0.06225950 0.03852213 0.05492304 0.06030544 0.04377104
    ##  [7] 0.05108225 0.06212121 0.07485570 0.05491582
    ## 
    ## [[3]]
    ## [1] 0.04010101 0.04761063 0.03848846 0.04030784 0.04575758 0.04016835 0.03841991
    ## [8] 0.04387205 0.05105099
    ## 
    ## [[4]]
    ## [1] 0.03464646 0.04572751 0.04030664 0.03852213

``` r
# A list which contains the cv training errors of each learner explored in a given dimension
```

``` r
train_swag_svml$VarMat 
```

    ## [[1]]
    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
    ## [1,]    1    2    3    4    5    6    7    8    9
    ## 
    ## [[2]]
    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    ## [1,]    2    2    2    2    3    3    3    5    5     6
    ## [2,]    3    5    6    7    5    6    7    6    7     7
    ## 
    ## [[3]]
    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
    ## [1,]    2    2    2    3    2    2    3    3    5
    ## [2,]    3    3    6    6    3    5    5    5    6
    ## [3,]    6    7    7    7    5    6    6    7    7
    ## 
    ## [[4]]
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    2    2    2    3
    ## [2,]    3    3    5    5
    ## [3,]    6    5    6    6
    ## [4,]    7    6    7    7

``` r
# A list which contrains a matrix, for each dimension, with the attributes tested at that step 
```

``` r
train_swag_svml$cv_alpha 
```

    ## [1] 0.11502886 0.05491943 0.04030784 0.03941438

``` r
# The cut-off cv training error, at each dimension, determined by the choice of alpha
```

The other two learners that we have implemented on **swag** are: lasso (**glmnet** package required) and random forest (**party** package required). The training phase for these learners, differs a little with respect to the SVM one. We can look at the random forest for a practical example:

``` r
### Random Forest Learner ###
train_swag_rf <- swag(
  # arguments for swag
  x = x, 
  y = y, 
  control = swagcon,
  auto_control = FALSE,
  # arguments for caret
  trControl = caret::trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F),
  metric = "Accuracy",
  method = "rf",
  # dynamically modify arguments for caret
  caret_args_dyn = function(list_arg,iter){
    list_arg$tuneGrid = expand.grid(.mtry=sqrt(iter))
    list_arg
  }
)
```

    ## [1] "Dimension explored: 1 - CV errors at alpha: 0.0996"
    ## [1] "Dimension explored: 2 - CV errors at alpha: 0.0534"
    ## [1] "Dimension explored: 3 - CV errors at alpha: 0.0461"
    ## [1] "Dimension explored: 4 - CV errors at alpha: 0.0425"

The newly introduced argument `caret_args_dyn` enables the user to modify the hyper-parameters related to a given learner in a dynamic way since they can change as the dimension grows up to the desired *p*<sub>*m**a**x*</sub>. This allows to adapt the *mtry* hyper-parameter as the dimension grows. In the example above, we have fixed *mtry* to the square root of the number of attributes at each step as it is usually done in practice.

You can tailor the learning arguments of *swag()* as you like, introducing for example grids for the hyper-parameters specific of a given learner or update these grids as the dimension increases similarly to what is usually done for the **caret** package. This gives you a wide range of possibilities and a lot of flexibility in the training phase.

To conclude this brief introduction, we present the usual *predict()* function which can be applied to a **swag** trained object similarly to many other packages in R. We pick the random forest learner for this purpose.

``` r
# best learner predictions 
# if `newdata` is not specified, then predict gives predictions based on the training 
# sample

sapply(predict(object = train_swag_rf), function(x) head(x))
```

    ## $predictions
    ##      [,1]
    ## [1,]    1
    ## [2,]    1
    ## [3,]    1
    ## [4,]    1
    ## [5,]    1
    ## [6,]    2
    ## 
    ## $models
    ## $models[[1]]
    ## [1] 3 5 6 7

``` r
# best learner predictions 
best_pred <- predict(object = train_swag_rf, 
                     newdata = x_test)

sapply(best_pred, function(x) head(x))
```

    ## $predictions
    ##      [,1]
    ## [1,]    1
    ## [2,]    1
    ## [3,]    1
    ## [4,]    2
    ## [5,]    1
    ## [6,]    1
    ## 
    ## $models
    ## $models[[1]]
    ## [1] 3 5 6 7

``` r
# predictions for a given dimension 

dim_pred <-  predict(
  object = train_swag_rf, 
  newdata = x_test, 
  type = "attribute",
  attribute = 4L)


sapply(dim_pred,function(x) head(x))
```

    ## $predictions
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    1    1    1
    ## [2,]    1    1    1    1
    ## [3,]    1    1    1    1
    ## [4,]    2    2    2    2
    ## [5,]    1    1    1    1
    ## [6,]    1    1    1    1
    ## 
    ## $models
    ## $models[[1]]
    ## [1] 2 3 5 6
    ## 
    ## $models[[2]]
    ## [1] 2 3 5 7
    ## 
    ## $models[[3]]
    ## [1] 3 5 6 7
    ## 
    ## $models[[4]]
    ## [1] 2 3 6 7

``` r
# predictions below a given CV error

cv_pred <-  predict(
  object = train_swag_rf, 
  newdata = x_test, 
  type = "cv_performance",
  cv_performance = 0.04)

sapply(cv_pred,function(x) head(x))
```

    ## $predictions
    ##      [,1]
    ## [1,]    1
    ## [2,]    1
    ## [3,]    1
    ## [4,]    2
    ## [5,]    1
    ## [6,]    1
    ## 
    ## $models
    ## $models[[1]]
    ## [1] 3 5 6 7

Now we can evaluate the performance of the best learner selected by **swag** thanks to the *confusionMatrix()* function of **caret**.

``` r
# transform predictions into a data.frame of factors with levels of `y_test`
best_learn <- factor(levels(y_test)[best_pred$predictions])
caret::confusionMatrix(best_learn,y_test)
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  benign malignant
    ##   benign        90         0
    ##   malignant      0        46
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9732, 1)
    ##     No Information Rate : 0.6618     
    ##     P-Value [Acc > NIR] : < 2.2e-16  
    ##                                      
    ##                   Kappa : 1          
    ##                                      
    ##  Mcnemar's Test P-Value : NA         
    ##                                      
    ##             Sensitivity : 1.0000     
    ##             Specificity : 1.0000     
    ##          Pos Pred Value : 1.0000     
    ##          Neg Pred Value : 1.0000     
    ##              Prevalence : 0.6618     
    ##          Detection Rate : 0.6618     
    ##    Detection Prevalence : 0.6618     
    ##       Balanced Accuracy : 1.0000     
    ##                                      
    ##        'Positive' Class : benign     
    ## 

Thanks for the attention. You can definitely say that you worked with **swag** !!!

Licensing
---------

The license this source code is released under is the GNU AFFERO GENERAL PUBLIC LICENSE (AGPL) v3.0. In some cases, the GPL license does apply. However, in the majority of the cases, the license in effect is the GNU AFFERO GENERAL PUBLIC LICENSE (AGPL) v3.0 as the computational code is heavily dependent on Armadilllo, which use the MPL license that enables us to recast our code to use the GNU AFFERO GENERAL PUBLIC LICENSE (AGPL) v3.0. See the LICENSE file for full text. Otherwise, please consult [TLDR Legal](https://tldrlegal.com/license/gnu-affero-general-public-license-v3-(agpl-3.0)) or [GNU](https://www.gnu.org/licenses/agpl-3.0.en.html) which will provide a synopsis of the restrictions placed upon the code. Please note, this does NOT excuse you from talking about licensing with a lawyer!
