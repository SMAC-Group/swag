
# create a betareg caret model
# type and library
betaregression <- list(type='Regression',
                       library='betareg',
                       loop=NULL)

# parameters to tune
prm <- data.frame(parameter=c("link", "type", "link.phi"),
                  class= rep("character", 3))

# add to the model
betaregression$parameters <- prm

# grid search, this is the default grid search, user can specify otherwise
# creates 54 separate models, so if looking to speed up try fewer params in grid
betaGrid  <- function(x, y, len=NULL, search="grid"){
  if(search == "grid"){
    out <- expand.grid(link=c("logit"),
                       # link=c("logit", "probit", "cloglog", "cauchit", "log", "loglog"),
                       type=c("ML"),
                       # type=c("ML", "BC", "BR"),
                       link.phi=c("identity"),
                       # link.phi=c("identity", "log", "sqrt"),
                       stringsAsFactors = F) # here force the strings as character,
    # othewise get error that the model arguments
    # were expecting 'chr' when fitting
  }
  out
}

# add the grid search
betaregression$grid <- betaGrid

# create the fit
betaFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...){

  dat <- if(is.data.frame(x)) x else as.data.frame(x)
  dat$.outcome <- y

  theDots <- list(...)

  modelArgs <- c(list(formula = as.formula(".outcome ~ ."), data = dat, link=param$link, type=param$type), theDots)

  out <- do.call(betareg::betareg, modelArgs)
  out$call <- NULL
  out
}

# betaregression fit
betaregression$fit <- betaFit

# predict element
betaPred <- function(modelFit, newdata, preProc=NULL, submodels=NULL){
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
  betareg::predict(modelFit, newdata)
}

# add the predict method
betaregression$predict <- betaPred

# regression, no probabities calculated
# just assigning NULL didnt work for some reason
# wrapped in a function instead
betaProb <- function(){
  return(NULL)
}
betaregression$prob <- betaProb


# load a dataset
library(dplyr)
data('GasolineYield', package = 'betareg')
y = GasolineYield$yield
x = GasolineYield %>% select(-yield)
str(x)
colnames(x)
set.seed(123)
id_to_na = sample(1:length(y), size = round(.2 * length(y)),replace=F)
y[id_to_na] = NA

randomly_set_NA <- function(df, prop_NA, seed = NULL) {
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe.")
  }

  if (prop_NA < 0 || prop_NA > 1) {
    stop("Proportion of NA values must be between 0 and 1.")
  }

  # Set the random seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Create a copy of the original dataframe to avoid modifying it directly
  df_new <- df

  # Iterate over each column
  for (col_name in names(df)) {
    # Check the class of the column
    col_class <- class(df[[col_name]])

    # Get the total number of elements in the column
    total_elements <- length(df[[col_name]])

    # Calculate the number of NA values to set in this column
    num_NA <- round(prop_NA * total_elements)

    # Randomly select the indices to set to NA
    na_indices <- sample(seq_len(total_elements), num_NA)

    # Set the selected indices to NA
    df_new[[col_name]][na_indices] <- NA

    # Convert the column class back to its original type
    if (col_class != "character") {
      df_new[[col_name]] <- as(df_new[[col_name]], col_class)
    }
  }

  return(df_new)
}


x_w_na = randomly_set_NA(x, prop_NA = .2, seed = 123)
str(x_w_na)

swagcon <- swagControl(pmax = 6L,
                       alpha = .5, #normally a small value, corresponds to the quantile
                       m = 20L,
                       seed = 163L, #for replicability
                       verbose = T #keeps track of completed dimensions
)


train_swag_betareg <- swag(
  # arguments for swag
  x = x_w_na,
  y = y,
  control = swagcon,
  auto_control = FALSE,
  # arguments for caret
  trControl = caret::trainControl(method = "repeatedcv", number = 5, repeats = 1, allowParallel = F),
  metric = "RMSE",
  method = betaregression,
)

train_swag_betareg$CVs
train_swag_betareg$VarMat
train_swag_betareg
