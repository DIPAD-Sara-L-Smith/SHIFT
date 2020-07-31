#' Helper function for cross-validation; model generator
#'
#' @param model.formula A vector of length
#' @param data The dataframe containing the regression data
#' @param nvars The max number of variables to use in the best subset model
#'
#' @export
get_model_formula <- function(id, object, outcome) {
  # get models data
  models <- summary(object)$which[id, -1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

#' Helper function for cross-validation; sampling part
#'
#' @param model.formula A vector of length
#' @param data The dataframe containing the regression data
#' @param nvars The max number of variables to use in the best subset model
#'
#' @return
#' @export
#' @importFrom caret trainControl train
get_cv_error <- function(model.formula, data, nvars) {
  set.seed(1)
  # Creates sample of the data
  train.control <-
    trainControl(method = "cv", number = nvars)
  # Conducts the training on the data
  cv <- train(
    model.formula,
    data = data,
    method = "lm",
    trControl = train.control
  )
  cv$results$RMSE
}

#' Conduct best subset regression
#'
#' @param dep.var The dependent variable as a string
#' @param data The dataframe containing the regression data
#' @param nvars The number of variables to use in the best subset model 
#'
#' @return
#' @export
#' @importFrom leaps regsubsets
#' @importFrom purrr map
#' @importFrom MASS stepAIC
#' @importFrom magrittr %>%
outputs <- function(dep.var, data, nvars)
{
  models <-
    regsubsets(formula(paste0(dep.var, "~.")), nbest = 4, data = data, nvmax = nvars)
  res.sum <- summary(models)
  
  # Returns the statistical measures for determining best subset
  stats.measures <- data.frame(
    Adj.R2 = which.max(res.sum$adjr2),
    CP = which.min(res.sum$cp),
    BIC = which.min(res.sum$bic),
    RSS = which.min(res.sum$rss)
  )
  
  # Compute cross-validation error  
  model.ids <- 1:nvars
  cv.errors <-
    map(model.ids, get_model_formula, models, dep.var) %>%
    map(get_cv_error, data = data, nvars = nvars) %>%
    unlist()
  
  best.model <- which.min(cv.errors)
  #print(best.model)
  
  # Select the model that minimize the CV error
  coef(models, best.model)
  
  # stepwise search for best regression model
  fit <- lm(formula(paste0(dep.var, "~.")), data=data)
  step <- stepAIC(fit, direction="both")
  #step$anova # display results
}

# Load the data
data("swiss")

# call function
outputs("Fertility", swiss, 5)
