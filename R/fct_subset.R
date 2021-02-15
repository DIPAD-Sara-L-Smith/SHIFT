#' Extracts lm and model string from regsubsets object
#'
#' @param models regsubsets object
#' @param modelindex Index for model
#' @param data The dataframe containing the regression data
#'
#' @export

model.from.regobject <- function(models, modelindex, data) {
  arraywhich <- summary(models)$which

  # find a boolean array from which matrix, telling us which variables
  # are to be included in the model, e.g. [1, 0, 0, 1, 1].
  # Note that the first element represents the intercept.
  includevar <- arraywhich[modelindex, ]

  # combine the relevant column names into a string to use to define the glm()
  str <- ""
  for (i in 2:ncol(arraywhich)) {
    if ((includevar[i]) == TRUE) {
      str <-
        paste(str, colnames(arraywhich)[i],
          sep = ifelse(str == "", "", " + ")
        )
    }
  }

  # formulate string for use as a formula in the model definition
  str <- reformulate(str, response = colnames(data)[1])

  # Gets string representation of formula
  formula <- paste(deparse(str), collapse = " ")

  # Trims whitespace, leaving single spaces between formula args
  formula <- trimws(gsub("\\s+", " ", formula))

  output <- list(formula)

  return(output)
}

#' Helper function for cross-validation; model generator
#'
#' @param id The id for the model
#' @param models regsubsets object
#' @param dep_var The dependent variable as a string
#'
#' @export
get_model_formula <- function(id, models, dep_var) {
  # get models data
  model <- summary(models)$which[id, -1]
  # Get model predictors
  predictors <- names(which(model == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  formula <- as.formula(paste0(dep_var, "~", predictors))

  return(formula)
}

#' Helper function for cross-validation; sampling part
#'
#' @param model.formula The formula for the model
#' @param data The dataframe containing the regression data
#'
#' @return
#' @export
#' @importFrom caret trainControl train
get_cv_error <- function(model.formula, data) {
  set.seed(1)
  # Creates sample of the data
  train.control <-
    trainControl(method = "cv", number = 10)
  # Conducts the training on the data
  cv <- train(
    model.formula,
    data = data,
    method = "lm",
    trControl = train.control
  )
  results <- cv$results$RMSE
  return(results)
}

#' Extracts an equation from the lm object
#'
#' @param model The lm object for the model
#'
#' @return
#' @export
#' @importFrom dplyr case_when if_else
model_equation <- function(model, ...) {
  format_args <- list(...)

  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(
    model_coeff_sign == -1 ~ " - ",
    model_coeff_sign == 1 ~ " + ",
    model_coeff_sign == 0 ~ " + "
  )
  model_eqn <- paste(
    strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
    "=",
    paste(if_else(model_coeff[1] < 0, "- ", ""),
      do.call(format, format_args)[1],
      paste(model_coeff_prefix[-1],
        do.call(format, format_args)[-1],
        " * ",
        names(model_coeff[-1]),
        sep = "", collapse = ""
      ),
      sep = ""
    )
  )
  return(model_eqn)
}

#' Helper function for writing signif. codes for rows in lm_summaries
#'
#' @param x Generic variable for current column
#'
#' @return
#' @export
ifelsefun <- function(x) {
  ifelse(x > 0 & x <= 0.001, "***",
    ifelse(x > 0.001 & x <= 0.01, "**",
      ifelse(x > 0.01 & x <= 0.05, "*",
        ifelse(x > 0.05 & x <= 0.1, ".",
          ifelse(x > 0.1 & x <= 1, " ", " ")
        )
      )
    )
  )
}

#' Conduct best subset regression
#' Conducts exhaustive best subset, cross-validation and stepwise search
#'
#' @param dep_var The dependent variable as a string
#' @param data The dataframe containing the regression data
#' @param nvars The number of variables to use in the best subset model
#'
#' @return
#' @export
#' @importFrom leaps regsubsets
#' @importFrom purrr map map2
#' @importFrom MASS stepAIC
#' @importFrom magrittr %>%
#' @importFrom reshape melt
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_wrap
#' @importFrom plotly ggplotly
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' @importFrom gridExtra grid.arrange
#' @import ggplot2
#' @import lattice
#' @import ggfortify
allsubsetregression <- function(dep_var, data, nvars) {
  data <- select(data, -c("Year", "Quarter"))

  models <-
    regsubsets(
      formula(paste0(dep_var, "~.")),
      data = data,
      nvmax = nvars,
      nbest = 4,
      method = "exhaustive"
    )
  res_sum <- summary(models)

  # Extracts required statistical measures from summary object
  res_df <- as.data.frame(res_sum[2:6])

  # Pulls dataframe index to use as model number
  res_df$`Model Number` <- as.numeric(rownames(res_df))

  # Extracts lm and formula string from regsubset object for each model
  formula_return <- 1:nrow(res_df) %>%
    map(~ model.from.regobject(models, .x, data)) %>%
    unlist()

  # Gets dataframe ready for plotting
  res_df <- melt(res_df, id = c("Model Number"))

  # Grabs model formula ready for joining onto res_df
  df <- as.data.frame(formula_return)
  names(df) <- "Model formula"
  df$`Model Number` <- as.numeric(rownames(df))

  # Does the join
  res_df <- merge(res_df, df, by = "Model Number")

  # reorders variables ready for plots
  res_df$variable <- factor(res_df$variable,
    levels = c("cp", "rss", "bic", "rsq", "adjr2")
  )

  # Generates plot of best subset
  plot <-
    ggplot(data = res_df, aes(
      x = `Model Number`,
      y = value,
      `Model formula` = `Model formula`, group = 1
    )) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "steelblue") +
    facet_wrap(~variable, scales = "free")

  plot <- ggplotly(plot)

  # Generates the results dataframe
  model_summaries_df <- pivot_wider(format(res_df, digits = 5),
    names_from = "variable",
    values_from = "value"
  )

  # Sorts by rsq
  model_summaries_df <- model_summaries_df[order(model_summaries_df$rsq,
    decreasing = TRUE
  ), ]

  # Compute cross-validation error
  model_ids <- 1:nrow(df)
  cv_errors <-
    map(model_ids, get_model_formula, models, dep_var) %>%
    map(get_cv_error, data = data) %>%
    unlist()

  # Select the model that minimize the CV error
  cv_model_index <- which.min(cv_errors)

  # stepwise search for best regression model
  fit <- lm(formula(paste0(dep_var, "~.")), data = data)
  step <- stepAIC(fit, direction = "both")

  # Pulls formula from lm object
  step <- paste(paste(step$terms[[2]]),
    substring(paste(step$terms[[3]], collapse = " + "), 5),
    sep = " ~ "
  )

  # Trims whitespace, leaving single spaces between formula args
  step <- trimws(gsub("\\s+", " ", step))

  # Table of all formulas
  formulas <- c(
    formula_return[[which.max(res_sum$rsq)]],
    formula_return[[which.max(res_sum$adjr2)]],
    formula_return[[which.min(res_sum$cp)]],
    formula_return[[which.min(res_sum$bic)]],
    formula_return[[which.min(res_sum$rss)]],
    formula_return[[cv_model_index]],
    step
  )

  # Title of measures employed
  stat_measure <- c("rsq", "adjr2", "cp", "bic", "rss", "cross-validation", "stepwise search (both)")

  # Model as per statistic
  determined_models <- do.call(rbind, Map(data.frame, stat_measure = stat_measure, formulas = formulas))

  # Renames columns
  names(determined_models) <- c("Statistical Measure", "Best Model for Statistic")

  # Resets row indices
  rownames(determined_models) <- NULL

  # Gets the final best models from analysis
  formula_results <- unique(formulas)

  # Generates lm objects on best models
  lm_results <- map(formula_results, lm, data)

  # Equations from the models
  lm_equations <- map(lm_results, model_equation)

  # Binds the equations onto the model formulas
  formula_table <- do.call(rbind, Map(data.frame, formula_results = formula_results, lm_equations = lm_equations))

  # Replaces .x argument with dependent variable name
  formula_table$lm_equations <- gsub("^.{0,2}", dep_var, formula_table$lm_equations)

  # Renames columns
  names(formula_table) <- c("Model Formula", "Model Equation")

  # Generates summary of coefficients for each lm model
  lm_summaries <- lm_results %>%
    map(~ as.data.frame(summary(.x)$coeff))

  # Adds signif. codes column to lm_summaries tables
  for (i in seq(lm_summaries))
  {
    lm_summaries[[i]]$Signif_codes <- ifelsefun(lm_summaries[[i]]$`Pr(>|t|)`)
  }

  # lm object diagnostic plots
  autoplots <- map(lm_results, autoplot, which = 1:6, label.size = 3, colour = "dodgerblue3", smooth.colour = "black")

  # Adds a title to the autoplots
  autoplots <- map2(autoplots, formula_results, ~ grid.arrange(grobs = .x@plots, top = .y))

  resultslist <-
    list(model_summaries_df, plot, formula_results, lm_summaries, autoplots, determined_models, formula_table, lm_results)

  names(resultslist) <- c("model_summaries_df", "plot", "formula_results", "lm_summaries", "autoplots", "determined_models", "formula_table", "lm_results")

  return(resultslist)
}
