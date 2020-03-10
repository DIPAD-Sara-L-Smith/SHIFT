
## Fit / Predict models (all types) -----

#' fit_models - find model fit objects for all selected model types
#'
#' @param df data frame containing data
#' @param dep_var string - name of dependent variable to forecast
#' @param ind_var string - vector of names of independent variables to fit
#' @param start vector - start year and quarter in format c(YYYY, Q)
#' @param end vector - end year and quarter in format c(YYYY, Q)
#' @param forecasts list of string values detailing which forecasts to include
#'
#' @return list of model fit objects
#' @importFrom forecast forecast
#' @export
fit_models <- function(df, dep_var, ind_var, start, end, forecasts_to_include){
  # check all lower case
  forecasts_to_include <- tolower(forecasts_to_include)

  # Run through the list of forecasts and get fit for each relevant model
  fits <- lapply(forecasts_to_include, function(model_type){
    model_fit <- switch(model_type,
           'holtwinters' = fit_holtwinters(df, dep_var, start, end),
           'decomposition' = fit_decomp(df, dep_var, start, end),
           'naive' = fit_naive(df, dep_var, start, end),
           'linear' = fit_linear(df, dep_var, ind_var, start, end),

           # Default case
           {warning(paste0('Forecast type - ', model_type, ' - not recognised.'))
             return(NULL)
             }
           )
    return(model_fit)
  })

  # rename list so we can easily extract the one we need
  names(fits) <- forecasts_to_include
  return(fits)
}

#' predict_models - calculate forecasts
#'
#' @param fit list - model object(s) containing fit

#' @param dep_var string - name of dependent variable to forecast
#' @param ind_var string - vector of names of independent variables to fit
#' @param start vector - start year and quarter in format c(YYYY, Q)
#' @param end vector - end year and quarter in format c(YYYY, Q)
#'
#' @return data frame containing predictions/forecasts for multiple models
#' @importFrom forecast forecast
#' @export
predict_models <- function(model_fits, proj_indep_var,
                           n_periods_to_forecast = 4){
  # run through all model fits and produce predictions
  predictions <- lapply(model_fits, function(fit){
    # TODO improve this check if fit is valid

    # Holt-Winters, Time Series Decomposition, Naive forecast -
    # don't need to consider projected independent variable data
    if (any(attr(fit, "class") %in% c("HoltWinters",
                                  "forecast",
                                  "stl"))) {
      return(forecast::forecast(fit, h = n_periods_to_forecast))
    } else if (attr(fit, "class") == "tslm") {
      # linear regression model -
      # different predict call to consider projected independent variable data
      # First, check if we have all the data we need to forecast
      ind_var_names <- names(myfit$data)
      ind_var_names <- ind_var_names[which(ind_var_names %not_in% c("data",
                                                                "trend",
                                                                "season"))]

      if (all(ind_var_names %in% names(proj_indep_var))) {
        fcast <- forecast::forecast(
          object = fit,
          newdata = as.data.frame(proj_indep_var),
          level = c(80, 95),
          fan = TRUE,
          h = n_periods_to_forecast
        )
        return(fcast)
      } else {
        # Projected data missing - alert user
        warning(paste0("Linear regression forecast not produced due to the
                       following not having data in the projected independent
                       variable dataset: "))
        return(NULL)
      }
    } else {
      warning(paste0("Error in predict_models(): fit object - ",
        attr(fit, "class"),
        "not valid."))
      return(NULL)
    }
  })

  # rename list
  names(predictions) <- names(model_fits)
  return(predictions)
}



## Holt-Winters -----

#' fit_holtwinters - takes a data frame and returns the fit object for a
#' Holt-Winters forecast on the data.
#'
#' @param df data frame of data
#' @param dep_var string - name of dependent variable to forecast
#' @param start vector - start year and quarter in format c(YYYY, Q)
#' @param end vector - end year and quarter in format c(YYYY, Q)
#'
#' @return fit object for a Holt-Winters forecast on the data in df
#' @export
#'
#' @importFrom stats HoltWinters
#' @importFrom stats window
fit_holtwinters <- function(df, dep_var, start, end){
  # subset to the selected variablef
  df_ts <- df_to_ts(df, dep_var, start, end)

  # get fit object
  fit <- stats::HoltWinters(df_ts)
  return(fit)
}


## Time Series Decomposition -----

#' fit_decomp - takes a data frame and returns the fit object for a time series
#' decomposition forecast on the data.
#'
#' @param df data frame of data
#' @param dep_var string - name of dependent variable to forecast
#' @param start vector - start year and quarter in format c(YYYY, Q)
#' @param end vector - end year and quarter in format c(YYYY, Q)
#'
#' @return fit object for a time series decomposition forecast on the data in df
#' @export
#'
#' @importFrom stats stl
#' @importFrom stats window
fit_decomp <- function(df, dep_var, start, end){
  # subset to the selected variable
  df_ts <- df_to_ts(df, dep_var, start, end)

  # get fit object
  fit <- stats::stl(df_ts[, dep_var], s.window = "period")
  return(fit)
}


## Naive -----

#' fit_naive - takes a data frame and returns the fit object for a naive
#' forecast on the data.
#'
#' @param df data frame of data
#' @param dep_var string - name of dependent variable to forecast
#' @param start vector - start year and quarter in format c(YYYY, Q)
#' @param end vector - end year and quarter in format c(YYYY, Q)
#'
#' @return fit object for a naive forecast on the data in df
#' @export
#'
#' @importFrom forecast naive
fit_naive <- function(df, dep_var, start, end) {
  # subset to the selected variable
  df_ts <- df_to_ts(df, dep_var, start, end)

  # get fit object
  fit <- forecast::naive(df_ts)
  return(fit)
}


## Linear Regression -----

# TODO set defaults for start/end throughout this script

#' fit_linear - takes a data frame and returns the fit object for a linear
#' regression forecast on the data.
#'
#' @param df data frame of data
#' @param dep_var string - name of dependent variable to forecast
#' @param ind_var string - vector of names of independent variables to fit
#' @param start vector - start year and quarter in format c(YYYY, Q)
#' @param end vector - end year and quarter in format c(YYYY, Q)
#'
#' @return fit object for a linear regression forecast on the data in df
#' @export
#'
#' @importFrom forecast tslm
fit_linear <- function(df, dep_var, ind_var, start, end){
  # get time series object
  vars <- append(dep_var, ind_var)
  df_ts <- df_to_ts(df, vars, start, end)

  # build and return lm model object
  fit <- forecast::tslm(formula = as.formula(paste0(dep_var, " ~ . + season")),
                        data = df_ts)
  return(fit)
}
