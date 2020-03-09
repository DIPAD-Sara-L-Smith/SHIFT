
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
predict_models <- function(model_fits, n_periods_to_forecast = 4){
  # run through all model fits and produce predictions
  predict_models <- lapply(model_fits, function(fit){
    # TODO improve this check if fit is valid
    if (any(attr(fit, "class") %in% c("HoltWinters",
                                  "forecast",
                                  "stl",
                                  "tslm"))) {
      return(forecast::forecast(fit, h = n_periods_to_forecast))
    } else {
      warning("Error in predict_models(): fit object not valid.")
    }
  })
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
  fit <- forecast::naive(df)
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
