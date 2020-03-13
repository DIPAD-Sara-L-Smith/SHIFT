
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
#' @description A function that returns a list of forecasts for the input
#' model fits.
#'
#' @param model_fits list of model fit objects to use in forecast. Can use
#' fit_models() to calculate these.
#' @param proj_indep_var if linear regression model is included in model_fits,
#' this data frame is required, containing projected data to use in the forecast
#' @param n_periods_to_forecast integer - number of periods to forecast forward.
#' Default is 4 periods.
#'
#' @return data frame containing predictions/forecasts for multiple models
#' @importFrom forecast forecast
#' @export
predict_models <- function(model_fits, proj_indep_var = NULL,
                           n_periods_to_forecast = 4){
  browser()

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
      ind_var_names <- names(fit$data)
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

#' get_forecast_plotdata
#' @description A function that takes the model fit object and returns a data
#' frame containing the x and y data required for a plot of the actuals,
#' forecast, and 95% confidence interval.
#'
#' @param fit - model object
#'
#' @return plotly line graph of forecast
#' @export
#'
#' @importFrom stats ts.union
#' @importFrom zoo as.yearqtr
get_forecast_plotdata <- function(fit, proj_data = NULL) {
  browser()



  # calculate forecast
  fcast <- predict_models(model_fits = list(fit),
                          proj_indep_var = proj_data,
                          n_periods_to_forecast = 8)
  fcast <- unlist(fcast, recursive = FALSE)

  # find model type
  model_type <- switch(fcast$method,
                       "STL +  ETS(M,A,N)" = "decomposition",
                       "Naive method" = "naive",
                       "HoltWinters" = "holtwinters",
                       "Linear regression model" = "linear",

                       { warning(paste0("get_forecast_plotdata:
                                 Forecast type ", fcast$method,
                                        " not recognised."))}
  )

  # gather data from forecast list
  if (model_type %in% c("decomposition", "naive")) {
    ts_actuals <- fcast$x
    start_actuals <- start(ts_actuals)
    start_forecast <- start(fcast$fitted)
    freq_actuals <- frequency(ts_actuals)

    ts_forecast <- ts(c(fcast$fitted,
                        fcast$mean),
                      start = start_forecast,
                      frequency = freq_actuals)
    ts_lower_95 <- ts(c(fcast$fitted,
                        fcast$lower[, "95%"]),
                      start = start_forecast,
                      frequency = freq_actuals)
    ts_upper_95 <- ts(c(fcast$fitted,
                        fcast$upper[, "95%"]),
                      start = start_forecast,
                      frequency = freq_actuals)
  } else if (model_type == "holtwinters") {
    # get model forecast object
    fcast <- unlist(fcast, recursive = FALSE)

    # get time series for plot
    ts_actuals <- fcast$x
    start_actuals <- start(ts_actuals)
    start_forecast <- start(fcast$model$fitted[, "xhat"])
    freq_actuals <- frequency(ts_actuals)

    ts_forecast <- ts(c(fcast$fitted[, "xhat"],
                        fcast$mean),
                      start = start_forecast,
                      frequency = freq_actuals)
    ts_lower_95 <- ts(c(fcast$fitted[, "xhat"],
                        fcast$lower[, "95%"]),
                      start = start_forecast,
                      frequency = freq_actuals)
    ts_upper_95 <- ts(c(fcast$fitted[, "xhat"],
                        fcast$upper[, "95%"]),
                      start = start_forecast,
                      frequency = freq_actuals)
  } else {
    # TODO default / error

  }

  # put data together into a single data frame
  y <- stats::ts.union(ts_actuals, ts_forecast,
                       ts_lower_95, ts_upper_95)
  x_labels <- zoo::as.yearqtr(time(y))
  data <- as.data.frame(cbind(x_labels, y))

  return(data)
}


#' plot_forecast
#' @description A function that takes a data frame and returns the plot of the
#' selected forecast for the selected column of data.
#'
#' @param df data frame of data
#' @param dep_var string - name of dependent variable to forecast
#' @param start vector - start year and quarter in format c(YYYY, Q)
#' @param end vector - end year and quarter in format c(YYYY, Q)
#'
#' @return plotly line graph of forecast
#' @export
#'
#' @importFrom plotly plot_ly
#' @importFrom zoo as.yearqtr
plot_forecast <- function(df, dep_var, ind_var = NULL, start, end,
                          forecast_type){
  # browser()

  # find model
  forecast_type <- tolower(forecast_type)
  fit <- switch(forecast_type,
    "holtwinters" = fit_holtwinters(df, dep_var, start, end),
    "naive" = fit_naive(df, dep_var, start, end),
    "decomposition" = fit_decomp(df, dep_var, start, end),
    "linear" = fit_linear(df, dep_var, ind_var, start, end),
    warning("plot_forecast: forecast_type not recognised.")
  )

  # get plot data from fit object
  data <- switch(forecast_type,
                 "holtwinters" = get_holtwinters_plotdata(fit),
                 "naive" = get_naive_plotdata(fit),
                 "decomposition" = get_decomposition_plotdata(fit),
                 "linear" = get_linearreg_plotdata(fit),
                 warning("plot_forecast: forecast_type not recognised.")
  )

  # produce plot
  plot <- plotly::plot_ly(data = data,
                  x = ~x_labels) %>%
    add_trace(y = ~y.ts_upper_95,
              mode = "lines",
              name = "Upper 95% CI",
              color = I("sandy brown"),
              showlegend = FALSE,
              line = list(color = 'transparent'),
              type = "scatter",
              opacity = 0.75) %>%
    add_trace(y = ~y.ts_lower_95,
              mode = "lines",
              name = "Lower 95% CI",
              color = I("sandy brown"),
              fill = "tonexty",
              type = "scatter",
              showlegend = FALSE,
              line = list(color = 'transparent'),
              opacity = 0.75) %>%
    layout(title = paste0(tools::toTitleCase(forecast_type),
                          " forecast of ",
                          tools::toTitleCase(dep_var),
                          " (with 95% CI)"),
           paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
           xaxis = list(title = "",
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        zeroline = FALSE),
           yaxis = list(title = "",
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        zeroline = FALSE)) %>%
    add_trace(y = ~y.ts_actuals,
              mode = "lines+markers",
              type = "scatter",
              name = "Actuals",
              color = I("dim grey"),
              text = paste0(zoo::as.yearqtr(data$x_labels),
                ": ",
                round(data$y.ts_actuals))) %>%
    add_trace(y = ~y.ts_forecast,
              type = "scatter",
              mode = "lines+markers",
              name = "Forecast",
              color = I("chocolate"),
              text = paste0(zoo::as.yearqtr(data$x_labels),
                            ": ",
                            round(data$y.ts_forecast)))

  return(plot)
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
  # get time series object containing only the relevant variables
  vars <- append(dep_var, ind_var)
  df_ts <- df_to_ts(df, vars, start, end)

  # build and return lm model object
  fit <- forecast::tslm(formula = as.formula(
    paste0(dep_var, " ~ ", paste(ind_var, " + "), "season")
    ), data = df_ts)
  return(fit)
}
