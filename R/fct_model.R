
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
  # run through all model fits and produce predictions
  predictions <- lapply(model_fits, function(fit){
    # TODO improve this check if fit is valid

    # Holt-Winters, Time Series Decomposition, Naive forecast -
    # don't need to consider projected independent variable data
    if (any(attr(fit, "class") %in% c("HoltWinters",
                                  "forecast",
                                  "stl"))) {
      return(forecast::forecast(fit, h = n_periods_to_forecast))
    } else if (all(attr(fit, "class") == c("tslm", "lm"))) {
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
          fan = FALSE,
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


#' get_forecast_plotdata
#' @description A function that takes the model fit object and returns a data
#' frame containing the x and y data required for a plot of the actuals,
#' forecast, and 95% confidence interval.
#'
#' @param fit - model object
#' @param proj_data - data frame containing forecast data for independent
#' variables.
#'
#' @return plotly line graph of forecast
#' @export
#'
#' @importFrom stats ts.union
#' @importFrom zoo as.yearqtr
get_forecast_plotdata <- function(fit, proj_data = NULL) {
  # calculate forecast
  fcast <- predict_models(model_fits = list(fit),
                          proj_indep_var = proj_data,
                          n_periods_to_forecast = 8)
  fcast <- unlist(fcast, recursive = FALSE)

  # find model type
  model_type <- switch(fcast$method,
                       "STL +  ETS(M,A,N)" = "decomposition",
                       "STL +  ETS(A,N,N)" = "decomposition",
                       "Naive method" = "naive",
                       "HoltWinters" = "holtwinters",
                       "Linear regression model" = "linear",

                       { warning(paste0("get_forecast_plotdata:
                                 Forecast type ", fcast$method,
                                        " not recognised."))
                         return(NULL) }
  )

  # gather data from forecast list
  if (model_type %in% c("decomposition", "naive", "linear", "holtwinters")) {
    ts_actuals <- fcast$x
    start_actuals <- start(ts_actuals)
    start_forecast <- start(fcast$fitted)
    freq_actuals <- frequency(ts_actuals)

    ts_forecast <- ts(c(fcast$fitted,
                        fcast$mean),
                      start = start_forecast,
                      frequency = freq_actuals)
    ts_lower_95 <- ts(c(fcast$fitted,
                        fcast$lower[, 2]),
                      start = start_forecast,
                      frequency = freq_actuals)
    ts_upper_95 <- ts(c(fcast$fitted,
                        fcast$upper[, 2]),
                      start = start_forecast,
                      frequency = freq_actuals)
  # } else if (model_type == "holtwinters") {
  #   # get time series for plot
  #   ts_actuals <- fcast$x
  #   start_actuals <- start(ts_actuals)
  #   start_forecast <- start(fcast$fitted[, "xhat"])
  #   freq_actuals <- frequency(ts_actuals)
  #
  #   ts_forecast <- ts(c(fcast$fitted[, "xhat"],
  #                       fcast$mean),
  #                     start = start_forecast,
  #                     frequency = freq_actuals)
  #   ts_lower_95 <- ts(c(fcast$fitted[, "xhat"],
  #                       fcast$lower[, "95%"]),
  #                     start = start_forecast,
  #                     frequency = freq_actuals)
  #   ts_upper_95 <- ts(c(fcast$fitted[, "xhat"],
  #                       fcast$upper[, "95%"]),
  #                     start = start_forecast,
  #                     frequency = freq_actuals)
  } else {
    # model_type not recognised
    warning("get_forecast_plotdata(): model_type not recognised from fit
            object.")
    return(NULL)
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
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom zoo as.yearqtr
plot_forecast <- function(df, dep_var, ind_var = NULL, start, end,
                          forecast_type, proj_data = NULL){
  # find start / end if not given
  if (missing(start)) {
    start <- c(df$Year[1], df$Quarter[1])
  }

  if (missing(end)) {
    end <- c(tail(df$Year, n = 1), tail(df$Quarter, n = 1))
  }

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
  data <- get_forecast_plotdata(fit, proj_data)

  # produce plot
  plot <- plotly::plot_ly(data = data,
                          x = ~x_labels) %>%
    plotly::add_trace(y = ~y.ts_upper_95,
              mode = "lines",
              name = "Upper 95% CI",
              color = I("sandy brown"),
              showlegend = FALSE,
              line = list(color = 'transparent'),
              type = "scatter",
              opacity = 0.75) %>%
    plotly::add_trace(y = ~y.ts_lower_95,
              mode = "lines",
              name = "Lower 95% CI",
              color = I("sandy brown"),
              fill = "tonexty",
              type = "scatter",
              showlegend = FALSE,
              line = list(color = 'transparent'),
              opacity = 0.75) %>%
    plotly::layout(title = paste0(tools::toTitleCase(forecast_type),
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
    plotly::add_trace(y = ~y.ts_actuals,
              mode = "lines+markers",
              type = "scatter",
              name = "Actuals",
              color = I("dim grey"),
              text = paste0(zoo::as.yearqtr(data$x_labels),
                            ": ",
                            round(data$y.ts_actuals))) %>%
    plotly::add_trace(y = ~y.ts_forecast,
              type = "scatter",
              mode = "lines+markers",
              name = "Forecast",
              color = I("chocolate"),
              text = paste0(zoo::as.yearqtr(data$x_labels),
                            ": ",
                            round(data$y.ts_forecast)))

  return(plot)
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


#' get_proj_data
#' @description Takes a data frame of time series data and an end point and
#' returns a data frame with just the data from after that point. To be used
#' to get project data for independent variables from a data frame of combined
#' historical and projection data.
#'
#' @param df data frame of data
#' @param end vector - end year and quarter in format c(YYYY, Q)
#'
#' @return data frame of projection data for all columns
#' @export
#'
#' @importFrom dplyr select arrange filter n
#' @importFrom tibble rownames_to_column
get_proj_data <- function(df, end){
  # check if df is valid
  if (is_valid_df(df)) {
    # sort data
    df_sorted <- df %>%
      dplyr::select(everything()) %>%
      tibble::rownames_to_column() %>%
      dplyr::arrange(Year, Quarter)

    # find last row of historical data
    df_row_number <- df_sorted %>%
      dplyr::filter(Year == end[1] & Quarter == end[2])

    # check if end date is in data
    if (nrow(df_row_number) == 0) {
      # end date isn't in data
      warning("get_proj_data: end date isn't in data in df.")
      return(NULL)
    } else if (nrow(df_row_number) > 1 ) {
      # multiple rows of data returned - raise error with user
      warning("get_proj_data: multiple rows of data in df match end date.")
      return(NULL)
    } else {
      # one row of data returned - get row index to use to filter out projected
      # data
      df_row_number <- as.numeric(df_row_number$rowname) + 1

      # filter data to only rows after the last row
      df_proj_data <- df_sorted %>%
        dplyr::slice(df_row_number:dplyr::n())
      return(df_proj_data)
    }
  } else {
    warning("get_proj_data: df is not a valid data frame.")
    return(NULL)
  }
}

