fit_models <- function(xf, ind_var, dep_var, start, end, forecasts){
  # Not tested
  fits <- lapply(forecasts, function(){
  switch(forecast,
         'holtwinters' = get_holtwinters(),
         'decomp' = get_decomp(),
         'naive' = get_naive(),
         'linear' = get_linear(),
         # Default case
         {warning('Forecast type not recognised.')
           NULL})

  })

}

#' fit_holtwinters - takes a data frame and returns the fit object for a
#' Holt-Winters forecast on the data.
#'
#' @param df data frame of data
#' @param ind_var string - name of indpendent variable to forecast
#' @param start vector - start year and quarter in format c(YYYY, Q)
#' @param end vector - end year and quarter in format c(YYYY, Q)
#'
#' @return fit object for a Holt-Winters forecast on the data in df
#' @export
#'
#' @importFrom stats HoltWinters
#' @importFrom stats window
fit_holtwinters <- function(df, ind_var, start, end){
  # subset to the selected variable
  df_xts <- df_to_xts(df, ind_var, start, end)

  # get fit object
  fit <- stats::HoltWinters(df_xts)
  return(fit)
}

#' fit_decomp - takes a data frame and returns the fit object for a time series
#' decomposition forecast on the data.
#'
#' @param df data frame of data
#' @param ind_var string - name of indpendent variable to forecast
#' @param start vector - start year and quarter in format c(YYYY, Q)
#' @param end vector - end year and quarter in format c(YYYY, Q)
#'
#' @return fit object for a time series decomposition forecast on the data in df
#' @export
#'
#' @importFrom stats stl
#' @importFrom stats window
fit_decomp <- function(df, ind_var, start, end){
  # subset to the selected variable
  df_xts <- df_to_xts(df, ind_var, start, end)

  # get fit object
  fit <- stats::stl(df_xts, s.window = "period")
  return(fit)
}

#' fit_naive - takes a data frame and returns the fit object for a naive
#' forecast on the data.
#'
#' @param df data frame of data
#' @param ind_var string - name of indpendent variable to forecast
#' @param start vector - start year and quarter in format c(YYYY, Q)
#' @param end vector - end year and quarter in format c(YYYY, Q)
#'
#' @return fit object for a naive forecast on the data in df
#' @export
#'
#' @importFrom forecast naive
fit_naive <- function(df, ind_var, start, end) {
  # subset to the selected variable
  df_xts <- df_to_xts(df, ind_var, start, end)

  # get fit object
  fit <- forecast::naive(df)
  return(fit)
}


fit_linear <- function(df, ind_var, dep_var, start, end){

}

predict_models <- function(fits ){

}
