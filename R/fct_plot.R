#' Functions which help with plotting
#'
#' @param df a dataframe
#'
#' @return dygraph to be rendered.
#' @export
#' @importFrom dygraphs dygraph
df_to_dygraph <- function(df){


  return(df)
}

# Function to return time series object
df_to_ts <- function(df, intFreq, intStartYear, intStartPeriod) {
  # Setup missing values
  if (missing(intFreq)) {
    intFreq <- 1
  }

  if (missing(intStartYear)) {
    intStartYear = 1
  }

  if (missing(intStartPeriod)) {
    intStartPeriod = 1
  }

  if (is.null(input$DataFilePath)) {
    # User has not uploaded a file yet
    return(NULL)
  } else {

    # find start point
    if (is.null(intStartYear)) {
      # warning("No integer year variable selected - time series may not display
      #         correctly. Number of periods set by default to 1.")
      getTimeSeries <- stats::ts(df, frequency = 1)
    } else {
      if (is.null(intStartPeriod)) {
        # warning("No integer period variable selected - time series may not
        #         display correctly. Number of periods set by default to 1.")
        getTimeSeries <- stats::ts(df, frequency = 1,
                                   start = c(intStartYear, 1))
      } else {
        strStart <- c(intStartYear, intStartPeriod)
        getTimeSeries <- stats::ts(df,
                                   frequency = intFreq,
                                   start = strStart)

      }
    }

    return(getTimeSeries)
  }
}
