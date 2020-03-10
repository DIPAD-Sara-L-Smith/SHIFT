#' Convert a dataframe to an XTS object. Only works for Year Quarters
#'
#' @param df
#'
#' @return an xts object generated from the dataframe.
#' @export
#' @importFrom zoo as.yearqtr
#' @importFrom xts xts
#' @importFrom dplyr select
df_to_xts <- function(df, selected_vars = NULL, start = NULL,
                      end = NULL) {
  # get list of selected variables
  # (if none specified in arguments, include all variables)
  if (is.null(selected_vars)) {
    selected_vars <- colnames(df)
  } else {
    # check Year and Quarter are included
    if ("Year" %not_in% selected_vars) {
      selected_vars <- append(selected_vars, "Year")
    }

    if ("Quarter" %not_in% selected_vars) {
      selected_vars <- append(selected_vars, "Quarter")
    }
  }

  # subset data
  df_selected <- df %>%
    select(one_of(selected_vars))

  # get time series object
  # I'm not sure why I need to convert stuff to xts...
  # This works for Year-Quarters only, it needs a re-think if we need to add
  # additional periodicities. Maybe add a switch case for Months/Weeks
  df_xts <- with(
    df_selected,
    xts::xts(
      df_selected %>% select(-c(Year, Quarter)),
      zoo::as.yearqtr(Year + (Quarter - 1) / 4)
    )
  )

  # subset to the selected time period
  # TODO Check start/end valid quarters?
  if (!is.null(start) && !is.null(end)) {
    df_xts <- stats::window(df_xts,
                            start = zoo::as.yearqtr(start),
                            end = zoo::as.yearqtr(end))
  } else {
    df_xts <- df_xts
  }

  return(df_xts)
}


#' Convert a dataframe to an time series object. Only works for Year Quarters.
#'
#' @param df
#'
#' @return an ts object generated from the dataframe.
#' @export
#' @importFrom zoo as.yearqtr
#' @importFrom stats ts
#' @importFrom dplyr select
df_to_ts <- function(df, selected_vars = NULL, start = NULL,
                      end = NULL) {
  # get list of selected variables
  # (if none specified in arguments, include all variables)
  if (is.null(selected_vars)) {
    selected_vars <- colnames(df)
  } else {
    # check Year and Quarter are included
    if ("Year" %not_in% selected_vars) {
      selected_vars <- append(selected_vars, "Year")
    }

    if ("Quarter" %not_in% selected_vars) {
      selected_vars <- append(selected_vars, "Quarter")
    }
  }

  # subset data
  df_selected <- as.data.frame(df) %>%
    select(one_of(selected_vars))

  # get time series object
  # This works for Year-Quarters only, it needs a re-think if we need to add
  # additional periodicities. Maybe add a switch case for Months/Weeks
  df_ts <- stats::ts(
      df_selected %>% select(-c(Year, Quarter)),
      frequency = 4,
      start = c(df_selected[1, "Year"], df_selected[1, "Quarter"])
  )

  # subset to the selected time period
  # TODO Check start/end valid quarters?
  if (!is.null(start) && !is.null(end)) {
    df_ts <- stats::window(df_ts,
                            start = start,
                            end = end)
  } else {
    df_ts <- df_ts
  }

  return(df_ts)
}

#' Converts a xts object into a dataframe compatible with SHIFT
#'
#' @param x an xts object
#'
#' @return a dataframe
#' @importFrom zoo index
#' @importFrom tibble as_tibble
#' @importFrom tidyr separate
xts_to_df <- function(x) {
  # Again this works for only year quarters. If we expand the app it needs a
  # rethink.
  df <- cbind(
    Date = zoo::index(x),
    tibble::as_tibble(x)
  ) %>%
    tidyr::separate(Date,
      into = c("Year", "Quarter"),
      sep = " Q",
      convert = TRUE
    )
  return(df)
}

#' Calculates differenced data for the selected data frame.
#' This is used in the forecasting to make the data stationary.
#' Note that it excludes columns: 'Year', 'Quarter'.
#'
#' @param df data frame to be differencd
#'
#' @return a dataframe
diff_df <- function(df){
  # check if data frame is valid
  if (is_valid_df(df)) {
    # separate out 'Year' and 'Quarter' (we don't want to difference these)
    time_variables <- df %>%
      select(one_of(c("Year", "Quarter")))

    data_variables <- df %>%
      select(-one_of(c("Year", "Quarter")))

    # calculate differences
    differenced_data <- sapply(data_variables, function(my_column) {
      diff(as.numeric(my_column))
      })

    # recombine columns
    return(dplyr::bind_cols(head(time_variables, -1),
                            as.data.frame(differenced_data)))
  } else {
    warning("Data frame being differenced is not valid.")
  }
}

# Here is an idea for a test for these functions for when we get round to adding
# the tests.
# > identical(df, xts_to_df(df_to_xts(df)))
# [1] TRUE
