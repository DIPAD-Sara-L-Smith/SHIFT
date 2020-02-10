#' Convert a dataframe to an XTS object. Only works for Year Quarters
#'
#' @param df
#'
#' @return an xts object generated from the dataframe.
#' @export
#' @importFrom zoo as.yearqtr
#' @importFrom xts xts
#' @importFrom dplyr select
df_to_xts <- function(df) {
  # I'm not sure why I need to convert stuff to xts...
  # This works for Year-Quarters only, it needs a re-think if we need to add
  # additional periodicities. Maybe add a switch case for Months/Weeks
  df_xts <- with(
    df,
    xts(
      df %>% select(-c(Year, Quarter)),
      as.yearqtr(Year + (Quarter-1)/4)
    )
  )
  return(df_xts)
}
