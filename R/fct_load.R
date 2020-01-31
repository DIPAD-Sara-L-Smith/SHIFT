
#' Load and convert user selected data
#'
#' @param upload a dataframe containing a details of uploaded files.
#'
#' @return a dataframe containing the combines files
#' @export
#'
load_user_data <- function(upload) {
  return(user_data)
}


#' Merge a list of valid dataframes into a single dataframe.
#'
#' @param df_list A list of valid dataframes
#' @param cols The columns used to perform the join by. Default = c("Year", "Quarter")
#'
#' @return The merged dataframes as a single dataframe.
#' @export
#'
#' @examples
#' @importFrom purrr reduce
#' @importFrom dplyr full_join
merge_user_data <- function(df_list, cols = c("Year", "Quarter")) {
  df <-
    reduce(
      df_list,
      function(...) {
        full_join(..., by = cols)
      }
    )

  return(df)
}


#' load_r_file
#'
#' @param filename a string of the r file to be run.
#'
#' @return a dataframe containg the data.
#' @export
#'
#' @examples
load_r_file <- function(filename) {
  if (check_valid_df(df)) {
    return(df)
  }
}


#' load_csv
#'
#' @param filename a string containing the filename.
#'
#' @return a dataframe containg the data.
#' @export
#'
#' @examples
load_csv <- function(filename) {
  if (check_valid_df(df)) {
    return(df)
  }
}


#' check_valid_df -Determines whether a dataframe is valid to be used for forecasting
#'
#' @param df a single dataframe to be checked.
#'
#' @return a boolean describing whether the dataframe is valid.
#' @export
#'
#' @examples
#' @importFrom assertthat assert_that
check_valid_df <- function(df){

  # Is dataframe
  assert_that(is.data.frame(df),
              msg = "This doesn't appear to be a data frame.")

  # Has Years
  assert_that('Year' %in% names(df),
                          msg = "Dataframe does not contain a column called Year.")

  # Has Quarters
  assert_that('Quarter' %in% names(df),
                          msg = "Dataframe does not contain a column called Quarter.")

  # Data is continuous
  assert_that(all(unlist(df %>%
                                       select(-c('Year', 'Quarter')) %>%
                                       map(~is_continuous(.)))),
                          msg = "Data is not continuous.")

  # Some of the fit functions don't play nicely with non-standard column names, i.e., spaces
  # Check by comparing to the function which builds nice names
  # TODO should be force make.names onto each dataframe?
  assert_that(any(names(df) == make.names(names(df), unique = TRUE)),
                          msg = paste("The column headings contain some bad characters",
                                      "probably a space. Fix in the data or consider adding",
                                      "make.names(names(df)) to your data-prep script."))

  # TODO paramatise this so it only prints when logging is on.
  cat('Loaded a valid dataframe\n')
  return(TRUE)
}
