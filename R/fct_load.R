
#' Load and convert user selected data
#'
#' @param upload a dataframe containing a details of uploaded files.
#'
#' @return a dataframe containing the combines files
#' @export
#' @importFrom tools file_ext file_path_sans_ext
load_user_data <- function(upload) {


  if (file_ext(upload$name) == "csv") {
    cat("Loading a csv file\n")
    df <- load_csv(upload$datapath)
  }

  user_data <- df
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
load_r_file <- function(filename) {
  if (is_valid_df(df)) {
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
#' @importFrom vroom vroom
load_csv <- function(filename) {
  df <- vroom(filename,
    delim = ","
  )

  if (is_valid_df(df)) {
    return(df)
  }
}


#' is_valid_df -Determines whether a dataframe is valid to be used for forecasting
#'
#' @param df a single dataframe to be checked.
#'
#' @return a boolean describing whether the dataframe is valid.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr select
#' @importFrom purrr map
is_valid_df <- function(df) {

  # Is dataframe
  assert_that(is.data.frame(df),
    msg = "This doesn't appear to be a data frame."
  )

  # Has Years
  assert_that("Year" %in% names(df),
    msg = "Dataframe does not contain a column called Year."
  )

  # Has Quarters
  assert_that("Quarter" %in% names(df),
    msg = "Dataframe does not contain a column called Quarter."
  )

  # Data is continuous
  assert_that(all(unlist(df %>%
    select(-c("Year", "Quarter")) %>%
    map(~ is_df_continuous(.)))),
  msg = "Data is not continuous."
  )

  # Some of the fit functions don't play nicely with non-standard column names, i.e., spaces
  # Check by comparing to the function which builds nice names
  # TODO replace messy call with one to function.
  # TODO should be force make.names onto each dataframe?
  assert_that(any(names(df) == make.names(names(df), unique = TRUE)),
    msg = paste(
      "The column headings contain some bad characters",
      "probably a space. Fix in the data or consider adding",
      "make.names(names(df)) to your data-prep script."
    )
  )

  # TODO paramatise this so it only prints when logging is on.
  cat("Loaded a valid dataframe\n")
  return(TRUE)
}



#' is_df_continous determines whether there are any missing values in the middle of a dataframe column.
#'
#' @param df
#'
#' @return A boolean indicting whether there is missing data.
#' @export
#'
is_df_continuous <- function(df) {
  # TODO investigate using zoo:trim_na to make this function a bit clearer.
  # Checks whether a dataset column is continuous with no missing values.
  # NA are allowed at the begining and end as datasets have different history lengths

  # Find the index of all missing values and take the diff
  index_diff <- diff(which(!is.na(df)))

  # NAs are allowed at beginning and end so drop those
  index_diff <- index_diff[2:length(index_diff)]

  # If the the data is continuous the diff indexes will always = 1
  continuous <- all(index_diff == 1)

  if (!continuous) {
    cat("Column: ", names(df), " is continuous, it has missing values in the middle\n")
    cat("Consider using dplyr::fill or fill_interp in your data prep script.\n")
  }

  return(continuous)
}


#' are_df_names_valid determines whether df has appropriate names. I.e., no spaces.
#'
#' @param col_names
#'
#' @return A boolean indicating whether the names are valid.
#' @export
are_df_names_valid <- function(col_names) {
  # check if the names are appropriate

  good_names <- make.names(col_names, unique = TRUE)
  name_check <- names(df) == good_names

  if (any(name_check)) {
    bad_names <- col_names[!name_check]
    cat("(", paste(bad_names, collapse = ", "), ") are not valid R names\n", sep = "")
    return(FALSE)
  } else {
    return(TRUE)
  }
}
