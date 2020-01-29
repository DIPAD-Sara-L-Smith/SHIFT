
#' Load and convert user selected data
#'
#' @param upload a dataframe containing a details of uploaded files.
#'
#' @return a dataframe containing the combines files
#' @export
#'
load_user_data <-function(upload){

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
merge_user_data <- function(df_list, cols = c('Year', 'Quarter')){

  df <-
    reduce(
      df_list,
      function(...) {
        full_join(..., by = cols)
      }
    )

  return(df)
}


check_valid_df <- function(df){
  # TODO add inline docs
  # TODO copy previous code for checking validity
  return(valid)
}
