
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
#' @param data_list a list of valid dataframes
#'
#' @return A merged data frame
#' @export
#'
#' @examples
merge_user_data <- function(data_list){
  # TODO look into using purrr:reduce() to merge an unknown number of dfs.
  return(merged_data)
}


check_valid_df <- function(df){
  # TODO add inline docs
  # TODO copy previous code for checking validity
  return(valid)
}
