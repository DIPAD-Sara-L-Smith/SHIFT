# A collection of functions used for loading dataframes and preparing them for use.

is_valid_df <- function(df){
  # Determines whether a dataframe is valid to be used for forecasting
  
  # Has Years
  assertthat::assert_that('Year' %in% names(df), 
                          msg = "Dataframe does not contain a column called Year")
  
  # Has Quarters
  assertthat::assert_that('Quarter' %in% names(df), 
                          msg = "Dataframe does not contain a column called Quarter")
  
  # Data is continuous 
  assertthat::assert_that(all(unlist(df %>% 
                                       select(-c('Year', 'Quarter')) %>% 
                                       map(~is_continuous(.)))),
                          msg = "Data is not continuous")
  
  # Some of the fit functions don't play nicely with non-standard column names, i.e., spaces
  # Check by comparing to the function which builds nice names 
  assertthat::assert_that(any(names(df) == make.names(names(df), unique = TRUE)),
                          msg = paste("The column headings contain some bad characters",
                                      "probably a space. Fix in the data or consider adding", 
                                      "make.names(names(df)) to your data-prep script"))
  
  cat('Loaded a valid dataframe\n')
  return(TRUE)
}


merge_df <- function(df_left, df_right){
  # Merge two valid dataframes
  df = full_join(left, right, by=c('Year', 'Quarter'))
  
  # check the new dataframe is valid
  if (is_valid_df(df)){
    cat('Merged two dataframes.\n')
    return(df)
  } else {
    cat('Dataframes could not be merged.\n')
    return(left)
  }
}

is_continuous <- function(df){
  # TODO investigate using zoo:trim_na to make this function a bit clearer.
  # Checks whether a dataset column is continuous with no missing values. 
  # NA are allowed at the begining and end as datasets have different history lengths
  
  # Find the index of all missing values and take the diff
  index_diff <- diff(which(!is.na(df)))
  
  # NAs are allowed at beginning and end so drop those
  index_diff <- index_diff[2:length(index_diff)]
  
  # If the the data is continuous the diff indexes will always = 1
  continuous = all(index_diff == 1)
  
  if (!continuous) {
    cat('Column: ',names(df), ' is continuous, it has missing values in the middle\n')
    cat('Consider using dplyr::fill or fill_interp in your data prep script.\n')
  }
  
  return(continuous)
}


are_names_valid <- function(col_names){
  # check if the names are appropriate
  
  good_names = make.names(col_names, unique=TRUE)
  name_check = names(df) == good_names
  
  if (any(name_check)) {
    bad_names = col_names[!name_check]
    cat('(',paste(bad_names, collapse=", "), ') are not valid R names\n', sep = "")
    return(FALSE)
  } else {
    return(TRUE)
  }

}