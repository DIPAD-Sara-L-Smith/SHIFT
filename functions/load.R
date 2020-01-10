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
  
  # Headings conform to standards
  
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
  }
  
  return(continuous)
}