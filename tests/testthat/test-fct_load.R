# tests for fct_load
#
# list of functions
#
# load_user_data
# merge_user_data
# load_rds_file
# load_csv
# load_excel
# is_valid_df
# is_df_continuous - DONE
# are_df_names_valid - DONE
# is_row_index_unique - DONE

# is_valid_df
test_that("is_valid_df works",{
  # checks it is a DF
  expect_error(is_valid_df(c(1, 2, 3)))
  # checks for "Year" in names()
  expect_error(is_valid_df(data.frame(Col1 = c(1, 2, 3))))
  # checks for "Quarter" in names
  expect_error(is_valid_df(data.frame(Year = c(2010, 2010), Col2 = c(1, 2))))
  # checks for unique indexes with is_row_index_unique - DONE
  # checks data are continuous with is_df_continuous - DONE
  # checks names are valid with are_df_names_valid - DONE
  # is accepts valid dataframe
  expect_true(is_valid_df(data.frame(Year = c(2010, 2010,
                                              2010, 2010),
                                     Quarter = c(1, 2, 3, 4),
                                     Values = c(1, 2, 3, 4))))
})


# is_df_continuous
test_that("is_df_continuous works", {
  # test with no missing values
  expect_true(is_df_continuous(data.frame(Col1 = c(1, 2, 3, 4))))
  # test with missing value NA
  expect_false(is_df_continuous(data.frame(Col1 = c(1, 2, NA, 4))))
  # test with no value ""
  expect_false(is_df_continuous(data.frame(Col1 = c(1, 2, "", 4))))
})



# are_df_names_valid
test_that("are_df_names_valid works", {
  # test with good names
  expect_true(are_df_names_valid(c("Col1", "Col2", "Col3")))
  # test with bad names
  warning <- "Invalid names found in dataframe header."
  # what versions of bad names?
  # ones with space
  expect_warning(are_df_names_valid(c("Col 1", "Col 2")),
                 warning)
  # non-unique
  expect_warning(are_df_names_valid(c("Col1", "Col1")),
                 warning)
})


# is_row_index_unique

test_that("is_row_index_unique function works",{
  # test with know good df
  expect_true(is_row_index_unique(data.frame(Year = c(2010, 2010,
                                                      2010, 2010),
                                             Quarter = c(1, 2, 3, 4))))
  # test with known bad df
  bad_df <- data.frame(Year = c(2010, 2010, 2010, 2010),
                       Quarter = c(1, 2, 1, 2))
  expect_warning(is_row_index_unique(data.frame(Year = c(2010, 2010,
                                                         2010, 2010),
                                                Quarter = c(1, 2, 1, 2))),
                 "Found duplicate dates.")

  # TODO do we want to test with something other than df?

})
