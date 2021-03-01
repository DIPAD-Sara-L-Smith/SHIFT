# tests for fct_xts
#
# list of functions
#
# df_to_ts
# xts_to_df
# diff_df
# diff_inv_df
#
# notes from file
#
# Here is an idea for a test for these functions for when we
# get round to adding the tests.
# > identical(df, xts_to_df(df_to_xts(df)))
# [1] TRUE

# good dfs have "Year" and "Quarter" columns with unique combinations
good_df_1 <- data.frame(Year    = c(2010, 2010, 2010, 2010),
                        Quarter = c(1, 2, 3, 4),
                        Val1    = c(1, 2, 3, 4))
good_df_2 <- data.frame(Year    = c(2010, 2010, 2010, 2010),
                        Quarter = c(1, 2, 3, 4),
                        Val2    = c(4, 3, 2, 1))

# df does not contain "Year" or "Quarter" columns
bad_df_1 <- data.frame(Col1     = c(2010, 2010, 2010, 2010),
                       Col2     = c(1, 2, 3, 4),
                       Val2     = c(4, 3, 2, 1))

# df contains duplicate "Year" / "Quarter" combinations
bad_df_2 <- data.frame(Year     = c(2010, 2010, 2010, 2010),
                       Quarter  = c(1, 2, 1, 1),
                       Value    = c(4, 3, 2, 1))

# df values are string
bad_df_3 <- data.frame(Year    = c(2010, 2010, 2010, 2010),
                       Quarter = c(1, 2, 3, 4),
                       Val2    = c("str1", "str2", "str3", "str4"))

# diff_df

test_that("diff_df works",{
  # test it rejects an invalid data frame (is_df_valid)
  # error comes from is_df_valid not from this function
  expect_warning(diff_df(bad_df_1), regexp = "not valid")

  # test with string values - currently result in col of NA


})

test_that("diff_df throws error for str values", {
  expect_error(diff_df(bad_df_3))
})



# diff_inv_df - undo differencing
# 2 dfs, both with year/quarter cols and data
