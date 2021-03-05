# tests for fct_xts
#
# list of functions
#
# df_to_ts - DONE
# xts_to_df - DONE
# diff_df - DONE
# diff_inv_df - UNUSED
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

# diffed df  removes the first value
good_df_1_diff <- data.frame(Year    = c(2010, 2010, 2010),
                             Quarter = c(1, 2, 3),
                             Val1    = c(1, 1, 1))

# starting values for diff_inv_df
startingvalues <- data.frame(Year    = 2020,
                             Quarter = 1,
                             Val1    = 1)

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
test_that("diff_df rejects invalid data frame",{
  # test it rejects an invalid data frame (is_df_valid)
  # error comes from is_df_valid not from this function
  expect_warning(diff_df(bad_df_1), regexp = "not valid")
})

test_that("diff_df throws error for str values", {
  expect_error(diff_df(bad_df_3))
})

test_that("diff_df produces a diffed data frame", {
  expect_equal(diff_df(good_df_1), good_df_1_diff)
})

# xts_to_df and df_to_xts test as suggested
test_that("xts_to_df and df_to_xts convert properly",{
  expect_equal(good_df_1, xts_to_df(df_to_xts(good_df_1)))
})

# no need to test diff_inv_df as it appears to be unused
# diff_inv_df - undo differencing
# 2 dfs, both with year/quarter cols and data
