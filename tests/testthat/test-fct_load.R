# tests for fct_load
#
# list of functions
#
# load_user_data - DONE
# merge_user_data - DONE
# load_rds_file - DONE
# load_csv - DONE
# load_excel - DONE
# is_valid_df - DONE
# is_df_continuous - DONE
# are_df_names_valid - DONE
# is_row_index_unique - DONE

# set up paths to test data
swiss_csv_path <- "../../example-data/swiss.csv"
swiss_xls_path <- "../../example-data/swiss.xls"
traff_xlsx_path <- "../../example-data/traffic.xlsx"
traff_csv_path <- "../../example-data/traffic.csv"
licen_rds_path <- "../../example-data/licenced_vehicles.rds"

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

# load_user_data
test_that("load_user_data works", {
  # test with 1 of each type known working
  # currently type xls not working
  uploads <-  data.frame(name = c("swiss.csv",
                                  "traffic.xlsx",
                                  "licenced_vehicles.rds"),
                       datapath = c(swiss_csv_path,
                                    traff_xlsx_path,
                                    licen_rds_path)
                       )
  expect_true(is.data.frame(load_user_data(uploads)))
  # check unnaccepted type is rejected
  expect_warning(
    load_user_data(data.frame(name = "test.pdf", datapath = "random")),
    regexp = "recognise")
})

# merge_user_data
test_that("merge_user_data produces dataframe",{
  # known working example
  res <- merge_user_data(list(df1 = good_df_1, df2 = good_df_2))
  expect_true(is.data.frame(res))
  # known failing example ? not working
  expect_error(merge_user_data(list(df1 = good_df_1, df2 = bad_df_1)))
})

# load_rds_file
test_that("load_rds_file", {
  # known good
  expect_true(
    is.data.frame(load_rds_file(licen_rds_path))
   )
  # known bad
  expect_error(load_rds_file(swiss_csv_path))
})

# load_csv
test_that("load_csv produces dataframe", {
  # known good
  expect_true(is.data.frame(load_csv(swiss_csv_path)))
  # known bad
  expect_error(load_csv(traff_xlsx_path))
})

# load_excel
test_that("load_excel produces dataframe",{
  # test xlsx file
  expect_true(is.data.frame(load_excel(traff_xlsx_path)))
  # test xls (97 - 2003)
  expect_true(is.data.frame(load_excel(swiss_xls_path)))
  # known bad
  expect_error(load_excel(traff_csv_path))
})

# is_valid_df
test_that("is_valid_df works",{
  # checks for a dataframe
  expect_error(is_valid_df(c(1, 2, 3)))
  # checks for "Year" in names()
  expect_error(is_valid_df(data.frame(Col1 = c(1, 2, 3))))
  # checks for "Quarter" in names
  expect_error(is_valid_df(data.frame(Year = c(2010, 2010), Col2 = c(1, 2))))
  # checks for unique indexes with is_row_index_unique - DONE BELOW
  # checks data are continuous with is_df_continuous - DONE BELOW
  # checks names are valid with are_df_names_valid - DONE BELOW
  # it accepts valid dataframe
  expect_true(is_valid_df(good_df_1))
  # rejects df with non numeric values
  expect_error(is_valid_df(bad_df_3))
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
  # ones with space
  expect_warning(are_df_names_valid(c("Col 1", "Col 2")), regexp = "Invalid")
  # non-unique
  expect_warning(are_df_names_valid(c("Col1", "Col1")), regexp = "Invalid")
  # TODO any more types of bad names to check?
})

# is_row_index_unique
test_that("is_row_index_unique works",{
  # test with known good df
  expect_true(is_row_index_unique(good_df_1))
  # test with known bad df
  expect_warning(is_row_index_unique(bad_df_2), regexp = "duplicate")
  # TODO do we want to test with something other than df?

})
