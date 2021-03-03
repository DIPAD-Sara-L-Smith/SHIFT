# tests for fct_model
#
# list of functions
#
# fit_models
# predict_models
# get_forecast_plotdata
# plot_forecast
# fit_holtwinters - DONE
# fit_decomp - DONE
# fit_naive - DONE
# fit_linear - DONE
# get_proj_data - DONE
# get_starting_values - UNUSED?

#load sample/test data

test_data <- readr::read_csv("../../example-data/swiss.csv")
cnames <- names(test_data)
start <- c(test_data[[1,"Year"]], test_data[[1,"Quarter"]])
# hotlwinters/naive needs complete years, so take 3 off row
last_row <- dim(test_data)[1] - 3
end <- c(test_data[[last_row,"Year"]], test_data[[last_row,"Quarter"]])

fc_names = c("method", "model", "lambda", "x", "fitted", "residuals",
             "series", "mean", "level", "lower", "upper")

# fit_models
# looks unused
# evidence - fit_linear has been updated to require model argument
# this code does provide that argument

# predict_models
test_that("predict_model predicts naive", {
  test_fit <- fit_naive(test_data, cnames[1], start, end)
  test_fc <- predict_models(list(test_fit))
  expect_equal(names(test_fc[[1]]), fc_names)
})


# plot_forecast
#   uses get_proj_data if linear model
#   uses get_forecast_plotdata all cases
#       uses predict_models
#
# needs data frame, dep_var, string vector of inv_vars,
#       start end dates c(YYYY, Q)
#       forecast_type
# returns a plot and need a way of testing for that
#
# use shiny test to test that forecasts get plotted

# fit_holtwinters
# needs complete seasons
test_that("fit_holtwinters produces HoltWinters object",{
  test_fit <- fit_holtwinters(test_data, cnames[1], start, end)
  expect_equal(class(test_fit), "HoltWinters")
})


# fit_decomp
test_that("fit_decomp produces stl object", {
  test_fit <- fit_decomp(test_data, cnames[1])
  expect_equal(class(test_fit), "stl")
})


# fit_naive
test_that("fit_naive produces forecast object", {
  test_fit <- fit_naive(test_data, cnames[1], start, end)
  expect_equal(class(test_fit), "forecast")
})

# fit_linear
test_that("fit_liner produces tslm / lm object", {
  model <- paste0(cnames[1], " ~ ", paste(cnames[2:4], collapse = " + "))
  test_fit <- fit_linear(test_data, cnames[1], cnames[2:4], start, end, model)
  expect_equal(class(test_fit),c("tslm", "lm"))
})

# get_proj_data
test_that("get_proj_data returns the right part of a dataframe", {
  #split the data roughly 75/25
  df_split_row <- (dim(test_data)[1]*0.75)%/%1
  proj_end <- c(test_data[[df_split_row,"Year"]],
                test_data[[df_split_row,"Quarter"]])
  expect_equal(get_proj_data(test_data, proj_end),
               test_data[(df_split_row + 1):(dim(test_data)[1]),])
})


# get_starting_values
# appears to be unused






