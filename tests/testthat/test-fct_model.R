# tests for fct_model
#
# list of functions
#
# fit_models - UNUSED?
# predict_models - DONE
# TODO get_forecast_plotdata
# plot_forecast -TEST WITH SHINYTEST
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
test_ts_lm <- ts(test_data, frequency = 4, start = start)
test_ts <-  ts(test_data[cnames[1]], frequency = 4, start = start, end = end)
model <- paste0(cnames[1], " ~ ", paste(cnames[2:4], collapse = " + "))

comp_fit_hw <- stats::HoltWinters(test_ts)
comp_fit_dc <- stats::stl(test_ts, s.window = "period")
comp_fit_na <- forecast::naive(test_ts)
comp_fit_lm <- forecast::tslm(formula = as.formula(paste0(model," + trend + season", collapse = "")), data = test_ts_lm)


# fit_models
# looks unused
# evidence - fit_linear has been updated to require model argument
# this code does provide that argument

# predict_models - holtwinters
test_that("predict_models predicts holtwinters",{
  test_fc <- predict_models(list(comp_fit_hw))
  comp_fc <- forecast(comp_fit_hw, h = 4)
  expect_equal(test_fc[[1]], comp_fc)
})

# predict_models - decomp
test_that("predict_models predicts stl / decomp", {
  test_fc <- predict_models(list(comp_fit_dc))
  comp_fc <- forecast(comp_fit_dc, h = 4)
  expect_equal(test_fc[[1]], comp_fc)
})

# predict_models - naive
test_that("predict_model predicts naive", {
  comp_fc <- forecast(comp_fit_na, h = 4)
  test_fc <- predict_models(list(comp_fit_na))
  expect_equal(test_fc[[1]], comp_fc)
})

# predict_models - linear
test_that("predict_models predicts linear", {
  test_proj_data <- test_data[5:8,]
  comp_fc <- forecast(comp_fit_lm, newdata = test_proj_data)
  test_fc <- predict_models(list(comp_fit_lm), proj_data = test_proj_data)
  test_fc <- test_fc[[1]]
  expect_equal(test_fc$model$coefficients, comp_fc$model$coefficients)
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
  expect_equal(test_fit$coefficients, comp_fit_hw$coefficients)
})


# fit_decomp
test_that("fit_decomp produces stl object", {
  test_fit <- fit_decomp(test_data, cnames[1])
  expect_equal(test_fit$coefficients, comp_fit_dc$coefficients)
})


# fit_naive
test_that("fit_naive produces forecast object", {
  test_fit <- fit_naive(test_data, cnames[1], start, end)
  expect_equal(test_fit$fitted, comp_fit_na$fitted)
})

# fit_linear
test_that("fit_liner produces tslm / lm object", {
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






