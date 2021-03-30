# make upload data frame
upload <-  data.frame(name = "validation-data.csv", datapath = "./example-data/validation-data.csv")

#load data
user_data <- load_user_data(upload)

#set a start and end date
start <- with(user_data, c(Year[1], Quarter[1]))
end <- with(user_data, c(Year[75], Quarter[75]))

#calulate projected data

proj_data <- get_proj_data(user_data, end)
# or!
df_sorted <- user_data %>%
  dplyr::select(everything()) %>%
  tibble::rownames_to_column() %>%
  dplyr::arrange(Year, Quarter)

df_row_number <- df_sorted %>%
  dplyr::filter(Year == end[1] & Quarter == end[2])

df_row_number <- as.numeric(df_row_number$rowname) + 1

proj_data <- df_sorted %>%
  dplyr::slice(df_row_number:dplyr::n()) %>%
  select(-one_of("rowname"))


# model into a variable
model <- paste0("Car.Demand", " ~ ", paste(names(user_data)[5:9], collapse = " + "))

# drop NAs in dep_var else it will break the fit
subset_data <- user_data %>% drop_na()

#fit the model
fit <- fit_linear(subset_data, dep_var = "Car.Demand",
          ind_var = names(subset_data)[5:9],
          start, end, model )





#get projected data to forecast against
data <- get_forecast_plotdata(fit, proj_data = proj_data)
data

#do forecast
fc <- forecast(fit, newdata = proj_data)

# make table of forecasts
test <- cbind(Forecast = fc$mean, Hi_95 = fc$upper[,2], Lo_95 = fc$lower[,2])
