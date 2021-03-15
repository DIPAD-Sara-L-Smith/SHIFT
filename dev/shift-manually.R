upload = data.frame(name = "validation-data.csv", datapath = "./example-data/validation-data.csv")
user_data <- load_user_data(upload)
start <- with(user_data, c(Year[1], Quarter[1]))
end <- with(user_data, c(Year[67], Quarter[67]))
proj_data <- get_proj_data(user_data, end)
model <- paste0("Car.Demand", " ~ ", paste(names(user_data)[5:9], collapse = " + "))
fit <- fit_linear(user_data, dep_var = "Car.Demand",
          ind_var = names(user_data)[5:9],
          start, end, model )

data <- get_forecast_plotdata(fit, proj_data = proj_data)
data

fc <- forecast(fit, newdata = proj_data)

test <- cbind(Forecast = fc$mean, Hi_95 = fc$upper[,2], Lo_95 = fc$lower[,2])

