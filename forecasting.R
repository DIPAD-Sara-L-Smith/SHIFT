# Forecasting Models
library(forecast)
library(stats)

server <- function(input, output) {
  
  ## Naive ----
  output$fitNaive <- reactive({
    ts <- tsDepVar()
    fit <- forecast::snaive(tsDepVar, 
                            h = 12)
    return(fit)
  })
  
  output$forecastNaive <- reactive({
    fit <- fitNaive()
    return(forecast(fit, 12))
  })
  
  
  ## Time series decomposition ----
  output$fitDecomposition <- reactive({
    ts <- tsDepVar()
    fit <- stats::stl(tsDepVar, 
                      s.window = "period")
    return(fit)
  })
  
  output$forecastDecomposition <- reactive({
    fit <- fitDecomposition()
    return(forecast(fit, 12))
  })
  
  
  ## Holt-Winters ----
  output$fitHoltWinters <- reactive({
    ts <- tsDepVar()
    fit <- stats::HoltWinters(tsDepVar)
    return(fit)
  })
  
  output$forecastHoltWinters <- reactive({
    fit <- fitHoltWinters()
    return(forecast(fit, 12))
  })
  
}
