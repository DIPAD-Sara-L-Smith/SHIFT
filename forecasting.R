# Forecasting Models
library(forecast)
library(stats)

server <- function(input, output) {
  
  ## Naive ----
  output$fitNaive <- reactive({
    fit <- forecast::naive(tsDepVar())
    return(fit)
  })
  
  output$forecastNaive <- reactive({
    fit <- fitNaive()
    return(forecast(fit))
  })
  
  output$plotNaive <- renderPlot({
    fit <- fitNaive()
    return(plot(fit))
  })
  
  
  ## Time series decomposition ----
  output$fitDecomposition <- reactive({
    fit <- stats::stl(tsDepVar(), 
                      s.window = "period")
    return(fit)
  })
  
  output$forecastDecomposition <- reactive({
    fit <- fitDecomposition()
    return(forecast(fit))
  })
  
  output$plotDecomposition <- renderPlot({
    fit <- fitDecomposition()
    return(plot(forecast(fit)))
  })
  
  
  ## Holt-Winters ----
  output$fitHoltWinters <- reactive({
    fit <- stats::HoltWinters(tsDepVar())
    return(fit)
  })
  
  output$forecastHoltWinters <- reactive({
    fit <- fitHoltWinters()
    return(forecast(fit))
  })
  
  output$plotHoltWinters <- renderPlot({
    fit <- fitHoltWinters()
    return(plot(forecast(fit)))
  })
  
}
