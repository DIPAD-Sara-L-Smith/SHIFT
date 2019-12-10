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
  
  output$plotNaive <- renderPlot({
    fit <- fitNaive()
    return(plot(fit))
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
    return(forecast(fit)[1:12])
  })
  
  output$plotDecomposition <- renderPlot({
    fit <- fitDecomposition()
    return(plot(fit))
  })
  
  
  ## Holt-Winters ----
  output$fitHoltWinters <- reactive({
    ts <- tsDepVar()
    fit <- stats::HoltWinters(tsDepVar)
    return(fit)
  })
  
  output$forecastHoltWinters <- reactive({
    fit <- fitHoltWinters()
    return(forecast(fit)[1:12])
  })
  
  output$plotHoltWinters <- renderPlot({
    fit <- fitHoltWinters()
    return(plot(fit))
  })
  
}
