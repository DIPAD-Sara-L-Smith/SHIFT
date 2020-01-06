#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(forecast)
# library(plotly)
library(shinyWidgets)
library(dygraphs)
library(xts)
library(zoo)

# Source additional scripts
#source("forecasting.R")

# User interface -----
ui <- dashboardPage(
   
  ## Title & sidebar -----
   # Application title
   dashboardHeader(title = "Forecasting Tool"),
   
   # Sidebar with a slider input for number of bins 
   dashboardSidebar(
     sidebarMenu(
       menuItem("Explore data", 
                tabName = "explore", 
                icon = icon("table")),
       menuItem("Compare forecasts", 
                tabName = "forecasts", 
                icon = icon("line-chart")),
       menuItem("Review regression models", 
                tabName = "review-regression", 
                icon = icon("list-ol"))
    )
  ), 
     
  dashboardBody(
    # Custom CSS formatting -----
    # Also add some custom CSS to make the title background area the same
    # color as the rest of the header.
    tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #006c56;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #006c56;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #006c56;
                              }       
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #006c56;
                              }

                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #006c56;
                              }

                              /* primary status box */
                              .box.box-solid.box-primary>.box-header {
                                color:#fff;
                                background:#006c56
                              }
                              
                              .box.box-solid.box-primary {
                                border-bottom-color:#006c56;
                                border-left-color:#006c56;
                                border-right-color:#006c56;
                                border-top-color:#006c56;
                              }

                              '))), 
    
    tabItems(
    
      ## Explore data tab -----
      tabItem(tabName = "explore",
        fluidRow(
          box(width = 12,
            h4("Use this tab to explore a dataset of your choice.")
          )
        ),
        
        fluidRow(
          box(width = 12,
              solidHeader = TRUE,
              status = "primary",
              title = "Choose your dataset",
              p("Select a data file (.csv or .xslx) or an R script that 
                outputs one of these files."),
              fileInput("DataFilePath",
                        " ",
                        multiple = FALSE,
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                          ".csv",
                          ".xlsx",
                          ".R"
                        ),
                        width = "400px",
                        buttonLabel = "Find file",
                        placeholder = "Data file path"),
              
              #uiOutput("DataType"),
              
              uiOutput("DepVar"), 
              uiOutput("YearVar"),
              uiOutput("PeriodVar"),
              uiOutput("RangeHistorical"),
              uiOutput("RangeProjections"),
              # uiOutput("YearStart"),
              # uiOutput("PeriodStart"),
              
              actionButton("button", "Refresh data")
              #)
          )
        ),

        fluidRow(
          box(width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE, 
            title = "Here's a snapshot of your data",
            verbatimTextOutput("DataHead")
          ),
  
          box(width = 12,
            solidHeader = TRUE,
            status = "primary",
            title = "What does your dependent variable look like?",
            dygraphOutput("DepVarPlot", height = 500)
          ),
  
          box(width = 12,
            solidHeader = TRUE,
            status = "primary",
            title = "Plots of all variables",
            plotOutput("tsPlots", height = 1000)
          )
        )
        
      ), #tabItem end
      
      # Compare forecasts -----
      tabItem(tabName = "forecasts",
        # tab_forecasts_ui("tab_forecasts_server")
        
        fluidRow(
          box(width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary",
              dygraphOutput("plotForecasts", height = 500),
              p(""),
              p("Use the button below to download these forecasts in a csv 
                file."),
              downloadButton("downloadData", "Download forecast data")
          )
        ),
        
        fluidRow(
          box(width = 12,
              title = "Naive",
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary", 
              # verbatimTextOutput("fitNaive"), 
              plotOutput("plotNaive", height = 500)
          )
        ),
        
        fluidRow(
          box(width = 12,
              title = "Holt-Winters",
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary", 
              # verbatimTextOutput("fitHoltWinters"), 
              plotOutput("plotHoltWinters", height = 500)
          )
        ),
        
        fluidRow(
          box(width = 12,
              title = "Time Series Decomposition",
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary", 
              # verbatimTextOutput("fitDecomposition"), 
              plotOutput("plotDecomposition", height = 500),
              p("The chart below shows the components of the time series 
                decomposition"),
              plotOutput("plotDecompositionComponents", height = 500)
          )
        ),
        
        fluidRow(
          box(width = 12,
              title = "Linear regression",
              solidHeader = TRUE,
              verbatimTextOutput("summaryLinearRegression"),
              status = "primary", 
              collapsible = TRUE, 
              plotOutput("plotLinearRegression", height = 500)
          )
        )
        
      ),
      
      # Review regression models -----
      tabItem(tabName = "review-regression",
        fluidRow(
          p("To be added...")
        )
      )
    )
  )
)

# Server -----
server <- function(input, output, session) {
  
  ## Read in data -----
  
  # Read in dataset
  FileData <- reactive({
    infile <- input$DataFilePath$name
    
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    } else if (tolower(tools::file_ext(infile)) == "csv") {
      # Return data
      FileData <- readr::read_csv(infile)
      return(FileData)
    } else if (tolower(tools::file_ext(infile)) == "r") {
      # Run R script that returns data
      source(infile, local = TRUE)
      
      if (exists("FileData")) {
        if (is.data.frame(FileData)) {
          return(FileData)
        } else {
          warning("The selected R script didn't produce a data frame called 
                  FileData. Please adjust your R script.")
          return(NULL) 
        }
      } else {
        warning("You selected an R script, but no variable named FileData was 
                produced as a result - please adjust your script to do so.")
        return(NULL)
      }
    }
    
  })
  
  # Setup reactive values for data
  # These variables contain time series objects, ensuring that time axes, etc.
  # are all correctly labelled.
  v <- reactiveValues(data = NULL)
  
  # Function refresh time series object
  observeEvent(input$button, {
    # Read in data
    df <- FileData()
    
    # Setup inputs
    if (is.null(input$PeriodVar)) {
      intFrequency <- 1
    } else {
      intFrequency <- max(df[, input$PeriodVar])
    }

    if (is.null(input$rngHistoricalData)) {
      intStartYear <- 1
      intStartPeriod <- 1
    } else {
      StartDate <- lubridate::yq(input$rngHistoricalData[1])
      intStartYear <- as.integer(lubridate::year(StartDate))
      intStartPeriod <- as.integer(lubridate::quarter(StartDate, 
                                                      with_year = FALSE))
    }
    
    # Setup variables for just dependent variable and all data
    # All data
    v$dataAll <- getTimeSeries(df,
                               intFreq = intFrequency,
                               intStartYear = intStartYear,
                               intStartPeriod = intStartPeriod)
    
    
    # Historical data only
    # read in the bounds for historical data (from user input)
    req(input$rngHistoricalData)
    dateStart <- lubridate::yq(input$rngHistoricalData[1])
    dateEnd <- lubridate::yq(input$rngHistoricalData[2])
    
    # subset data
    v$dataHist <- stats::window(
      v$dataAll, 
      start = c(lubridate::year(dateStart), 
                lubridate::quarter(dateStart, with_year = FALSE)),
      end = c(lubridate::year(dateEnd), 
              lubridate::quarter(dateEnd, with_year = FALSE))
    )
    v$dataHist <- na.trim(v$dataHist)
    
    # Dependent variable only
    v$dataDepVar <- v$dataHist[, input$DepVar]
    
    
    # Projection data only
    # read in bounds
    req(input$rngProjectionData)
    dateStart <- lubridate::yq(input$rngProjectionData[1])
    dateEnd <- lubridate::yq(input$rngProjectionData[2])
    
    # subset data
    v$dataProj <- stats::window(
      v$dataAll,
      start = c(lubridate::year(dateStart),
                lubridate::quarter(dateStart, with_year = FALSE)),
      end = c(lubridate::year(dateEnd),
              lubridate::quarter(dateEnd, with_year = FALSE))
    )
    
    if (nrow(v$dataProj) > 1) {
      v$dataProj <- v$dataProj[, !(colnames(v$dataProj) %in% c(input$DepVar))]
      v$dataProj <- na.trim(v$dataProj)
    }
    
    # Number of periods to forecast
    v$NPeriodsToForecast <- nrow(v$dataProj)
        
  })

  
  
  ## Functions
  
  # Function to return time series object
  getTimeSeries <- function(df, intFreq, intStartYear, intStartPeriod) {
    # Setup missing values
    if (missing(intFreq)) {
      intFreq <- 1
    }
    
    if (missing(intStartYear)) {
      intStartYear = 1
    }
    
    if (missing(intStartPeriod)) {
      intStartPeriod = 1
    }
    
    if (is.null(input$DataFilePath)) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      
      # find start point
      if (is.null(intStartYear)) {
        # warning("No integer year variable selected - time series may not display 
        #         correctly. Number of periods set by default to 1.")
        getTimeSeries <- stats::ts(df, frequency = 1)
      } else {
        if (is.null(intStartPeriod)) {
          # warning("No integer period variable selected - time series may not 
          #         display correctly. Number of periods set by default to 1.")
          getTimeSeries <- stats::ts(df, frequency = 1,
                           start = c(intStartYear, 1))
        } else {
          strStart <- c(intStartYear, intStartPeriod)
          getTimeSeries <- stats::ts(df, 
                                     frequency = intFreq,
                                     start = strStart)
          
        }
      }
      
      return(getTimeSeries)
    }
  }
  
  # Get list of variable names in dataset
  DataVarNames <- reactive({
    df <- FileData()
    if (is.null(df)) {
      return(NULL)
    } else {
      return(names(df)) 
    }
  })
  
  
  ## Setup dynamic inputs ----
  
  output$DataType <- renderUI({
    radioButtons("DataType", 
                 label = "What type of data is this?", 
                 choices = c("Frequency",
                             "Time series"),
                 selected = "Time series")
  })
  
  output$DepVar <- renderUI({
    df <- FileData()
    if (is.null(df)) {
      items <- NULL
    } else {
      items <- names(df)
      names(items) <- items      
    }
    
    selectInput("DepVar", 
                "Now choose your dependent variable:",
                items,
                selected = items[3])
    
  })
  
  getTimeValue <- function(intYear, intPeriod) {
    if (missing(intPeriod)) {
      intPeriod <- 0
    }
    
    TimeValue <- intYear + (intPeriod / 4)
    return(TimeValue)
  }
  
  output$RangeHistorical <- renderUI({
    req(input$DataFilePath, input$YearVar, input$PeriodVar)
    df <- FileData()
    
    # create the sequence of Date objects
    dateList <- seq(lubridate::yq(paste0(df[1, input$YearVar], 
                                         ": Q", 
                                         df[1, input$PeriodVar])),
                    to = lubridate::yq(paste0(df[nrow(df), input$YearVar], 
                                              ": Q", 
                                              df[nrow(df), input$PeriodVar])), 
                    by = "quarter")
        
    # format vector
    dateListFormatted <- zoo::as.yearqtr(dateList)
    
    # find default end for historical data 
    # (based on when dependent variable ends)
    if (is.null(input$dataFilePath)) {
      defaultEnd <- dateListFormatted[length(dateListFormatted)]
    } else {
      # find the last data point for the selected dependent variable
      dataDepVar <- df %>%
        select(input$YearVar, input$PeriodVar, input$DepVar) %>%
      dataDepvar <- na.trim(dataDepVar)
      dataDepVar <- dataDepVar[nrow(dataDepVar), ]
      
      defaultEnd <- c(dataDepVar[, input$YearVar],
                      dataDepVar[, input$Periodvar])
    }
    
    # put together widget
    sliderTextInput(
      inputId = "rngHistoricalData", 
      label = "Select the start and end points for the historical data", 
      grid = TRUE, 
      force_edges = TRUE,
      choices = dateListFormatted,
      selected = c(dateListFormatted[1], 
                   defaultEnd)
    )
  })
  
  output$RangeProjections <- renderUI({
    req(input$DataFilePath, input$YearVar, input$PeriodVar,
        input$rngHistoricalData)
    df <- FileData()
    # browser()
    
    # create the sequence of Date objects
    dateList <- seq(lubridate::yq(paste0(df[1, input$YearVar], 
                                         ": Q", 
                                         df[1, input$PeriodVar])),
                    to = lubridate::yq(paste0(df[nrow(df), input$YearVar], 
                                              ": Q", 
                                              df[nrow(df), input$PeriodVar])), 
                    by = "quarter")
    
    # format vector
    dateListFormatted <- zoo::as.yearqtr(dateList)
    
    # find default end for historical data 
    # (based on when dependent variable ends)
    if (is.null(input$DepVar)) {
      defaultStart <- dateListFormatted[length(dateListFormatted)]
    } else {
      if (is.null(input$rngHistoricalData)) {
        # strEndOfHistData <- lubridate::yq("2019: Q1")
        # defaultStart <- zoo::as.yearqtr(strEndOfHistData)
      } else {
        strEndOfHistData <- input$rngHistoricalData[2]
        defaultStart <- zoo::as.yearqtr(strEndOfHistData)
      }
    }
    
    # ensure defaultEnd doesn't exceed slider limits
    defaultEnd <- lubridate::yq(defaultStart) + lubridate::years(5)
    if ((defaultEnd -  
         lubridate::yq(dateListFormatted[length(dateListFormatted)])) > 0) {
      defaultEnd <- lubridate::yq(dateListFormatted[length(dateListFormatted)])
    }
    defaultEnd <- zoo::as.yearqtr(defaultEnd)
    
    # put together widget
    sliderTextInput(
      inputId = "rngProjectionData", 
      label = "Select the end point for the projections.", 
      grid = TRUE, 
      force_edges = TRUE,
      choices = dateListFormatted,
      selected = c(defaultStart, 
                   defaultEnd),
      from_fixed = TRUE
    )
  })
  
  output$YearVar <- renderUI({
    df <- FileData()
    if (is.null(df)) {
      items <- NULL
    } else {
      items <- names(df)
      names(items) <- items
    }
    
    selectInput("YearVar", 
                "Which variable contains the time period (e.g. year)?",
                items,
                selected = items[1])
    
  })
  
  output$PeriodVar <- renderUI({
    shiny::req(input$DataFilePath)
    
    if (is.null(input$DataFilePath)) {
      items <- NULL
      itemToSelect <- NULL
    } else {
      df <- FileData()
      items <- names(df)
      names(items) <- items
      itemToSelect <- items[2]
    }
    
    # setup input
    selectInput("PeriodVar", 
                "Which variable contains the period (e.g. quarter)?",
                items,
                selected = itemToSelect)
    
  })
  
  output$YearStart <- renderUI({
    shiny::req(input$DataFilePath)
    
    if (is.null(input$DataFilePath)) {
      items <- NULL
    } else {
      if (is.null(input$YearVar)) {
        items <- NULL
      } else {
        shiny::req(input$YearVar)
        df <- FileData()
        items <- unique(df[, input$YearVar])
        names(items) <- items
      }
    }
    
    # setup input
    selectInput("YearStart",
                "What's the first year in the data?",
                items)
  })
  
  output$PeriodStart <- renderUI({
    shiny::req(input$DataFilePath)
    if (is.null(input$DataFilePath)) {
      items <- NULL
    } else {
      if (is.null(input$PeriodVar)) {
        items <- NULL
      } else {
        shiny::req(input$PeriodVar)
        df <- FileData()
        items <- unique(df[, input$PeriodVar])
        names(items) <- items
      }
    }
    
    # setup input
    selectInput("PeriodStart",
                "What's the first period in the data?",
                items)
  })
  
  # Generate textual info about data for display in app
  output$DataInfo <- renderPrint({
    if(is.null(input$DataFilePath)) {
      textVarNames <- "No data file has been selected."
      textVarNames
    } else {
      # infile <- input$DataFilePath
      DataVarNames()
    }
  })
  
  # Generate snapshot of data
  output$DataHead <- renderPrint({
    if(is.null(input$DataFilePath)) {
      return("No data file has been selected.")
    } else {
      head(FileData())
    }
  })
  
  
  ## Download forecast data ----
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    req(input$DataFilePath, v$dataDepVar)
    
    # get forecasts
    hist <- v$dataDepVar
    naive <- forecastNaive()
    decomposition <- forecastDecomposition()
    holtwinters <- forecastHoltWinters()
    regression <- forecastLinearRegression()
    
    # put together plot
    plotData <- ts.union(
      hist, 
      naive$mean, 
      decomposition$mean, 
      holtwinters$mean,
      regression$mean
    )
    
    # rename variables for readability
    colnames(plotData) <- c("Historical data",
                            "Naive forecast",
                            "Time Series Decomposition forecast", 
                            "Holt-Winters forecast",
                            "Linear regression forecast")
    
    return(plotData)
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$DepVar, "_and_forecasts.csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
  ## Plots ----
  
  # Generate plot of dependent variable
  output$DepVarPlot <- renderDygraph({
    if (is.null(input$DataFilePath) | is.null(v$dataDepVar)) {
      # No data file has been selected.
      return(NULL)
    } else {
      if(is.null(input$DepVar)) {
        #  No dependent variable has been selected.
        return(NULL)
      } else {
        # if (input$DataType == "Time series") {
          if (is.null(input$YearVar)) {
            # No time period has been selected.
            return(NULL)
          } else {
            # Read in and convert ts to xts object to avoid error
            plotData <- xts::as.xts(v$dataDepVar)
            
            p <- dygraphs::dygraph(plotData) %>%
              dyRangeSelector(height = 40) 
            return(p)
          }
      }
    }
  })
  
  # Generate plots of all variables
  output$tsPlots <- renderPlot({
    df <- v$dataAll

    if(is.null(df)) {
      # No data file has been selected.
      return(NULL)
    } else {
      if (nrow(df) == 0) {
        return(NULL)
      } else {
        return(plot(df))
      }
    }
  })
  
  # Load tab modules
  # callModule(tab_forecasts_server, "tab_forecasts_server", tsDepVar, tsFileData)
  
  ## Naive ----
  fitNaive <- reactive({
    fit <- forecast::naive(v$dataDepVar, h = v$NPeriodsToForecast)
    return(fit)
  })
  
  forecastNaive <- reactive({
    fit <- fitNaive()
    return(forecast::forecast(fit, h = v$NPeriodsToForecast))
  })
  
  output$plotNaive <- renderPlot({
    fit <- forecast::naive(v$dataDepVar, h = v$NPeriodsToForecast)
    return(plot(fit))
  })
  
  
  ## Time series decomposition ----
  forecastDecomposition <- reactive({
    fit <- stats::stl(v$dataDepVar, s.window = "period")
    return(forecast::forecast(fit, h = v$NPeriodsToForecast))
  })
  
  output$plotDecomposition <- renderPlot({
    fit <- stats::stl(v$dataDepVar, s.window = "period")
    return(plot(forecast::forecast(fit, h = v$NPeriodsToForecast)))
  })
  
  output$plotDecompositionComponents <- renderPlot({
    df <- v$dataDepVar
    fit <- stats::decompose(df, type = "additive")
    return(plot(fit))
  })
  
  
  ## Holt-Winters ----
  fitHoltWinters <- reactive({
    fit <- stats::HoltWinters(v$dataDepVar)
    return(fit)
  })
  
  forecastHoltWinters <- reactive({
    fit <- fitHoltWinters()
    return(forecast::forecast(fit))#, ts = TRUE))
  })
  
  output$plotHoltWinters <- renderPlot({
    fit <- stats::HoltWinters(v$dataDepVar)
    return(plot(forecast::forecast(fit, h = v$NPeriodsToForecast)))
  })
  

  ## Linear regression ----
  fitLinearRegression <- reactive({
    req(input$DepVar, input$YearVar, input$PeriodVar)
    #browser()
    # get data and remove time variable
    fitData <- v$dataHist
    
    # find variables to include (including seasonal dummy variables)
    varsToInclude <- colnames(fitData)[which(!(colnames(fitData) %in% c(input$DepVar,
                                                                        input$YearVar,
                                                                        input$PeriodVar)))]
    varsToInclude <- c(varsToInclude, "season")
    
    # setup formula for linear model
    strFormula <- stats::reformulate(varsToInclude, response = input$DepVar)
    
    # build lm model
    fit <- forecast::tslm(formula = strFormula, data = fitData)
    return(fit)
  }) 
  
  output$summaryLinearRegression <- renderPrint({
    # build lm model
    fit <- fitLinearRegression()
    return(summary(fit))
  })
  
  forecastLinearRegression <- reactive({
    # fit model
    fit <- fitLinearRegression()
    
    # forecast forward
    fcast <- forecast(
      object = fit,
      newdata = as.data.frame(v$dataProj),
      level = c(80, 95),
      fan = TRUE
    )
    
    return(fcast)
  })

  output$plotLinearRegression <- renderPlot({
    return(plot(forecastLinearRegression()))
  })

  
  # Generate plot showing all forecasts
  output$plotForecasts <- renderDygraph({
    # get data
    plotData <- datasetInput()
    
    # build plot
    p <- dygraph(plotData,
                 main = "Comparison of forecasts"
                 ) %>%
      dyRangeSelector(height = 40) %>%
      dySeries(colnames(plotData)[1], drawPoints = TRUE, color = "black") %>%
      dySeries(colnames(plotData)[2], drawPoints = TRUE, color = "grey") %>%
      dySeries(colnames(plotData)[3], drawPoints = TRUE, color = "green") %>%
      dySeries(colnames(plotData)[4], drawPoints = TRUE, color = "purple") %>%
      dySeries(colnames(plotData)[5], drawPoints = TRUE, color = "blue")
    return(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
#shiny::runApp(display.mode="showcase")

