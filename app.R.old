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
library(plotly)
library(dygraphs)

# Source additional scripts
#source("forecasting.R")

# Define user interface
ui <- dashboardPage(
   
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
    tabItems(
    
      # Explore data tab -----
      tabItem(tabName = "explore",
        fluidRow(
          box(width = 12,
            h4("Explore a dataset of your choice.")
          )
        ),
        
        fluidRow(
          box(width = 6,
              solidHeader = TRUE,
              status = "primary",
              title = "Choose your dataset", 
              fileInput("DataFilePath",
                        " ",
                        multiple = FALSE,
                        accept = NULL,
                        width = "400px",
                        buttonLabel = "Find file",
                        placeholder = "Data file path")
          ),
          
          box(width = 6,
              solidHeader = TRUE,
              status = "primary",
              title = "Setup data", 
              
              #uiOutput("DataType"),
              
              uiOutput("DepVar"), 
              
              #conditionalPanel(
                #condition = "input.DataType == 'Time series'",
                uiOutput("YearVar"),
                uiOutput("PeriodVar"),
                uiOutput("YearStart"),
                uiOutput("PeriodStart")
              #)
          )
        ),

        fluidRow(
          box(width = 12,
            solidHeader = TRUE,
            status = "primary",
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
              h4("Compare forecasts"),
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary",
              dygraphOutput("plotForecasts", height = 500)
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
              verbatimTextOutput("fitLinearRegression"),
              status = "primary", 
              collapsible = TRUE#, 
              # plotOutput("plotLinearRegression", height = 500)
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## Read in data ----
  
  # Read in dataset
  FileData <- reactive({
    infile <- input$DataFilePath
    
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    
    # Return data
    FileData <- readr::read_csv(infile$datapath)
    
    return(FileData)
  })
  
  # Dataset as ts object
  tsFileData <- reactive({
    df <- FileData()
    
    if (is.null(df)) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      
      # find start point
      if (is.null(input$YearStart)) {
        # warning("No integer year variable selected - time series may not display 
        #         correctly. Number of periods set by default to 1.")
        getTimeSeries <- stats::ts(df, frequency = 1)
      } else {
        if (is.null(input$PeriodStart)) {
          # warning("No integer period variable selected - time series may not 
          #         display correctly. Number of periods set by default to 1.")
          getTimeSeries <- stats::ts(df, frequency = 1,
                                     start = c(input$YearStart, 1))
        } else {
          strStart <- c(input$YearStart, input$PeriodStart)
          intFrequency <- max(df[, input$PeriodVar])
          getTimeSeries <- stats::ts(df, 
                                     frequency = intFrequency,
                                     start = strStart)
        }
      }
    
      return(getTimeSeries)
    }
  })
  
  # Function to return time series object
  getTimeSeries <- function(df, intFreq = NULL, intStartYear = NULL, intStartPeriod = NULL) {
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
  
  # Dependent var dataset
  tsDepVar <- reactive({
    df <- tsFileData()
    
    if (is.null(input$DepVar) | is.null(df)) {
      return(NULL)
    } else{
      df <- df[, input$DepVar]
      # find start point
      if (is.null(input$YearStart)) {
        # warning("No integer year variable selected - time series may not display 
        #         correctly. Number of periods set by default to 1.")
        getTimeSeries <- stats::ts(df, frequency = 1)
      } else {
        if (is.null(input$PeriodStart)) {
          # warning("No integer period variable selected - time series may not 
          #         display correctly. Number of periods set by default to 1.")
          getTimeSeries <- stats::ts(df, frequency = 1,
                                     start = c(input$YearStart, 1))
        } else {
          strStart <- c(input$YearStart, input$PeriodStart)
          intFrequency <- max(df[, input$PeriodVar])
          getTimeSeries <- stats::ts(df, 
                                     frequency = intFrequency,
                                     start = strStart)
        }
      }
  
      return(getTimeSeries)
    }
  })
  
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
    if (is.null(df)) return(NULL)
    
    items <- names(df)
    names(items) <- items
    selectInput("DepVar", 
                "Now choose your dependent variable:",
                items,
                selected = items[3])
    
  })
  
  output$YearVar <- renderUI({
    df <- FileData()
    if (is.null(df)) return(NULL)
    
    items <- names(df)
    names(items) <- items
    selectInput("YearVar", 
                "Which variable contains the time period (e.g. year)?",
                items,
                selected = items[1])
    
  })
  
  output$PeriodVar <- renderUI({
    df <- FileData()
    if (is.null(df)) {
      items <- NULL
      itemToSelect <- NULL
    } else {
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
    df <- FileData()
    if (is.null(df)) {
      items <- NULL
    } else {
      items <- unique(df[, input$YearVar])
      names(items) <- items
    }
    
    # setup input
    selectInput("DataStart",
                "What's the first year in the data?",
                items)
  })
  
  output$PeriodStart <- renderUI({
    df <- FileData()
    if (is.null(df)) {
      items <- NULL
    } else {
      items <- unique(df[, input$PeriodVar])
      names(items) <- items
    }
    
    # setup input
    selectInput("DataStart",
                "What's the first period in the data?",
                items)
  })
  
  # output$DataEnd <- renderUI({
  #   df <- FileData()
  #   if (is.null(df)) return(NULL)
  #   
  #   items <- unique(df[, input$YearVar])
  #   names(items) <- items
  #   selectInput("DataEnd",
  #               "Select end of time series:",
  #               choices = items,
  #               selected = items[length(items)])
  # })
  # 
  
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
  
  
  ## Plots ----
  
  # Generate plot of dependent variable
  output$DepVarPlot <- renderDygraph({
    if (is.null(input$DataFilePath)) {
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
            dygraph(tsDepVar(),
                    main = "Plot of dependent variable") %>%
              dyRangeSelector(height = 40)
          }
        # } else {
          # Plot frequency histogram
          # ggplot2::ggplot(PlottingData, 
          #   ggplot2::aes_string(x = input$DepVar)) +
          #   ggplot2::geom_histogram(color="black", 
          #                           fill="turquoise", 
          #                           binwidth=((max(PlotY)-min(PlotY))/10),
          #                           stat="bin") +
          #   ggplot2::labs(title=paste0("Frequency of ", input$DepVar), 
          #                 y="Count", 
          #                 x=input$DepVar) +
          #   ggplot2::theme_classic()
        # }
      }
    }
  })
  
  # Generate plots of all variables
  output$tsPlots <- renderPlot({
    df <- tsFileData()

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
    fit <- forecast::naive(tsDepVar())
    return(fit)
  })
  
  forecastNaive <- reactive({
    fit <- fitNaive()
    return(forecast::forecast(fit))
  })
  
  output$plotNaive <- renderPlot({
    fit <- forecast::naive(stats::ts(tsDepVar()))
    return(plot(fit))
  })
  
  
  ## Time series decomposition ----
  fitDecomposition <- reactive({
    # fit <- stats::stl(tsDepVar(), 
    #                   s.window = "period")
    #browser()
    df <- stats::ts(tsDepVar(), frequency = 4, start = c(1962, 1))
    fit <- stats::decompose(df, type = "additive")
    return(fit)
  })
  
  forecastDecomposition <- reactive({
    fit <- stats::stl(tsDepVar(), s.window = "period")
    return(forecast::forecast(fit))
  })
  
  output$plotDecomposition <- renderPlot({
    ds_ts <- tsDepVar()
    ds_ts <- getTimeSeries(ds_ts, 
                           intFreq = 4, 
                           intStartYear = 1962, 
                           intStartPeriod = 1)
    fit <- stats::stl(ds_ts, s.window = "period")
    #fit <- stats::decompose(ds_ts, type = "additive")
    return(plot(forecast::forecast(fit)))
  })
  
  output$plotDecompositionComponents <- renderPlot({
    fit <- fitDecomposition()
    return(plot(fit))
  })
  
  
  ## Holt-Winters ----
  fitHoltWinters <- reactive({
    fit <- stats::HoltWinters(tsDepVar())
    return(fit)
  })
  
  forecastHoltWinters <- reactive({
    fit <- fitHoltWinters()
    return(forecast::forecast(fit))#, ts = TRUE))
  })
  
  output$plotHoltWinters <- renderPlot({
    fit <- stats::HoltWinters(tsDepVar())
    return(plot(forecast::forecast(fit, h = 12)))
  })
  

  ## Linear regression ----
  output$fitLinearRegression <- renderPrint({
    # get data and remove time variable
    fitData <- tsFileData()
    
    # find variables to include (including seasonal dummy variables)
    varsToInclude <- colnames(fitData)[which(!(colnames(fitData) %in% c("Time", "y")))]
    varsToInclude <- c(varsToInclude, "season")
    strFormula <- stats::reformulate(varsToInclude, response = input$DepVar)
    
    fit <- forecast::tslm(formula = strFormula, data = fitData)
    return(summary(fit))
  })
  
  # forecastLinearRegression <- reactive({
  #   fit <- fitLinearRegression()
  #   return(forecast(fit))
  # })
  # 
  # output$plotLinearRegression <- renderPlot({
  #   fit <- fitLinearRegression()
  #   return(plot(forecast(fit, h = 12)))
  # })
  # 
  
  # Generate plot showing all forecasts
  output$plotForecasts <- renderDygraph({
    # get forecasts
    hist <- tsDepVar()
    naive <- forecastNaive()
    decomposition <- forecastDecomposition()
    holtwinters <- forecastHoltWinters()
    # regression <- forecastLinearRegression()
    
    # put together plot
    plotData <- ts.union(
      hist, 
      naive$mean, 
      decomposition$mean, 
      holtwinters$mean#,
      # regression$mean
    )
    
    # convert to data frame for plotting
    plotData <- data.frame(Y=as.matrix(plotData), Date=time(plotData))
    names(plotData) <- c("Historical data",
                         "Naive forecast",
                         "Time Series Decomposition forecast", 
                         "Holt-Winters forecast",
                         "Date")
    
    p <- dygraph(plotData,
                 main = "Comparison of forecasts"
                 )
    
    # p <- plotly::plot_ly(data = plotData,
    #                      x = plotData$Date,
    #                      y = plotData$`Historical data`,
    #                      name = "Historical data",
    #                      type = 'scatter',
    #                      mode = 'lines') %>%
    #   plotly::layout(title = "Comparison of forecasts",
    #                  xaxis = list(title = "Time"),
    #                  yaxis = list(title = " "), 
    #                              # range = c(0, ceiling(max(plotData[1:4], na.rm = TRUE)))),
    #                  legend = list(x = 100, y = 0.5))
    # 
    # for (trace in colnames(plotData)){
    #   if (!(trace %in% c("Date", "trace 0", "Historical data"))){
    #     p <- p %>% plotly::add_trace(y = as.formula(paste0("~`", trace, "`")),
    #                                  name = trace)
    #   }                                                                  
    # }
    
    return(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
#shiny::runApp(display.mode="showcase")
