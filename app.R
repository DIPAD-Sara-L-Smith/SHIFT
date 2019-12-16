#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

source("forecasting.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Exploratory Statistical Tool (EXSTAT)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        shiny::tags$p("Explore and perform regression on a dataset of your choice."),
        
        fileInput("DataFilePath",
                  "First, select your dataset:",
                  multiple = FALSE,
                  accept = NULL,
                  width = "400px",
                  buttonLabel = "Find file", 
                  placeholder = "Data file path"),
        
        uiOutput("DataType"),
        
        uiOutput("DepVar"),

        conditionalPanel(
          condition = "input.DataType == 'Time series'", 
          uiOutput("YearVar"),
          
          # uiOutput("TimeInterval"),
          uiOutput("DataStart"),
          # uiOutput("DataStartInterval"),
          uiOutput("DataEnd")#,
          # uiOutput("DataEndInterval")
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h5("Here is a list of variables in your data: "), 
        verbatimTextOutput("DataInfo"),
        
        h5("And here is a summary of the data: "), 
        verbatimTextOutput("DataHead"),
        
        h5("What does your dependent variable look like?"), 
        plotOutput("DepVarPlot"),
        
        h5("Here are plots of all variables:"), 
        plotOutput("tsPlots")
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
    FileData <- as.data.frame(read.csv(infile$datapath, 
                                       stringsAsFactors = FALSE))
    
    return(FileData)
  })
  
  # Dataset as ts object
  tsFileData <- reactive({
    df <- FileData()
    tsFileData <- ts(df)#,
                     # start = c(input$DataStart, 1),
                     # end = c(input$DataEnd, 1))
    return(tsFileData)
  })
  
  # Dependent var dataset
  tsDepVar <- reactive({
    df <- FileData()
    
    if (is.null(input$DepVar) | is.null(df)) {
      return(NULL)
    } else{
      tsDepVar <- df[, input$DepVar]
      return(tsDepVar)
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
                             "Time series"))
  })
  
  output$DepVar <- renderUI({
    df <- FileData()
    if (is.null(df)) return(NULL)
    
    items <- names(df)
    names(items) <- items
    selectInput("DepVar", 
                "Now choose your dependent variable:",
                items,
                selected = items[2])
    
  })
  
  output$YearVar <- renderUI({
    df <- FileData()
    if (is.null(df)) return(NULL)
    
    items <- names(df)
    names(items) <- items
    selectInput("YearVar", 
                "Which variable hows the time period:",
                items)
    
  })
  
  output$DataStart <- renderUI({
    df <- FileData()
    if (is.null(df)) return(NULL)
    
    items <- unique(df[, input$YearVar])
    names(items) <- items
    selectInput("DataStart",
                "Select start of time series:",
                items)
  })
  
  output$DataEnd <- renderUI({
    df <- FileData()
    if (is.null(df)) return(NULL)
    
    items <- unique(df[, input$YearVar])
    names(items) <- items
    selectInput("DataEnd",
                "Select end of time series:",
                choices = items,
                selected = items[length(items)])
  })
  
  # output$TimeInterval <- renderUI({
  #   items <- c("Annual",
  #              "Quarterly")
  #   names(items) <- items
  #   selectInput("TimeInterval",
  #               "Select time interval:",
  #               items)
  # })
  
  # Generate textual info about data for display in app
  output$DataInfo <- renderPrint({
    # shiny::tags$h3("Variable names:")
    # shiny::tags$br()
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
  output$DepVarPlot <- renderPlot({
    if (is.null(input$DepVar)) {
      return("No dependent variable has been selected.")
    } else {
      if(is.null(input$DataFilePath)) {
        return("No data file has been selected.")
      } else {
        # Get data
        PlottingData <- as.data.frame(FileData())
        
        if (input$DataType == "Time series") {
          if (is.null(input$YearVar)) {
            return("No time period has been selected.")
          } else {
            # PlotX <- as.data.frame(PlottingData[, input$YearVar])
            # PlotY <- as.data.frame(PlottingData[, input$DepVar])
            
            ggplot2::ggplot(PlottingData, 
                            ggplot2::aes_string(y = input$DepVar, 
                                         input$YearVar)) +
              ggplot2::geom_point() +
              ggplot2::labs(title=paste0(input$DepVar, " by ", input$YearVar), 
                            y=input$DepVar, 
                            x=input$YearVar) +
              ggplot2::theme_classic()
          }
        } else {
          # Plot frequency histogram
          ggplot2::ggplot(PlottingData, 
            ggplot2::aes_string(x = input$DepVar)) +
            ggplot2::geom_histogram(color="black", 
                                    fill="turquoise", 
                                    binwidth=((max(PlotY)-min(PlotY))/10),
                                    stat="bin") +
            ggplot2::labs(title=paste0("Frequency of ", input$DepVar), 
                          y="Count", 
                          x=input$DepVar) +
            ggplot2::theme_classic()
        }
      }
    }
  })
  
  # Generate plots of all variables
  output$tsPlots <- renderPlot({
    plot(tsFileData())
  })
  
  # Generate plot showing all forecasts
  output$forecasts <- renderPlotly({
    # get forecasts
    hist <- tsDepVar()
    naive <- forecastNaive()
    decomposition <- forecastDecomposition()
    holtwinters <- forecastHoltWinters()
    
    # put together plot
    plotData <- ts.union(
      hist, 
      naive$mean, 
      decomposition$mean, 
      holtwinters$mean
    )
    
    # convert to data frame for plotting
    plotData <- data.frame(Y=as.matrix(plotData), Date=time(plotData))
    names(plotData) <- c("Historical data",
                         "Naive forecast",
                         "Time Series Decomposition forecast", 
                         "Holt-Winters forecast",
                         "Date")
    
    p <- plotly::plot_ly(data = plotData,
                         x = plotData$Date,
                         y = plotData$`Historical data`,
                         name = "Historical data",
                         type = 'scatter',
                         mode = 'lines') %>%
      plotly::layout(title = "Comparison of forecasts",
             xaxis = list(title = "Time"),
             yaxis = list(title = " ", 
                          range = c(0, ceiling(max(plotData[1:4], na.rm = TRUE)))),
             legend = list(x = 100, y = 0.5))
    
    for (trace in colnames(plotData)){
      if (!(trace %in% c("Date", "trace 0", "Historical data"))){
        p <- p %>% plotly::add_trace(y = as.formula(paste0("~`", trace, "`")),
                                     name = trace)
      }                                                                  
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
#shiny::runApp(display.mode="showcase")
