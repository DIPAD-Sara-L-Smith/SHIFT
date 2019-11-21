#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
        
        uiOutput("DepVar")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h5("Here is a list of variables in your data: "), 
        verbatimTextOutput("DataInfo"),
        
        h5("And here is a summary of the data: "), 
        verbatimTextOutput("DataHead"),
        
        h5("What does your dependent variable look like?"), 
        plotOutput("DepVarPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Read in dataset
  FileData <- reactive({
    infile <- input$DataFilePath
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    
    # Return data
    read.csv(infile$datapath, stringsAsFactors = FALSE)
  })
  
  # Get list of variable names in dataset
  DataVarNames <- reactive({
    df <- FileData()
    if(is.null(df)) {
      return(NULL)
    } else {
      return(names(df)) 
    }
  })
  
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
                items)
    
  })
  
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
  
  
  # Generate plot of dependent variable
  output$DepVarPlot <- renderPlot({
    if(is.null(input$DataFilePath)) {
      return("No data file has been selected.")
    } else {
      # Get data
      PlottingData <- FileData()
      
      if (input$DataType == "Time series") {
        # Plot dependent variable against time
        ggplot2::ggplot(PlottingData, 
                        ggplot2::aes(y = input$DepVar, 
                            x = Year))
        + ggplot2::geom_point()        
      } else {
        # Plot frequency histogram
        x <- as.data.frame(PlottingData[, input$DepVar])
        # bins <- seq(min(x), max(x), length.out = 10 + 1)
      
        # Plot dependent variable
        ggplot2::ggplot(PlottingData, 
          ggplot2::aes(x = x)) + #input$DepVar)) +
          ggplot2::geom_histogram(color="black", fill="turquoise", binwidth=((max(x)-min(x))/10)) +
          ggplot2::labs(title=paste0("Frequency of ", input$DepVar), y="Count", x=input$DepVar) +
          ggplot2::theme_classic()
      }
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

