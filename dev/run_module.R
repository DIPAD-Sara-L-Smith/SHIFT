library(shiny)
library(shinydashboard)

#source("../R/mod_features.R")
#source("../R/fct_feature.R")


ui <-  dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    mod_features_ui("test")
  )
)

server <-  function(input, output, session) {
    r <- reactiveValues()

    # make upload data frame
    upload <-  data.frame(name = "validation-data.csv", datapath = "../example-data/validation-data.csv")

    #load data
    r$data <- load_user_data(upload)

    mod_features_server("test", r)

  }

shinyApp(ui, server)
