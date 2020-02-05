#' @import shiny
#' @import shinydashboard
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
      ## Title & sidebar -----
      # Application title
      dashboardHeader(title = "SHIFT"),

      # Sidebar with a slider input for number of bins
      dashboardSidebar(
        sidebarMenu(
          menuItem("Explore data",
                   tabName = "explore",
                   icon = icon("table")
          ),
          menuItem("Compare forecasts",
                   tabName = "forecasts",
                   icon = icon("line-chart")
          ),
          menuItem("Review regression models",
                   tabName = "review-regression",
                   icon = icon("list-ol")
          )
        )
      ),

      dashboardBody(
        tabItems(

          ## Explore data tab -----
          tabItem(
            tabName = "explore",
            fluidRow(
              box(
                width = 12,
                h4("Use this tab to explore a dataset of your choice."),
                mod_load_data_ui("load_data_ui_1")
              )
            ),
          ), # tabItem end

          # Compare forecasts -----
          tabItem(
            tabName = "forecasts",
            fluidRow(
              p("To be added...")
            )
          ), # tabItem end


          # Review regression models -----
          tabItem(
            tabName = "review-regression",
            fluidRow(
              p("To be added...")
            )
          ) # tabItem end
        )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){

  addResourcePath(
    'www', system.file('app/www', package = 'shift')
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

