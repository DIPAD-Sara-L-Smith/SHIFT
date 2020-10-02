# Module UI

#' @title   mod_best_subset_ui and mod_best_subset_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_best_subset
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList uiOutput renderUI selectInput
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom DT DTOutput renderDT
mod_best_subset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("browser_button"), label = "Browser()"),
    uiOutput(ns("dep_var_selector")),
    uiOutput(ns("ind_var_selector")),
    plotlyOutput(ns("plot_best_subset_stats")),
    DTOutput(ns("summaries_table")),
    actionButton(ns("run_subset_button"), label = "Do regression")
  )
}

# Module Server

#' @rdname mod_best_subset
#' @export
#' @keywords internal
#' @importFrom dplyr select
#'
mod_best_subset_server <- function(input, output, session, r) {
  ns <- session$ns

  # Selector for dependent variable
  output$dep_var_selector <- renderUI({
    req(r$data)
    selectInput(ns("dep_var_selector"),
      label = "Select your Dependent Variable:",
      choices = names(select(r$data, -c("Year", "Quarter")))
    )
  })

  # Selector for independent variables
  output$ind_var_selector <- renderUI({
    req(r$data, input$dep_var_selector)
    selectInput(ns("ind_var_selector"),
      label = "Select your Independent Variables: (*) Multiple Allowed",
      # drop the current dep_var from the options
      choices = setdiff(names(select(r$data, -c("Year", "Quarter"))), input$dep_var_selector),
      multiple = TRUE
    )
  })

  observeEvent(input$run_subset_button, {
    req(r$data, input$dep_var_selector, input$ind_var_selector)
    r$allsubset <- allsubsetregression(input$dep_var_selector, r$data, length(input$ind_var_selector))
  })

  observeEvent(r$allsubset, {
    req(r$allsubset$plot)
    output$plot_best_subset_stats <- renderPlotly(r$allsubset$plot)
  })

  observeEvent(r$allsubset, {
    req(r$allsubset$model_summaries_df)
    output$summaries_table <- renderDT(r$allsubset$model_summaries_df,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        lengthMenu = list(
          c(5, 15, -1),
          c("5", "15", "All")
        ),
        scrollX = TRUE,
        searching = FALSE,
        pagingType = "simple"
      )
    )
  })

  # Delete for prod, or add to golem_dev function.
  observeEvent(input$browser_button, {
    browser()
  })
}

## To be copied in the UI
# mod_best_subset_ui("best_subset_ui_1")

## To be copied in the server
# callModule(mod_best_subset_server, "best_subset_ui_1")
#' best_subset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_best_subset_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' best_subset Server Function
#'
#' @noRd 
mod_best_subset_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_best_subset_ui("best_subset_ui_1")
    
## To be copied in the server
# callModule(mod_best_subset_server, "best_subset_ui_1")
 
