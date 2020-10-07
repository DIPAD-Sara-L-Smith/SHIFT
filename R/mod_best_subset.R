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
    fluidRow(
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Select inputs.",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("dep_var_selector")),
        uiOutput(ns("ind_var_selector")),
        actionButton(ns("run_subset_button"), label = "Do regression")
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Results from best subsets regression.",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("plot_best_subset_stats")),
        DTOutput(ns("summaries_table"))
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        title = "Analyse quality of best subset output.",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("analyse_model_selector")),
        actionButton(ns("run_diagnostics"), label = "Show diagnostics"),
        plotOutput(ns("plot_best_subset_diagnostics"))
      )
    )
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

  # Runs the all subsets calculations
  observeEvent(input$run_subset_button, {
    req(r$data, input$dep_var_selector, input$ind_var_selector)
    r$allsubset <- allsubsetregression(input$dep_var_selector, r$data, length(input$ind_var_selector))
  })

  # Renders the plot object generated from all subsets calculation
  observeEvent(r$allsubset, {
    req(r$allsubset$plot)
    output$plot_best_subset_stats <- renderPlotly(r$allsubset$plot)
  })

  # Renders the datatable containing the model summary data
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

  # Selector for choosing model for displaying diagnostics
  output$analyse_model_selector <- renderUI({
    req(r$allsubset$formula_results)
    selectInput(ns("analyse_model_selector"),
                label = "Select model to check quality of fit",
                choices = r$allsubset$formula_results,
                multiple = FALSE
    )
  })

  # Renders the plot for the diagnostics
  observeEvent(input$run_diagnostics, {
    req(r$data, input$analyse_model_selector)
    modelindex <- which(r$allsubset$formula_results==input$analyse_model_selector)
    output$plot_best_subset_diagnostics <- renderPlot(plot(r$allsubset$autoplots[[modelindex]]))
  })
}
