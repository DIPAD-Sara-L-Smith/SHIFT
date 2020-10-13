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
        title = "Best models from calculations.",
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("formula_table")),
        uiOutput(ns("explainer_text"))
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Analyse quality of best subset output.",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("analyse_model_selector")),
        actionButton(ns("run_diagnostics"), label = "Show diagnostics"),
        plotOutput(ns("plot_best_subset_diagnostics")),
        DTOutput(ns("lm_summaries_table"))
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Best models extracted from each parameter.",
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("determined_models"))
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "All models calculated for best subsets.",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("plot_best_subset_stats")),
        DTOutput(ns("summaries_table"))
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
    if (length(input$ind_var_selector) > 1) {
      r$subsetdata <- select(r$data, c(input$dep_var_selector, input$ind_var_selector, "Year", "Quarter"))
      r$allsubset <- allsubsetregression(input$dep_var_selector, r$subsetdata, length(input$ind_var_selector))
    }
  })

  # Renders the datatable containing the best models from all analyses
  observeEvent(r$allsubset, {
    output$formula_table <- renderDT(r$allsubset$formula_table,
      rownames = FALSE,
      options = list(
        lengthMenu = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        pagingType = "simple"
      )
    )
  })

  # Explainer text
  observeEvent(r$allsubset, {
    output$explainer_text <- renderText({
      HTML("<b>If there is more than one row,
           then the models are statistically very similar and so any of them
           can be used.</b>")
    })
  })

  # Finds the index of the selected model
  observeEvent(input$run_diagnostics, {
    req(r$allsubset, input$analyse_model_selector)
    r$modelindex <- which(r$allsubset$formula_results == input$analyse_model_selector)
  })

  # Renders the plot for the diagnostics
  observeEvent(input$run_diagnostics, {
    req(r$allsubset, input$analyse_model_selector)
    output$plot_best_subset_diagnostics <- renderPlot(plot(r$allsubset$autoplots[[r$modelindex]]))
  })

  # Renders the datatable containing the lm model summary data
  observeEvent(input$run_diagnostics, {
    output$lm_summaries_table <- renderDT(r$allsubset$lm_summaries[[r$modelindex]],
      rownames = TRUE,
      options = list(
        lengthMenu = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        pagingType = "simple"
      )
    )
  })

  # Renders the datatable containing the best models from all analyses
  observeEvent(r$allsubset, {
    output$determined_models <- renderDT(r$allsubset$determined_models,
      rownames = FALSE,
      options = list(
        lengthMenu = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        pagingType = "simple"
      )
    )
  })

  # Renders the plot object generated from all subsets calculation
  observeEvent(r$allsubset, {
    output$plot_best_subset_stats <- renderPlotly(r$allsubset$plot)
  })

  # Renders the datatable containing the model summary data
  observeEvent(r$allsubset, {
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
    req(r$allsubset)
    selectInput(ns("analyse_model_selector"),
      label = "Select model to check quality of fit",
      choices = r$allsubset$formula_results,
      multiple = FALSE
    )
  })
}
