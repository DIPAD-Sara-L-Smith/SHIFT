# Module UI

#' @title   mod_plot_data_ui and mod_plot_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_plot_data
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList uiOutput renderUI selectInput
#' @importFrom dygraphs renderDygraph dygraphOutput dygraph dyRangeSelector dyLegend
#' @importFrom plotly plotlyOutput
mod_plot_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("browser_button"), label = "Browser()"),
    dygraphOutput(ns("dep_var_dygraph")),
    uiOutput(ns("dep_var_selector")),
    uiOutput(ns("ind_var_selector")),
    plotlyOutput(ns("plot_holtwinters"))
  )
}

# Module Server

#' @rdname mod_plot_data
#' @export
#' @keywords internal

mod_plot_data_server <- function(input, output, session, r) {
  ns <- session$ns

  # Each time we see a change to r$data we should regenerate the times series
  # object. This is quite quick at the moment, but may need to be triggered by
  # something else if it starts to hold things up.
  observeEvent(r$data, {
    req(r$data)
    r$xts <- df_to_xts(r$data)
  })

  observeEvent(r$xts, {
    # dyGraph of the independent variable
    output$dep_var_dygraph <- renderDygraph({
      req(r$xts)
      # function to convert from df to dygraph
      p <- dygraph(r$xts) %>%
        dyLegend(
          show = "follow",
          labelsSeparateLines = TRUE
        ) %>%
        dyRangeSelector(height = 40)
    })
  })

  # Selector for dependent variable
  output$dep_var_selector <- renderUI({
    req(r$xts)
    selectInput(ns("dep_var_selector"),
      label = "Select your Dependent Variable:",
      choices = names(r$xts)
    )
  })

  # Selector for independent variables
  output$ind_var_selector <- renderUI({
    req(r$xts, input$dep_var_selector)
    selectInput(ns("ind_var_selector"),
      label = "Select your Independent Variables: (*) Multiple Allowed",
      # drop the current dep_var from the options
      choices = setdiff(names(r$xts), input$dep_var_selector),
      multiple = TRUE
      )
  })

  # Graph of Holt-Winters forecast
  observeEvent(r$data, {
    # dyGraph of the independent variable
    output$plot_holtwinters <- renderPlotly({
      req(r$data)

      # function to convert from df to dygraph
      p <- plot_forecast(

         )
    })
  })

  # Delete for prod, or add to golem_dev function.
  observeEvent(input$browser_button, {
    browser()
  })
}
