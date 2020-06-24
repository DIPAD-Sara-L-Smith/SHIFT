#' review_forecasts UI Function
#'
#' @title   mod_review_forecasts_ui and mod_review_forecasts_server
#' @description  A shiny Module containing code relating to the 'Review
#' forecasts' tab.
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
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_review_forecasts_ui <- function(id){
  ns <- NS(id)
  tagList(
    # actionButton(ns("browser_button"), label = "Browser()"),
    fluidRow(
      box(
        # TODO - change this to a compare-all forecasts graph
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Historic data",
        status = "primary",
        solidHeader = TRUE,
        dygraphOutput(ns("dep_var_dygraph"))
      ),

      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Naive",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("plot_naive"))
      ),

      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Holt-Winters",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("plot_holtwinters"))
      ),

      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Time Series Decomposition",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("plot_decomposition"))
      )

      # TODO - add other forecasts in separate boxes below
    )
  )
}

#' review_forecasts Server Function
#'
#' @noRd
mod_review_forecasts_server <- function(input, output, session, r){
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

  # Graph of Holt-Winters forecast
  observeEvent(r$data, {
    req(r$data)

    # Holt-Winters plot
    output$plot_holtwinters <- plotly::renderPlotly({
      # function to convert from df to dygraph
      p <- plot_forecast(
        df = r$data,
        dep_var = r$dep_var,
        ind_var = NULL,
        # start,
        # end,
        forecast_type = "holtwinters",
        proj_data = NULL,
        diff_inv = FALSE
      )
    })

    # Naive forecast
    output$plot_naive <- plotly::renderPlotly({
      # function to convert from df to dygraph
      p <- plot_forecast(
        df = r$data,
        dep_var = r$dep_var,
        ind_var = NULL,
        # start,
        # end,
        forecast_type = "naive",
        proj_data = NULL,
        diff_inv = FALSE
      )
    })

    # Naive forecast
    output$plot_decomposition <- plotly::renderPlotly({
      # function to convert from df to dygraph
      p <- plot_forecast(
        df = r$data,
        dep_var = r$dep_var,
        ind_var = NULL,
        # start,
        # end,
        forecast_type = "decomposition",
        proj_data = NULL,
        diff_inv = FALSE
      )
    })
  })
}

## To be copied in the UI
# mod_review_forecasts_ui("review_forecasts_ui_1")

## To be copied in the server
# callModule(mod_review_forecasts_server, "review_forecasts_ui_1")

