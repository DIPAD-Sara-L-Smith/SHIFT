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
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Compare short-term forecasts",
        status = "primary",
        solidHeader = TRUE,
        h3("Short-term forecasts"),
        p("This box allows you to compare all short-term forecasts."),
        plotly::plotlyOutput(ns("plot_shortterm"))

        # conditionalPanel(
        #   condition = "is.null(output.plot_shortterm2) == FALSE",
        #   h3("Differenced data"),
        #   p("The chart above shows the inverse differenced models - use the
        #     chart below to view just the differenced data."),
        #   plotly::plotlyOutput(ns("plot_shortterm2"))
        # )
      ),

      box(
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        title = "Naive",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("plot_naive"))
      ),

      box(
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        title = "Holt-Winters",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("plot_holtwinters"))
      ),

      box(
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        title = "Time Series Decomposition",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("plot_decomposition"))
      ),

      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Compare long-term forecasts",
        status = "primary",
        solidHeader = TRUE,
        h3("Long-term forecasts"),
        p("This box currently only shows the selected linear regression model,
          but could be developed in the future to show multiple and compare
          them and related statistics."),
        plotly::plotlyOutput(ns("plot_longterm"))
      )
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

  # graph comparing multiple short-term forecasts with CIs
  observeEvent(r$xts, {
    req(r$data, r$dep_var)

    output$plot_shortterm <- plotly::renderPlotly(
      p <- plot_forecast(
        df = r$data,
        dep_var = r$dep_var,
        # start,
        # end,
        forecast_type = c("naive",
                          "holtwinters",
                          "decomposition"),
        proj_data = NULL,
        diff_inv = ifelse(is.null(r$flg_diff),
                          FALSE,
                          r$flg_diff),
        diff_starting_values = ifelse(r$flg_diff,
                                      as.matrix(r$starting_values[, r$dep_var]),
                                      NULL)
      )
    )
  })

  # # graph comparing multiple short-term forecasts with CIs
  # observeEvent(r$xts, {
  #   req(r$data, r$dep_var, r$flg_diff)
  #
  #   output$plot_shortterm2 <- if (r$flg_diff) {
  #     plotly::renderPlotly(
  #       p <- plot_forecast(
  #         df = r$data,
  #         dep_var = r$dep_var,
  #         # start,NU
  #         # end,
  #         forecast_type = c("naive",
  #                           "holtwinters",
  #                           "decomposition"),
  #         proj_data = NULL,
  #         diff_inv = FALSE,
  #         diff_starting_values = NULL
  #       )
  #     )
  #   } else {
  #     return(NULL)
  #   }
  # })

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

    # graph comparing long-term forecasts with CIs
    observeEvent(r$xts, {
      req(r$data, r$dep_var, r$ind_vars)

      output$plot_longterm <- plotly::renderPlotly(
        p <- plot_forecast(
          df = r$data,
          dep_var = r$dep_var,
          ind_var = r$ind_var,
          # start,
          # end,
          forecast_type = c("linear"),
          proj_data = NULL,
          diff_inv = FALSE
        )
      )
    })
  })

}

## To be copied in the UI
# mod_review_forecasts_ui("review_forecasts_ui_1")

## To be copied in the server
# callModule(mod_review_forecasts_server, "review_forecasts_ui_1")

