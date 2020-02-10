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
mod_plot_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("browser_button"), label = "Browser()"),
    uiOutput(ns("dep_var_selector")),
    dygraphOutput(ns("dep_var_dygraph"))
  )
}

# Module Server

#' @rdname mod_plot_data
#' @export
#' @keywords internal

mod_plot_data_server <- function(input, output, session, r) {
  ns <- session$ns

  # Selector for dependent variable
  output$dep_var_selector <- renderUI({
    req(r$data)
    items <- names(r$data %>% select(-c("Year", "Quarter")))

    selectInput(ns("dep_var_selector"),
      label = "Select you Independent Variable:",
      choices = items,
      selected = items[1]
    )
  })

  observeEvent(input$dep_var_selector, {
    # dyGraph of the independent variable
    output$dep_var_dygraph <- renderDygraph({
      req(r$data, input$dep_var_selector)
      # function to convert from df to dygraph

      p <- dygraph(df_to_xts(r$data)) %>%
        dyLegend(show = "follow",
                 labelsSeparateLines = TRUE) %>%
        dyRangeSelector(height = 40)
    })
  })

  observeEvent(input$browser_button, {
    browser()
  })
}

## To be copied in the UI
# mod_plot_data_ui("plot_data_ui_1")

## To be copied in the server
# callModule(mod_plot_data_server, "plot_data_ui_1")
