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
#' @importFrom dygraphs renderDygraph
mod_plot_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("dep_var_selector")),
  )
}

# Module Server

#' @rdname mod_plot_data
#' @export
#' @keywords internal

mod_plot_data_server <- function(input, output, session, r){
  ns <- session$ns

  # Selector for dependent variable
  output$dep_var_selector <- renderUI({
    req(r$data)
    selectInput("dep_var_selector",
                label = "Select you Dependent Variable:",
                choices = names(r$data %>% select(-c("Year", "Quarter"))),
                )
  })

  # dyGraph of the independent variable
  output$ind_var_dygraph <- renderDygraph({
    req(r$data, input$ind_var_selector)
    #function to convert from df to dygraph
  })
}

## To be copied in the UI
# mod_plot_data_ui("plot_data_ui_1")

## To be copied in the server
# callModule(mod_plot_data_server, "plot_data_ui_1")

