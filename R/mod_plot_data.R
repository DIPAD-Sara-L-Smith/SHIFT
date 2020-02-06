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
#' @importFrom shiny NS tagList
mod_plot_data_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

# Module Server

#' @rdname mod_plot_data
#' @export
#' @keywords internal

mod_plot_data_server <- function(input, output, session, r){
  ns <- session$ns
}

## To be copied in the UI
# mod_plot_data_ui("plot_data_ui_1")

## To be copied in the server
# callModule(mod_plot_data_server, "plot_data_ui_1")

