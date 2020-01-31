# Module UI

#' @title   mod_load_data_ui and mod_load_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_load_data
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList fileInput
mod_load_data_ui <- function(id) {
  ns <- NS(id)
  tagList(fileInput(
    ns("file"),
    "Choose a file or files",
    accept = c(".csv",
               ".xlxs",
               ".R"),
    multiple = TRUE
  ))

}

# Module Server

#' @rdname mod_load_data
#' @export
#' @keywords internal

mod_load_data_server <- function(input, output, session, r){
  ns <- session$ns

  # When we see a change to the input object
  # load the file.
  observeEvent(input$file, {
    r$data <- load_user_data(input$file)
  })

}

## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")

## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")

