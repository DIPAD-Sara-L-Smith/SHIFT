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
#' @importFrom shiny NS tagList fileInput actionButton
#' @importFrom DT renderDT DTOutput
#'
mod_load_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(
      ns("file"),
      "Choose a file or files",
      accept = c(
        ".csv",
        ".xlxs",
        ".R"
      ),
      multiple = TRUE
    ),
    DTOutput(ns("user_DT")),
    actionButton(ns("undo"), label = "Undo", icon = icon("undo")),
    actionButton(ns("keep_col"), label = "Keep Selected"),
    actionButton(ns("drop_col"), label = "Drop Selected"),
    actionButton(ns("add_data"), label = "Add more data", icon = icon("plus-square")),
  )
}

# Module Server

#' @rdname mod_load_data
#' @export
#' @keywords internal

mod_load_data_server <- function(input, output, session, r) {
  ns <- session$ns

  # When we see a change to the input object
  # load the file.
  observeEvent(input$file, {
    r$data <- load_user_data(input$file)
  })

  # Whenever the underlying data changes update the DT which displays it.
  observeEvent(r$data, {
    output$user_DT <- renderDT(r$data,
      selection = list(
        target = "column"
      ),
      options = list(
        pageLength = 5,
        lengthMenu = list(
          c(5, 15, -1),
          c("5", "15", "All")
        ),
        scrollX = TRUE,
        searching = FALSE,
        ordering = FALSE,
        pagingType = "simple"
      )
    )
  })

  # If the users hits undo revert to the previous dataset, quite crude but might
  # be useful if you make a mistake with the columns. Could be expaned to revert
  # more changes if we make data_old a list of old dataframes. One should do for
  # now.
  observeEvent(input$undo, {
    if (is.null(r$data_old)) {
      message("No operations to undo.")
    } else {
      r$data <- r$data_old
    }
  })
  # Deal with the user adding more data to the current dataframe.
  observeEvent(input$add_data, {
    req(r$data)

    r$data_old <- r$data
    # r$data <- merge_user_data( list(r$data_old, load_user_data(input$file)) )
  })

  # Drop columns
  observeEvent(input$drop_col, {
    req(r$data, input$user_DT_columns_selected)
   cols_to_drop <- input$user_DT_columns_selected
    if( any(c(1,2) %in% cols_to_drop)){
      warning("Dropping Year or Quarter is a bad idea so lets not.")
      cols_to_drop <- cols_to_drop[cols_to_drop %not_in% c(1,2)]
    }

    r$data_old <- r$data
    r$data <- r$data %>% select(-cols_to_drop)
  })

  # Keep columns
  observeEvent(input$keep_col, {
    req(r$data, input$user_DT_columns_selected)

    cols_to_keep <- input$user_DT_columns_selected
    if( any(c(1,2) %not_in% cols_to_keep)){
      warning("Dropping Year or Quarter is a bad idea so lets not.")
      cols_to_keep <- union(c(1,2), cols_to_keep)
    }

    r$data_old <- r$data
    r$data <- r$data %>% select(1:2, cols_to_keep)
  })
}

## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")

## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")

