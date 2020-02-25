#' @import shiny
app_server <- function(input, output,session) {

  # use golems recommeded petit r.
  r <- reactiveValues()
  # List the first level callModules here
  callModule(mod_load_data_server, "load_data_ui_1", r)
  callModule(mod_plot_data_server, "plot_data_ui_1", r)
}
