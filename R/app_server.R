#' @import shiny
app_server <- function(input, output,session) {

  # use golems recommeded petit r.
  r <- reactiveValues()
  # List the first level callModules here
  callModule(mod_load_data_server, "load_data_ui_1", r)
  callModule(mod_plot_data_server, "plot_data_ui_1", r)
  callModule(mod_review_forecasts_server, "review_forecasts_ui_1", r)
  callModule(mod_best_subset_server, "best_subset_ui_1", r)
}
