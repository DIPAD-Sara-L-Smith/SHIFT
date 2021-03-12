shinyApp(
  ui = mod_review_forecasts_ui("test"),
  server = function(input, output, session) {
    r <- reactiveValues()
    mod_review_forecasts_server("test", r)
  }
)
