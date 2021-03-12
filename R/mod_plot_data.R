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
#' @importFrom plotly plotlyOutput
mod_plot_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Look at it here.",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("dep_var_selector")),
        uiOutput(ns("ind_var_selector")),
        dygraphOutput(ns("dep_var_dygraph")),
        uiOutput(ns("RangeHistorical")),
        br(),
        actionButton(ns("browser_button"), label = "Debug Browser()")
        # plotlyOutput(ns("plot_holtwinters"))
      )
    )
  )
}

# Module Server

#' @rdname mod_plot_data
#' @export
#' @keywords internal
mod_plot_data_server <- function(id, r) {
  moduleServer(
    id,
    function(input, output, session) {
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

      observeEvent(input$dep_var_selector, {
        req(input$dep_var_selector)
        r$dep_var <- input$dep_var_selector
      })

      observeEvent(input$ind_var_selector, {
        req(input$ind_var_selector)
        r$ind_var <- input$ind_var_selector
      })

      # Selector for dependent variable
      output$dep_var_selector <- renderUI({
        req(r$xts)
        selectInput(ns("dep_var_selector"),
                    label = "Select your Dependent Variable:",
                    choices = names(r$xts)
        )
      })

      # Selector for independent variables
      output$ind_var_selector <- renderUI({
        req(r$xts, input$dep_var_selector)
        selectInput(ns("ind_var_selector"),
                    label = "Select your Independent Variables: (*) Multiple Allowed",
                    # drop the current dep_var from the options
                    choices = setdiff(names(r$xts), input$dep_var_selector),
                    multiple = TRUE
        )
      })

      # when user updates last period of historical data, setup end period for
      # models
      observeEvent(input$RangeHistorical, {
        req(r$data, r$dep_var)

        # read in the bounds for historical data (from user input)
        r$date_start <- yq(input$RangeHistorical[1])
        r$date_start <- c(year(r$date_start), quarter(r$date_start))
        r$date_end <- yq(input$RangeHistorical[2])
        r$date_end <- c(year(r$date_end), quarter(r$date_end))
      })

      # slider input - Historical data -----
      output$RangeHistorical <- renderUI({
        #req(r$data, r$dep_var)
        req(r$data)
        df <- r$data

        # create the sequence of Date objects
        dateList <- seq(yq(paste0(
          df[1, "Year"],
          ": Q",
          df[1, "Quarter"]
        )),
        to = yq(paste0(
          df[nrow(df), "Year"],
          ": Q",
          df[nrow(df), "Quarter"]
        )),
        by = "quarter"
        )

        # format vector
        dateListFormatted <- as.yearqtr(dateList)

        # find default end for historical data
        # (based on when dependent variable ends)
        if (is.null(r$dep_var)) {
          defaultEnd <- dateListFormatted[length(dateListFormatted)]
        } else {
          # find the last data point for the selected dependent variable
          lastDepVarDataPoint <- df %>%
            select("Year", "Quarter", r$dep_var)
          lastDepVarDataPoint <- na.trim(lastDepVarDataPoint)
          lastDepVarDataPoint <- lastDepVarDataPoint[nrow(lastDepVarDataPoint), ]

          defaultEnd <- c(
            as.numeric(lastDepVarDataPoint[, "Year"]),
            as.numeric(lastDepVarDataPoint[, "Quarter"])
          )
        }

        # put together widget
        sliderTextInput(
          inputId = ns("RangeHistorical"),
          label = "Select the start and end points for the historical data",
          grid = TRUE,
          force_edges = TRUE,
          choices = dateListFormatted,
          selected = c(
            dateListFormatted[1],
            defaultEnd
          )
        )
      })

      # Delete for prod, or add to golem_dev function.
      observeEvent(input$browser_button, {
        browser()
      })
    }
  )
}
