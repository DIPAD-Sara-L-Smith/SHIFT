# Module UI

#' @title   mod_best_subset_ui and mod_best_subset_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_best_subset
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList uiOutput renderUI selectInput
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom DT DTOutput renderDT
#' @importFrom shinycssloaders withSpinner
mod_best_subset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Overview.",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("overview_text"))
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Select inputs.",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("dep_var_selector")),
        uiOutput(ns("ind_var_selector")),
        uiOutput(ns("spinner_placeholder")) %>% withSpinner(color="#0dc5c1", proxy.height = 35),
        actionButton(ns("run_subset_button"), label = "Do regression")
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Collinearity Matrix",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("coll_mat"))
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Candidate best models from calculations.",
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("formula_table")),
        uiOutput(ns("bestsubset_explainer_text"))
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Compare models using ANOVA.",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("anova_explainer_text")),
        br(),
        uiOutput(ns("first_anova_selector")),
        uiOutput(ns("second_anova_selector")),
        actionButton(ns("run_anova"), label = "Run ANOVA"),
        DTOutput(ns("anova_table")),
        uiOutput(ns("multipleanova_signifcodes"))
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Analyse quality of best subset output.",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("analyse_model_selector")),
        actionButton(ns("run_diagnostics"), label = "Show diagnostics"),
        br(),
        uiOutput(ns("diagnostic_plots_header")),
        br(),
        plotOutput(ns("plot_best_subset_diagnostics")),
        br(),
        uiOutput(ns("diagnostic_plots_explainer_text")),
        br(),
        uiOutput(ns("lm_summaries_table_header")),
        br(),
        DTOutput(ns("lm_summaries_table")),
        uiOutput(ns("lm_ttest_signifcodes")),
        br(),
        uiOutput(ns("lm_anova_table_header")),
        br(),
        DTOutput(ns("lm_anova_table")),
        uiOutput(ns("anova_signifcodes")),
        br(),
        uiOutput(ns("summary_statistics_header")),
        uiOutput(ns("summary_statistics"))
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Choose final best model from analysis.",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("final_model_selector")),
        actionButton(ns("final_model_button"), label = "Confirm selection")
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        title = "Best models extracted from each statistical measure.",
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("determined_models"))
      ),
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        title = "All models calculated for best subsets.",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("plot_best_subset_stats")),
        DTOutput(ns("summaries_table"))
      )
    )
  )
}

# Module Server

#' @rdname mod_best_subset
#' @export
#' @keywords internal
#' @importFrom dplyr select
mod_best_subset_server <- function(input, output, session, r) {
  ns <- session$ns

  # Explainer text to describe the process here.
  observeEvent(r$data, {
    output$overview_text <- renderText({
      HTML("<b><p style='font-size:18px'>
           This is the best subset model, which takes a series of inputs then
           determines, statistically, which models are best.
           <br><br>
           It is possible that multiple models are returned due to closeness
           in statistical parameters, therefore ANOVA is provided to compare
           the possible models.
           <br><br>
           Once a model is determined, the quality of fit can be determined. Finally,
           the selected model can be stored to be used for long term forecasting.
           <br><br>
           To conduct the process, do the following: <br>
           1. Select the Dependent variable and Independent variables, then click on the
           Do Regression button. <br>
           2. View the Candidate best models. <br>
           (i) If there is just one, move to Analyse quality of best subset output
           and click on Show Diagnostics. <br>
           (ii) If there is more than one, go to Compare Models using Anova. Determine
           the best model using this then move to Analyse quality of best subset output
           and click on Show Diagnostics. <br>
           3. Go to Choose final best model from analysis and click on Confirm Selection.
           <br><br>
           All the models that were calculated are included in the two collapsed frames
           at the bottom of this page.
           </b></p>")
    })
  })

  # Selector for dependent variable
  output$dep_var_selector <- renderUI({
    req(r$data)
    selectInput(ns("dep_var_selector"),
      label = "Select your Dependent Variable:",
      choices = names(select(r$data, -c("Year", "Quarter")))
    )
  })

  # Selector for independent variables
  output$ind_var_selector <- renderUI({
    req(r$data, input$dep_var_selector)
    selectInput(ns("ind_var_selector"),
      label = "Select your Independent Variables: (*) Multiple Allowed",
      # drop the current dep_var from the options
      choices = setdiff(names(select(r$data, -c("Year", "Quarter"))), input$dep_var_selector),
      multiple = TRUE
    )
  })

  # Runs the all subsets calculations
  observeEvent(input$run_subset_button, {
    req(r$data, input$dep_var_selector, input$ind_var_selector)
    if (length(input$ind_var_selector) > 1) {
      r$spinner_spinning <- r$spinner_spinning + 1
      r$subsetdata <- select(
                             r$data,
                             #change to filter NAs out of dep_var column
                             #r$data[!is.na(r$data[[input$dep_var_selector]]),],
                             c(input$dep_var_selector,
                               input$ind_var_selector,
                               "Year",
                               "Quarter"))
      r$allsubset <- allsubsetregression(input$dep_var_selector,
                                         r$subsetdata,
                                         length(input$ind_var_selector))
    }
  })

  # spinner for when doing subset regression
  r$spinner_spinning <- 0
  output$spinner_placeholder <- renderUI({
     if(r$spinner_spinning){ NULL } else { NULL }
  })

  # Renders the datatable containing the best models from all analyses
  observeEvent(r$allsubset, {
    output$formula_table <- renderDT(r$allsubset$formula_table,
      rownames = FALSE,
      options = list(
        lengthMenu = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        pagingType = "simple",
        selection = "single"
      )
    )
  })

  # Explainer text for best subsets regression results
  observeEvent(r$allsubset, {
    output$bestsubset_explainer_text <- renderText({
      HTML("<b><p style='font-size:16px'>
           If there is more than one row,
           then the models are statistically very similar
           but one must be selected. Conduct ANOVA testing
           below to help determine which model to use.
           </b></p>")
    })
  })

  # Explainer text for conducting ANOVA
  observeEvent(r$allsubset, {
    output$anova_explainer_text <- renderText({
      HTML("<b><p style='font-size:16px'>ANOVA compares two models against
           each other to find whether adding/removing parameter(s) improves
           a model output.
           <br><br>
           To conduct an ANOVA:<br>
           1. Select the two models to compare and run ANOVA test. <br>
           2. Check whether the p value is significant (Signif_codes column).
           <br><br>
           Do these two steps for each combination of available models and see which
           one amongst them minimises the p-value. This is the optimal model.
           </b></p>")
    })
  })

  # Select first model to compare against (Anova)
  output$first_anova_selector <- renderUI({
    req(r$allsubset)
    selectInput(ns("first_anova_selector"),
      label = "Select model to compare against",
      # drop the current dep_var from the options
      choices = r$allsubset$formula_results,
      multiple = FALSE
    )
  })

  # Selector comparator to compare (Anova)
  output$second_anova_selector <- renderUI({
    req(r$allsubset, input$first_anova_selector)
    selectInput(ns("second_anova_selector"),
      label = "Select model to compare",
      # drop the current dep_var from the options
      choices = setdiff(r$allsubset$formula_results, input$first_anova_selector),
      multiple = FALSE
    )
  })

  # Runs the ANOVA on two variables
  observeEvent(input$run_anova, {
    req(r$allsubset, input$first_anova_selector, input$second_anova_selector)
    r$anova <- as.data.frame(anova(lm(input$first_anova_selector, r$data), lm(input$second_anova_selector, r$data)))
    r$anova$Signif_codes <- ifelsefun(r$anova$`Pr(>F)`)
  })

  # Renders the datatable containing the ANOVA for comparison
  observeEvent(r$anova, {
    output$anova_table <- renderDT(r$anova,
      rownames = FALSE,
      options = list(
        lengthMenu = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        pagingType = "simple",
        selection = "none"
      )
    )
  })

  # Significance codes from output models
  observeEvent(r$anova, {
    output$multipleanova_signifcodes <- renderText({
      HTML("<b>
      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
      </b>")
    })
  })

  # Selector for choosing model for displaying diagnostics
  output$analyse_model_selector <- renderUI({
    req(r$allsubset)
    selectInput(ns("analyse_model_selector"),
      label = "Select model to check quality of fit",
      choices = r$allsubset$formula_results,
      multiple = FALSE
    )
  })

  # Finds the index of the selected model
  observeEvent(input$run_diagnostics, {
    req(r$allsubset, input$analyse_model_selector)
    r$modelindex <- which(r$allsubset$formula_results == input$analyse_model_selector)
  })

  # Header for diagnostic plot
  observeEvent(input$run_diagnostics, {
    req(r$allsubset, input$analyse_model_selector)
    output$diagnostic_plots_header <- renderText({
      HTML("<h1 style='font-size:3vw'><b>
      Diagnostic Plot
      </b></h1>")
    })
  })

  # Renders the plot for the diagnostics
  observeEvent(input$run_diagnostics, {
    req(r$allsubset, input$analyse_model_selector)
    output$plot_best_subset_diagnostics <- renderPlot(plot(r$allsubset$autoplots[[r$modelindex]]))
  })

  # Explains outputs in diagnostic plots
  observeEvent(input$run_diagnostics, {
    req(r$allsubset, input$analyse_model_selector)
    output$diagnostic_plots_explainer_text <- renderText({
      HTML("<p style='font-size:14px'><b>

      This is used to check the quality of fit (additionally,
      it also checks the ANOVA assumptions).
      <br><br>
      1. If the Residuals vs Fitted solid line is close to 0,
      this is a good distribution (homogeneity in variances). <br>
      2. If the Normal Q-Q is close to linear, this is a good fit
      (residuals normally distributed). <br>
      3. If Scale-Location is close to horizontal,
      then this is good (standardised residuals do not vary much
      with fitted values). <br>
      4. Values with high Cook's distance may be outliers (check y scale). <br>
      5. If Residuals vs Leverage graph datapoints away from Cook's distance curves,
      then this is good. <br>
      6. Data points being below the first and second dotted lines from the right
      in general is good
      (data points distort distribution in limited manner).
      <br><br>
      Data points that are labelled as numbers are potential outliers.
      Consider removing data points from analysis.
        </b></p>")
    })
  })

  # Header for t-test
  observeEvent(input$run_diagnostics, {
    req(r$allsubset, input$analyse_model_selector)
    output$lm_summaries_table_header <- renderText({
      HTML("<h1 style='font-size:3vw'><b>
      Single model t-test
      </b></h1>")
    })
  })

  # Renders the datatable containing the lm model summary data
  observeEvent(input$run_diagnostics, {
    output$lm_summaries_table <- renderDT(r$allsubset$lm_summaries[[r$modelindex]],
      rownames = TRUE,
      options = list(
        lengthMenu = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        pagingType = "simple",
        selection = "none"
      )
    )
  })

  # Significance codes from output models
  observeEvent(input$run_diagnostics, {
    output$lm_ttest_signifcodes <- renderText({
      HTML("<b>
      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
      </b>")
    })
  })

  # Runs the ANOVA on one variable
  observeEvent(input$run_diagnostics, {
    r$anovaresult <- as.data.frame(anova(r$allsubset$lm_results[[r$modelindex]]))
    r$anovaresult$Signif_codes <- ifelsefun(r$anovaresult$`Pr(>F)`)
  })

  # Header for single model ANOVA
  observeEvent(input$run_diagnostics, {
    req(r$allsubset, input$analyse_model_selector)
    output$lm_anova_table_header <- renderText({
      HTML("<h1 style='font-size:3vw'><b>
      Single model ANOVA
      </b></h1>")
    })
  })

  # Renders the datatable with ANOVA data
  observeEvent(input$run_diagnostics, {
    output$lm_anova_table <- renderDT(r$anovaresult,
      rownames = TRUE,
      options = list(
        lengthMenu = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        pagingType = "simple",
        selection = "none"
      )
    )
  })

  # Significance codes from output models
  observeEvent(input$run_diagnostics, {
    output$anova_signifcodes <- renderText({
      HTML("<b>
      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
      </b>")
    })
  })

  # Creates summary object of selected lm object
  observeEvent(input$run_diagnostics, {
    r$summaries <- summary(r$allsubset$lm_results[[r$modelindex]])
  })

  # Header for statistics
  observeEvent(input$run_diagnostics, {
    req(r$allsubset, input$analyse_model_selector)
    output$summary_statistics_header <- renderText({
      HTML("<h1 style='font-size:3vw'><b>
      R squared & adjusted R squared
      </b></h1>")
    })
  })

  # Displays the R2 and adj-R2 values
  observeEvent(input$run_diagnostics, {
    output$summary_statistics <- renderText({
      HTML(paste0(c(
        "<b>R-squared:</b> ", round(r$summaries$r.squared, digits = 3),
        "<br><b>Adjusted R-squared:</b> ", round(r$summaries$adj.r.squared, digits = 3)
      )))
    })
  })

  # Selector for the best model
  output$final_model_selector <- renderUI({
    req(r$allsubset)
    selectInput(ns("final_model_selector"),
      label = "Select the final model:",
      choices = r$allsubset$formula_results
    )
  })

  # Stores the selected model in the r object
  observeEvent(input$final_model_button, {
    req(input$final_model_selector)
    #r$best_model <- lm(input$final_model_selector, r$data)
    r$best_model <- input$final_model_selector
    r$dep_var <- input$dep_var_selector
    r$ind_var <- input$ind_var_selector
    InitPath <-
      paste0(
        gsub("OneDrive - ", "", gsub(".{10}$", "", Sys.getenv("HOME"))),
        "\\DVSA Dashboards & Reports - Driver & Rider\\SHIFT\\Outputs\\"
      )
  })

  # Renders the datatable containing the best models from all analyses
  observeEvent(r$allsubset, {
    output$determined_models <- renderDT(r$allsubset$determined_models,
      rownames = FALSE,
      options = list(
        lengthMenu = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        pagingType = "simple",
        selection = "none"
      )
    )
  })

  # Renders the plot object generated from all subsets calculation
  observeEvent(r$allsubset, {
    output$plot_best_subset_stats <- renderPlotly(r$allsubset$plot)
  })

  # Renders the datatable containing the model summary data
  observeEvent(r$allsubset, {
    output$summaries_table <- renderDT(r$allsubset$model_summaries_df,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        lengthMenu = list(
          c(5, 15, -1),
          c("5", "15", "All")
        ),
        scrollX = TRUE,
        searching = FALSE,
        pagingType = "simple",
        selection = "none"
      )
    )
    showNotification("Best Subset Regression completed", type = "message")
  })

  # Render Collinearity Matrix
  observeEvent(r$allsubset, {
    req(r$data, input$ind_var_selector)

    output$coll_mat <- renderPlot({
      plot_cormat(r$data, input$ind_var_selector)
    })
  })

}
