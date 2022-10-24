library(shinydashboard)
library(shinyWidgets)

shinyApp(
    ui = dashboardPage(
        dashboardHeader(title = "Automated reports"),
        dashboardSidebar(
            fileInput("inputData", "Data file (ignored for now)"),
            h4("Epicurves"),
            sliderTextInput("epicurve_unit", "Epicurve Unit", choices = "week"),
            sliderInput("incomplete_days", "Incomplete days", min = 0L, max = 21L, value = 7L, ticks = FALSE, step = 1L, post = " days"),
            h4("Reproduction number estimation"),
            radioButtons("use_epiparameter", "Serial interval", c("from epiparameter", "from custom input")),
            uiOutput("ui_si"),
            radioButtons("transmissibilityPkg", "Rt estimator" , c("EpiEstim", "EpiNow2", "i2extras", "R0")),
            sliderInput("r_estim_window", "Estimation window", min = 7L, max = 35L, value = 21L, ticks = FALSE, step = 1L, post = " days"),
            h4("Final size estimation"),
            radioButtons("contactdataPkg", "Contact data", c("conmat", "contactdata", "socialmixr")),
            hr(),
            actionButton("renderReport", "Render report", icon = icon("play"))
        ),
        dashboardBody(
            shinycssloaders::withSpinner(
              htmlOutput("report")
            )
        )
    ),
    server = function(input, output) {
        output$ui_si <- renderUI({
            if (is.null(input$use_epiparameter))
                return()
            switch(
                input$use_epiparameter,
                "from epiparameter" = selectInput("epiparameter_pathogen", "Pathogen", epiparameter::list_distributions("serial_interval")$pathogen_id),
                "from custom input" = tagList(
                    textInput("si_dist", "Serial interval distribution"),
                    textInput("si_mean", "Serial interval mean"),
                    textInput("si_sd", "Serial interval sd")
                )
            )
        })

        parms <- eventReactive(
            input$renderReport,
            list(
                epicurve_unit = input$epicurve_unit,
                incomplete_days = input$incomplete_days,
                r_estim_window = input$r_estim_window,
                local = FALSE,
                use_epiparameter = identical(input$use_epiparameter, "from epiparameter"),
                si_dist = input$si_dist,
                si_mean = input$si_mean,
                si_sd = input$si_sd,
                epiparameter_pathogen = input$epiparameter_pathogen
            )
        )

        output$report <- renderUI({
            includeHTML(
                rmarkdown::render(
                    "./reports/transmissibility.Rmd",
                    params = parms()
                )
            )
        })
    }
)
