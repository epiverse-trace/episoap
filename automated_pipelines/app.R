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
            radioButtons("transmissibilityPkg", "R package" , c("EpiEstim", "EpiNow2", "R0")),
            sliderInput("r_estim_window", "Estimation window", min = 7L, max = 35L, value = 21L, ticks = FALSE, step = 1L, post = " days"),
            h4("Final size estimation"),
            radioButtons("contactdataPkg", "Contact data", c("conmat", "contactdata", "socialmixr"))
            #                radioButtons("outputFormat", "Output format", c("HTML", "PDF"), inline = TRUE),
            #                downloadButton("report", "Download report")
        ),
        dashboardBody(
            htmlOutput("report")
        )
    ),
    server = function(input, output) {
        output$report <- renderUI({
            params <- list(
                epicurve_unit = "week",
                incomplete_days = 7L,
                r_estim_window = 21L,
                si_mean = 4.2,
                si_sd = 4.9
            )
            includeHTML(
                rmarkdown::render("../reports/transmissibility.Rmd")
            )
        })
    }
)
