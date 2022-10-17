library(shinydashboard)

shinyApp(
    ui = dashboardPage(
        dashboardHeader(title = "Automated reports"),
        dashboardSidebar(
            fileInput("inputData", "Data file (ignored for now)"),
            radioButtons("transmissibilityPkg", "Reproduction number estimation", c("EpiEstim", "EpiNow2", "R0")),
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
