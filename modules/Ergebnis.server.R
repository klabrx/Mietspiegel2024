# Ergebnis.server.R
ErgebnisServer <- function(id, report_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dynamically generate the summary based on report_data
    output$dynamicSummary <- renderUI({
      items <- names(report_data)

      # Create UI elements for each item in report_data
      summary_ui <- lapply(items, function(item) {
        value <- report_data[[item]]

        # Format value for display
        if (is.numeric(value)) {
          value <- sprintf("%.2f", value)
        } else if (is.list(value) || is.vector(value)) {
          value <- paste(value, collapse = ", ")
        }

        # Return a wellPanel containing the item name and value
        wellPanel(
          tags$strong(paste(item, ":")),
          p(value)
        )
      })

      # Return the list of UI elements
      do.call(tagList, summary_ui)
    })

    # Generate the downloadable report
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste("Ergebnisbericht", Sys.Date(), ".html", sep = "_")
      },
      content = function(file) {
        rmarkdown::render(
          input = "reports/report.Rmd",
          output_file = file,
          params = as.list(report_data),
          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}
