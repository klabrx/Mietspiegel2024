# Baujahr.server.R

BaujahrServer <- function(id, report_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render the table
    output$baujahr_table <- DT::renderDataTable({
      table_data <- data.frame(
        Altersklasse = names(year_ranges),
        `Zu-/Abschlag in %` = sprintf("%+.0f%%", year_ranges * 100),  # Adding signs to percentages with numeric formatting
        stringsAsFactors = FALSE
      )

      # Render the DataTable
      DT::datatable(
        table_data,
        colnames = c("Altersklasse", "Zu-/Abschlag in %"),  # Manually setting column names
        options = list(pageLength = 10, dom = 't', ordering = FALSE),
        rownames = FALSE
      )
    })

    # Update report_data when an Altersklasse is selected
    observeEvent(input$baujahr, {
      if (input$baujahr != "") {
        report_data$Baujahr <- input$baujahr
        report_data$Baujahr_Abschlag <- year_ranges[[input$baujahr]]
      }
    })
  })
}
