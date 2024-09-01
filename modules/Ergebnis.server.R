# Ergebnis.server.R

ErgebnisServer <- function(id, report_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$summaryTable <- renderTable({
      # Initialize the table data with just Wohnungsgröße row
      table_data <- data.frame(
        Merkmal = "Wohnungsgröße",
        Eingabe = ifelse(!is.null(report_data$Groesse), report_data$Groesse, "Nicht angegeben"),
        Anteil = "100%",
        Ergebnis = ifelse(!is.null(report_data$Groesse_med), sprintf("%.2f EUR/m²", report_data$Groesse_med), "N/A"),
        stringsAsFactors = FALSE
      )

      table_data
    })

  })
}
