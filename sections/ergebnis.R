# sections/ergebnis.R

ergebnisUI <- function() {
  fluidRow(
    column(
      width = 12,
      h3("Zusammenfassung und Ergebnis"),
      tableOutput("ergebnis_summaryTable"),
      downloadButton("download_report", "Ergebnisbericht herunterladen")
    )
  )
}

ergebnis <- function(input, output, session, report_data) {
  # Render the summary table
  output$ergebnis_summaryTable <- renderTable({
    data <- list(
      Merkmal = c(
        "Wohnungsgröße",
        "Lage",
        "Alter",
        "Modernisierung",
        "Sanitärausstattung",
        "Sonstige Ausstattungsmerkmale",
        "Summe der Zu-/Abschläge",
        "Ortsübliche Vergleichsmiete"
      ),
      Eingabe = c(
        report_data$groesse %||% "Nicht angegeben",
        report_data$Adresse %||% "Nicht angegeben",
        report_data$baujahr %||% "Nicht angegeben",
        "To be implemented",
        "To be implemented",
        "To be implemented",
        "Berechnung folgt",
        "Berechnung folgt"
      ),
      Anteil = c(
        "100%",
        sprintf("%+.2f%%", (wohnlage_adjustments[[report_data$Wohnlage]] * 100) %||% 0),
        sprintf("%+.2f%%", (year_ranges[[report_data$baujahr]] * 100) %||% 0),
        "To be implemented",
        "To be implemented",
        "To be implemented",
        "Gesamt",
        "Ergebnis"
      ),
      Ergebnis = c(
        sprintf("%.2f EUR/m²", report_data$groesse_med %||% 0),
        "",
        "",
        "",
        "",
        "",
        sprintf("%.2f EUR/m²", report_data$groesse_med * (1 + ((wohnlage_adjustments[[report_data$Wohnlage]] %||% 0) + (year_ranges[[report_data$baujahr]] %||% 0))) %||% 0),
        sprintf("%.2f EUR/m²", report_data$groesse_med * (1 + ((wohnlage_adjustments[[report_data$Wohnlage]] %||% 0) + (year_ranges[[report_data$baujahr]] %||% 0))) %||% 0)
      )
    )
    as.data.frame(data, stringsAsFactors = FALSE)
  })

  # Download report functionality
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Ergebnisbericht", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # Render the report with current report_data
      rmarkdown::render(
        "reports/report.Rmd",
        output_file = file,
        params = list(
          Groesse = report_data$groesse,
          Adresse = report_data$Adresse,
          Baujahr = report_data$baujahr,
          GroesseMed = report_data$groesse_med,
          WohnlageAdjustment = wohnlage_adjustments[[report_data$Wohnlage]],
          BaujahrAdjustment = year_ranges[[report_data$baujahr]]
        )
      )
    }
  )
}
