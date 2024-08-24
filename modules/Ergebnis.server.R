ErgebnisServer <- function(id, groesse_data) {
  moduleServer(id, function(input, output, session) {

    summary_data <- reactive({
      list(
        Groesse = groesse_data()$label,
        Adresse = "N/A",  # Placeholder until the address module is added
        Baujahr = "N/A",  # Placeholder until the Baujahr module is added
        Sanierungen = "N/A",  # Placeholder until the Sanierung module is added
        Sanitaer = "N/A",  # Placeholder until the Sanitärausstattung module is added
        Ausstattung = "N/A"  # Placeholder until the Beschaffenheit module is added
      )
    })

    output$summaryOutput <- renderText({
      data <- summary_data()
      paste(
        "Wohnungsgröße:", data$Groesse, "\n",
        "Adresse:", data$Adresse, "\n",
        "Baujahr:", data$Baujahr, "\n",
        "Sanierungsmaßnahmen:", paste(data$Sanierungen, collapse = ", "), "\n",
        "Sanitärausstattung:", paste(data$Sanitaer, collapse = ", "), "\n",
        "Weitere Ausstattungsmerkmale:", paste(data$Ausstattung, collapse = ", ")
      )
    })
  })
}
