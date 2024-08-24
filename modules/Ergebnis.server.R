# Ergebnis.server.R
ErgebnisServer <- function(input, output, session) {
  ns <- session$ns

  # Dummy data for the summary
  summary_data <- reactive({
    list(
      Groesse = 60,  # Example Wohnungsgröße
      Basiswert = 10, # Example Basiswert
      Wohnlagekategorie = "B", # Example Wohnlagekategorie
      AbschlagWohnlage = "-7%", # Example Abschlag for Wohnlage
      Baujahr = "1946 - 1977",  # Example Baujahr
      AbschlagBaujahr = "-10%", # Example Abschlag for Baujahr
      Sanierungen = c("Sanitärbereich erneuert", "Fenster-/Rahmenerneuerung"),  # Example Sanierung selections
      ZuschlagSanierung = "6%", # Example Zuschlag for Sanierung
      Sanitaer = c("zweites WC/Gäste-WC", "Handtuchheizkörper"),  # Example Sanitärausstattung selections
      ZuschlagSanitaer = "6%", # Example Zuschlag for Sanitärausstattung
      Ausstattung = c("Einbauküche", "Terrasse"),  # Example Ausstattung selections
      ZuschlagAusstattung = "10%", # Example Zuschlag for Ausstattung
      GesamtAnpassung = "5%" # Example Gesamtanpassung
    )
  })

  # Generate summary text with proper formatting
  output$summary_output <- renderUI({
    data <- summary_data()
    HTML(paste0(
      "Angegebene Wohnungsgröße: ", data$Groesse, " m², Basiswert: ", data$Basiswert, " EUR/m².<br><br>",
      "Wohnlagekategorie: ", data$Wohnlagekategorie, ", Abschlag: ", data$AbschlagWohnlage, ".<br><br>",
      "Baujahr: ", data$Baujahr, ", Abschlag: ", data$AbschlagBaujahr, ".<br><br>",
      "Sanierungsmaßnahmen: ", paste(data$Sanierungen, collapse = ", "), ", Zuschlag: ", data$ZuschlagSanierung, ".<br><br>",
      "Sanitärausstattung: ", paste(data$Sanitaer, collapse = ", "), ", Zuschlag: ", data$ZuschlagSanitaer, ".<br><br>",
      "Ausstattung: ", paste(data$Ausstattung, collapse = ", "), ", Zuschlag: ", data$ZuschlagAusstattung, ".<br><br>",
      "Insgesamt ergibt sich ein Anpassungsfaktor von ", data$GesamtAnpassung, "."
    ))
  })

  # Download button logic (dummy implementation for now)
  output$download_report <- downloadHandler(
    filename = function() { "Ergebnisbericht.html" },
    content = function(file) {
      # Implement the report generation logic here
      writeLines("<html><body><h1>Ergebnisbericht</h1></body></html>", file)
    }
  )
}
