BaujahrServer <- function(id, report_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$baujahr, {
      if (!is.null(input$baujahr) && input$baujahr != "") {
        baujahr_label <- names(year_ranges)[input$baujahr]
        adjustment <- year_ranges[[baujahr_label]]
        report_data$baujahr <- list(
          label = baujahr_label,
          adjustment = adjustment,
          percent = display_labels[[baujahr_label]]
        )
      }
    })

    output$description_Baujahr <- renderUI({
      req(input$baujahr)
      selected_value <- report_data$baujahr
      description_text <- paste0(
        "Sie haben den Baujahresbereich \"", selected_value$label, "\" angegeben. ",
        "Dieser führt zu einem Zu-/Abschlag von ", selected_value$percent, ". <br><br>",
        "Die Baujahresklassen berücksichtigen den baulichen Zustand und die typischen Wohnungszuschnitte, ",
        "die für die angegebene Altersklasse zu erwarten sind."
      )
      HTML(description_text)
    })
  })
}
