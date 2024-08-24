WohnungsgroesseServer <- function(id, report_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # This ensures that UI components are properly namespaced

    # Update the selected Wohnungsgröße value
    selected_values <- reactive({
      values_list[[as.numeric(input$groesse)]]
    })

    # Update report_data with the selected Wohnungsgröße values
    observe({
      report_data$groesse <- names(dropdown_options)[as.numeric(input$groesse)]
      report_data$groesse_low <- selected_values()$low
      report_data$groesse_med <- selected_values()$med
      report_data$groesse_hi <- selected_values()$hi
    })

    output$low_value <- renderText({
      req(input$groesse)
      paste("Basiswert (min):", selected_values()$low, " EUR/m²")
    })

    output$med_value <- renderUI({
      req(input$groesse)
      strong(paste("Basismittelwert:", selected_values()$med, " EUR/m²"),
             style = "font-weight: bold; font-size: 20px; color: #FF5733;")
    })

    output$hi_value <- renderText({
      req(input$groesse)
      paste("Basiswert (max):", selected_values()$hi, " EUR/m²")
    })

    output$GROESSE <- renderText({
      req(input$groesse)
      paste("Die Wohnungsgröße von ", names(dropdown_options)[as.numeric(input$groesse)], " ergibt folgende Basiswerte:")
    })
  })
}
