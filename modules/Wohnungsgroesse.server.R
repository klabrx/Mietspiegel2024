WohnungsgroesseServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    selected_values <- reactive({
      req(input$groesse)
      index <- as.numeric(input$groesse)
      values_list[[index]]  # Ensure index is valid
    })

    output$low_value <- renderText({
      req(selected_values())
      paste("Basiswert (min):", selected_values()$low, " EUR/m²")
    })

    output$med_value <- renderUI({
      req(selected_values())
      strong(paste("Basismittelwert:", selected_values()$med, " EUR/m²"),
             style = "font-weight: bold; font-size: 20px; color: #FF5733;")
    })

    output$hi_value <- renderText({
      req(selected_values())
      paste("Basiswert (max):", selected_values()$hi, " EUR/m²")
    })

    output$GROESSE <- renderText({
      req(input$groesse)
      paste("Die Wohnungsgröße von ",
            names(dropdown_options)[as.numeric(input$groesse)],
            " ergibt folgende Basiswerte:")
    })

    # Return the selected values and label for use in other modules
    return(reactive({
      list(
        label = names(dropdown_options)[as.numeric(input$groesse)],
        values = selected_values()
      )
    }))
  })
}
