# sections/wohnungsgroesse.R

wohnungsgroesseUI <- function() {
  fluidRow(
    column(
      width = 4,
      selectInput("groesse", "Auswahl der Wohnungsgröße:", choices = c("", dropdown_options)),
      textOutput("low_value"),
      uiOutput("med_value"),
      textOutput("hi_value")
    ),
    column(
      width = 8,
      h3("Informationen zur Wohnungsgröße"),
      uiOutput("description_Wohnungsgroesse")
    )
  )
}

wohnungsgroesse <- function(input, output, session, report_data) {
  selected_values <- reactive({
    values_list[[as.numeric(input$groesse)]]
  })

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

  output$description_Wohnungsgroesse <- renderUI({
    req(input$groesse)
    paste("Die Wohnungsgröße von ", names(dropdown_options)[as.numeric(input$groesse)], " ergibt folgende Basiswerte:")
  })
}
