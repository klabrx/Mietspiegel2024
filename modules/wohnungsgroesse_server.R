# wohnungsgroesse_server.R

wohnungsGrosseServer <- function(id, updateTabColor) {
  moduleServer(id, function(input, output, session) {
    selected_values <- reactive({
      values_list[[as.numeric(input$groesse)]]
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
      paste(
        "Die Wohnungsgröße von ",
        names(dropdown_options)[as.numeric(input$groesse)],
        " ergibt folgende Basiswerte:"
      )
    })

    observeEvent(input$groesse, {
      if (input$groesse != "") {
        updateTabColor("tabWohnungsgröße", "btn-light-green")
      }
    })
  })
}
