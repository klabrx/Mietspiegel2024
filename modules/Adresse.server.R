AdresseServer <- function(id, report_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define color palettes for map markers
    color_palette <- colorFactor(
      palette = c("#FF0000", "#0000FF", "#00FF00"), # Colors for A, B, C
      domain = c("A", "B", "C")
    )

    # Logic for the house number dropdown
    output$hausnummer_dropdown <- renderUI({
      req(input$strasse)
      gefilterte_adr <- sf_data %>%
        filter(STRASSE == input$strasse)
      hausnummern <- gefilterte_adr$HAUSNUMMER
      selectInput(ns("hausnummer"), "Hausnummer:", choices = c("", unique(hausnummern)))
    })

    # Logic for the Wohnlage description
    output$wohnlage <- renderText({
      req(input$strasse, input$hausnummer)
      gefilterte_adr <- sf_data %>%
        filter(STRASSE == input$strasse, HAUSNUMMER == input$hausnummer)
      if (nrow(gefilterte_adr) > 0) {
        lagekategorie <- gefilterte_adr$WL_2024
        adjustment <- wohnlage_adjustments[[lagekategorie]]
        percent_adjustment <- adjustment * 100
        paste(
          gefilterte_adr$STRASSE_HS,
          "liegt in Lagekategorie", lagekategorie,
          ", Abschlag", sprintf("%.2f", adjustment),
          "bzw", sprintf("%+.0f%%", percent_adjustment)
        )
      } else {
        return("Keine Daten gefunden")
      }
    })

    # Logic for the map output
    output$map <- renderLeaflet({
      req(input$strasse)

      gefilterte_adr <- sf_data %>%
        filter(STRASSE == input$strasse)

      map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addTiles() %>%
        addCircleMarkers(
          data = gefilterte_adr,
          lng = ~ st_coordinates(geometry)[, 1],
          lat = ~ st_coordinates(geometry)[, 2],
          fillColor = ~ color_palette(WL_2024),
          color = NA,
          radius = 5,
          stroke = FALSE,
          fillOpacity = 0.8
        )

      if (!is.null(input$hausnummer) && input$hausnummer != "" && nrow(gefilterte_adr) > 0) {
        selected_adr <- gefilterte_adr %>%
          filter(HAUSNUMMER == input$hausnummer)

        if (nrow(selected_adr) > 0) {
          map <- map %>%
            addCircleMarkers(
              data = selected_adr,
              lng = ~ st_coordinates(geometry)[, 1],
              lat = ~ st_coordinates(geometry)[, 2],
              fillColor = ~ color_palette(WL_2024),
              color = "#8B0000",  # Dark red border color
              weight = 4,
              radius = 10,
              stroke = TRUE,
              fillOpacity = 0.8,
              popup = ~ paste("Adresse:", STRASSE_HS)
            )
        }
      }

      map <- map %>%
        addLegend(
          position = "bottomright",
          title = "Wohnlage",
          colors = c("#FF0000", "#0000FF", "#00FF00"),
          labels = c("Wohnlage A", "Wohnlage B", "Wohnlage C"),
          opacity = 1
        )

      map
    })

    # Update the report data with selected address information
    observeEvent(input$hausnummer, {
      if (input$strasse != "" && input$hausnummer != "") {
        gefilterte_adr <- sf_data %>%
          filter(STRASSE == input$strasse, HAUSNUMMER == input$hausnummer)

        report_data$Adresse <- gefilterte_adr$STRASSE_HS
        report_data$Wohnlage <- gefilterte_adr$WL_2024
      }
    })
  })
}
