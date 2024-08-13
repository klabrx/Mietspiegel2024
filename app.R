library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Load Data
load("./data/adr_2024.RData")

# Extract unique street names and sort them alphabetically
strassen <- sort(unique(sf_data$STRASSE))

# Define Wohnlage Adjustments
wohnlage_adjustments <- list(
  "A" = 0.00,
  "B" = -0.07,
  "C" = -0.10
)

# Define UI
ui <- fluidPage(
  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      title = "Wohnungsgröße",
      fluidRow(
        column(
          width = 4,
          selectInput("groesse", "Auswahl der Wohnungsgröße:", choices = c("", dropdown_options)),
          textOutput("GROESSE"),
          textOutput("low_value"),
          uiOutput("med_value"),
          textOutput("hi_value")
        ),
        column(
          width = 8,
          p("Beschreibung der Wohnungsgröße.")
        )
      )
    ),
    tabPanel(
      title = "Adresse",
      fluidRow(
        column(
          width = 4,
          selectizeInput("strasse", "Straße:", choices = c("", strassen)),
          uiOutput("hausnummer_dropdown"),
          leafletOutput("map", height = 300),
          textOutput("wohnlage")  # Display the Wohnlage output below the map
        ),
        column(
          width = 8,
          p("Beschreibung der Adresse.")
        )
      )
    ),
    tabPanel(
      title = "Baujahr",
      fluidRow(
        column(
          width = 4,
          selectInput("baujahr", "Altersklasse:", choices = c("", names(year_ranges))),
          textOutput("baujahr_percent")
        ),
        column(
          width = 8,
          p("Beschreibung des Baujahrs.")
        )
      )
    ),
    tabPanel(
      title = "Sanierung",
      fluidRow(
        column(
          width = 4,
          checkboxGroupInput("sanierung", "Wählen Sie durchgeführte Sanierungen aus:", choices = renovation_items),
          textOutput("sanierung_zuschlag")
        ),
        column(
          width = 8,
          p("Beschreibung der Sanierung.")
        )
      )
    ),
    tabPanel(
      title = "Sanitärausstattung",
      fluidRow(
        column(
          width = 4,
          checkboxGroupInput("sanitär", "Wählen Sie aus:", choices = sanitär_items),
          textOutput("sanitär_zuschlag")
        ),
        column(
          width = 8,
          p("Beschreibung der Sanitärausstattung.")
        )
      )
    ),
    tabPanel(
      title = "Beschaffenheit",
      fluidRow(
        column(
          width = 4,
          checkboxGroupInput("ausstattung", "Wählen Sie aus:", choices = names(ausstattung_items)),
          textOutput("ausstattung_zuschlag")
        ),
        column(
          width = 8,
          p("Beschreibung der Beschaffenheit.")
        )
      )
    ),
    tabPanel(
      title = "Ergebnis",
      fluidRow(
        column(
          width = 4,
          h3("Zusammenfassung der Wohnungsgröße"),
          textOutput("summary_GROESSE"),
          textOutput("summary_low_value"),
          uiOutput("summary_med_value"),
          textOutput("summary_hi_value"),

          h3("Zusammenfassung der Adresse"),
          textOutput("zusammenfassung_wohnlage"),

          h3("Zusammenfassung des Baujahrs"),
          textOutput("zusammenfassung_baujahr"),

          h3("Zusammenfassung der Sanierung"),
          textOutput("zusammenfassung_sanierung"),

          h3("Zusammenfassung der Sanitärausstattung"),
          textOutput("zusammenfassung_sanitär"),

          h3("Zusammenfassung der Beschaffenheit"),
          textOutput("zusammenfassung_beschaffenheit")
        ),
        column(
          width = 8,
          p("Dies ist die Ergebnisübersicht.")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Define color palettes for map markers
  color_palette <- colorFactor(
    palette = marker_colors,  # Use the predefined marker colors
    domain = c("A", "B", "C") # The categories of the Wohnlage
  )

  border_palette <- colorFactor(
    palette = border_colors,  # Use the predefined border colors
    domain = c("A", "B", "C") # The categories of the Wohnlage
  )

  # Wohnungsgröße Logic
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
    paste("Die Wohnungsgröße von ",
          names(dropdown_options)[as.numeric(input$groesse)],
          " ergibt folgende Basiswerte:")
  })

  # Summary Outputs for Wohnungsgröße
  output$summary_low_value <- renderText({
    req(input$groesse)
    paste("Basiswert (min):", selected_values()$low, " EUR/m²")
  })

  output$summary_med_value <- renderUI({
    req(input$groesse)
    strong(paste("Basismittelwert:", selected_values()$med, " EUR/m²"),
           style = "font-weight: bold; font-size: 20px; color: #FF5733;")
  })

  output$summary_hi_value <- renderText({
    req(input$groesse)
    paste("Basiswert (max):", selected_values()$hi, " EUR/m²")
  })

  output$summary_GROESSE <- renderText({
    req(input$groesse)
    paste("Die Wohnungsgröße von ",
          names(dropdown_options)[as.numeric(input$groesse)],
          " ergibt folgende Basiswerte:")
  })

  # Adresse Logic
  output$hausnummer_dropdown <- renderUI({
    req(input$strasse)
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)
    hausnummern <- gefilterte_adr$HAUSNUMMER
    selectInput("hausnummer", "Hausnummer:", choices = c("", unique(hausnummern)))
  })

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

  # Map Logic
  output$map <- renderLeaflet({
    req(input$strasse)
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)
    leaflet() %>%
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
  })

  # Baujahr Logic
  output$baujahr_percent <- renderText({
    req(input$baujahr)
    percent_value <- year_ranges[[input$baujahr]]
    display_value <- display_labels[[input$baujahr]]
    paste("Zu-/Abschlag für BJ ", input$baujahr, ": ", display_value, " (", sprintf("%.2f", percent_value), ")", sep = "")
  })

  # Sanierung Logic
  output$sanierung_zuschlag <- renderText({
    if ("Keine Sanierungsmaßnahme bekannt" %in% input$sanierung) {
      return("Sanierungszuschlag: 0%")
    } else if (length(input$sanierung) >= 3) {
      return("Sanierungszuschlag: +6%")
    } else {
      return("Sanierungszuschlag: 0%")
    }
  })

  # Sanitärausstattung Logic
  output$sanitär_zuschlag <- renderText({
    if (length(input$sanitär) >= 3) {
      return("Sanitärausstattungszuschlag: +6%")
    } else {
      return("Sanitärausstattungszuschlag: 0%")
    }
  })

  # Beschaffenheit Logic
  output$ausstattung_zuschlag <- renderText({
    total_percentage <- sum(unlist(ausstattung_items[input$ausstattung]))
    paste("Weitere Ausstattungszuschläge: ", sprintf("%.2f%%", total_percentage * 100), sep = "")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
