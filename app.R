library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Source external scripts
source("./scripts/load_data.R")
source("./scripts/define_options.R")

# Define the create_hausnummer function
create_hausnummer <- function(data) {
  data %>%
    mutate(Hausnummer = paste0(as.character(HNR), ifelse(is.na(HNRZ), "", HNRZ))) %>%
    arrange(as.numeric(HNR), HNRZ) %>%
    pull(Hausnummer)
}

# Define UI
ui <- fluidPage(
  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),

  tabsetPanel(
    # Wohnungsgröße Tab
    tabPanel(
      "Wohnungsgröße",
      fluidRow(
        column(
          4,  # Left column for inputs
          h3("Wohnungsgröße auswählen"),
          selectInput("groesse", "Auswahl der Wohnungsgröße:", choices = c("", dropdown_options)),
          textOutput("GROESSE"),
          textOutput("low_value"),
          uiOutput("med_value"),
          textOutput("hi_value")
        ),
        column(
          8,  # Right column for explanations
          h3("Erläuterungen"),
          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
        )
      )
    ),

    # Adresse Tab with Nested Tabset
    tabPanel(
      "Adresse",
      fluidRow(
        column(
          4,  # Left column for inputs
          h3("Adresse auswählen"),
          fluidRow(
            column(
              8,  # Street selection
              selectizeInput("strasse", "Straße:", choices = c("", strassen),
                             options = list(create = TRUE, highlight = TRUE, placeholder = "Wählen oder suchen Sie eine Straße"))
            ),
            column(
              4,  # House number selection
              uiOutput("hausnummer_dropdown")
            )
          ),
          leafletOutput("map", height = 300)
        ),
        column(
          8,  # Right column for explanations
          h3("Erläuterungen"),
          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
        )
      )
    ),

    # Baujahr Tab
    tabPanel(
      "Baujahr",
      fluidRow(
        column(
          4,  # Left column for inputs
          h3("Altersklasse auswählen"),
          selectInput("baujahr", "Altersklasse:", choices = c("", names(year_ranges))),
          textOutput("baujahr_percent")
        ),
        column(
          8,  # Right column for explanations
          h3("Erläuterungen"),
          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
        )
      )
    ),

    # Sanierung Tab
    tabPanel(
      "Sanierung",
      fluidRow(
        column(
          4,  # Left column for inputs
          h3("Sanierung auswählen"),
          checkboxGroupInput("sanierung", "Wählen Sie durchgeführte Sanierungen aus:", choices = renovation_items),
          textOutput("sanierung_zuschlag")
        ),
        column(
          8,  # Right column for explanations
          h3("Erläuterungen"),
          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
        )
      )
    ),

    # Sanitärausstattung Tab
    tabPanel(
      "Sanitärausstattung",
      fluidRow(
        column(
          4,  # Left column for inputs
          h3("Sanitärausstattung auswählen"),
          checkboxGroupInput("sanitär", "Wählen Sie aus:", choices = sanitär_items),
          textOutput("sanitär_zuschlag")
        ),
        column(
          8,  # Right column for explanations
          h3("Erläuterungen"),
          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
        )
      )
    ),

    # Beschaffenheit Tab
    tabPanel(
      "Beschaffenheit",
      fluidRow(
        column(
          4,  # Left column for inputs
          h3("Weitere Ausstattungsmerkmale"),
          checkboxGroupInput("ausstattung", "Wählen Sie aus:", choices = names(ausstattung_items)),
          textOutput("ausstattung_zuschlag")
        ),
        column(
          8,  # Right column for explanations
          h3("Erläuterungen"),
          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Reactively get the selected size values
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
    paste("Die Wohnungsgröße von ", names(dropdown_options)[as.numeric(input$groesse)], " ergibt folgende Basiswerte:")
  })

  # Dynamically generate the house number dropdown based on the selected street
  output$hausnummer_dropdown <- renderUI({
    req(input$strasse)
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)
    hausnummern <- create_hausnummer(gefilterte_adr)
    selectInput("hausnummer", "Hausnummer:", choices = c("", unique(hausnummern)))
  })

  # Render the Leaflet map based on the selected street and house number
  output$map <- renderLeaflet({
    req(input$strasse)
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)
    color_palette <- colorFactor(marker_colors, domain = c("A", "B", "C"))
    border_palette <- colorFactor(border_colors, domain = c("A", "B", "C"))
    map <- leaflet() %>%
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
    if (!is.null(input$hausnummer) && input$hausnummer != "") {
      gefilterte_adr <- gefilterte_adr %>%
        mutate(FullAddress = paste(STRASSE, paste0(HNR, ifelse(is.na(HNRZ), "", HNRZ))))
      selected_adr <- gefilterte_adr %>%
        filter(FullAddress == paste(input$strasse, input$hausnummer))
      if (nrow(selected_adr) > 0) {
        map <- map %>%
          addCircleMarkers(
            data = selected_adr,
            lng = ~ st_coordinates(geometry)[, 1],
            lat = ~ st_coordinates(geometry)[, 2],
            fillColor = ~ color_palette(WL_2024),
            color = ~ border_palette(WL_2024),
            weight = 4,
            radius = 10,
            stroke = TRUE,
            fillOpacity = 0.8,
            popup = ~ paste("Adresse:", FullAddress)
          )
      }
    }
    map %>%
      addLegend(
        position = "bottomright",
        title = "Legende",
        colors = marker_colors,
        labels = c("Lage A", "Lage B", "Lage C"),
        opacity = 1
      ) %>%
      fitBounds(
        lng1 = min(st_coordinates(gefilterte_adr$geometry)[, 1]),
        lat1 = min(st_coordinates(gefilterte_adr$geometry)[, 2]),
        lng2 = max(st_coordinates(gefilterte_adr$geometry)[, 1]),
        lat2 = max(st_coordinates(gefilterte_adr$geometry)[, 2])
      )
  })

  # Render the report based on user inputs
  output$report <- renderUI({
    req(input$groesse, input$strasse, input$hausnummer, input$baujahr)

    report_path <- tempfile(fileext = ".html")
    quarto::quarto_render(
      input = "./reports/report.qmd",
      output_file = report_path,
      execute_params = list(
        groesse = input$groesse,
        strasse = input$strasse,
        hausnummer = input$hausnummer,
        baujahr = input$baujahr,
        sanierung = input$sanierung,
        sanitär = input$sanitär,
        ausstattung = input$ausstattung
      )
    )

    tags$iframe(src = report_path, width = "100%", height = "800px")
  })

  # Handle other selections and updates
  observeEvent(input$baujahr, {
    if (input$baujahr != "") {
      updateActionButton(session, "baujahr", label = "Baujahr (completed)")
    }
  })

  observeEvent(input$sanierung, {
    if ("Keine Sanierungsmaßnahme bekannt" %in% input$sanierung || length(input$sanierung) >= 3) {
      updateActionButton(session, "sanierung", label = "Sanierung (completed)")
    }
  })

  observeEvent(input$sanitär, {
    if (length(input$sanitär) >= 3) {
      updateActionButton(session, "sanitär", label = "Sanitärausstattung (completed)")
    }
  })

  observeEvent(input$ausstattung, {
    if (length(input$ausstattung) > 0) {
      updateActionButton(session, "ausstattung", label = "Beschaffenheit (completed)")
    }
  })

  # Display the percentage based on the selected year range
  output$baujahr_percent <- renderText({
    req(input$baujahr)
    if (input$baujahr == "") {
      return(NULL)
    }
    percent_value <- year_ranges[input$baujahr]
    display_value <- display_labels[input$baujahr]
    paste("Zu-/Abschlag für BJ ", input$baujahr, ": ", display_value, " (", sprintf("%.2f", percent_value), ")", sep = "")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
