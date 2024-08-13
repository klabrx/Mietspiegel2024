library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(shinyjs)

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
  useShinyjs(),  # Include shinyjs

  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),

  # Custom CSS for tab colors
  tags$head(
    tags$style(HTML("
      .nav-pills .nav-link.red-tab {
        background-color: #f8d7da;
        color: #721c24;
      }
      .nav-pills .nav-link.green-tab {
        background-color: #d4edda;
        color: #155724;
      }
      .nav-pills .nav-link {
        margin-right: 5px;
        border-radius: 5px;
      }
    "))
  ),

  tabsetPanel(
    id = "mainTabs",
    tabPanel(
      "Wohnungsgröße",
      value = "Wohnungsgröße",
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

    tabPanel(
      "Adresse",
      value = "Adresse",
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
          leafletOutput("map", height = 300),
          textOutput("wohnlage")  # New output for Wohnlage
        ),
        column(
          8,  # Right column for explanations
          h3("Erläuterungen"),
          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
        )
      )
    ),

    tabPanel(
      "Baujahr",
      value = "Baujahr",
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

    tabPanel(
      "Sanierung",
      value = "Sanierung",
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

    tabPanel(
      "Sanitärausstattung",
      value = "Sanitärausstattung",
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

    tabPanel(
      "Beschaffenheit",
      value = "Beschaffenheit",
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

  # Initialize all tabs as red
  observe({
    addClass(selector = "a[data-value='Wohnungsgröße']", class = "red-tab")
    addClass(selector = "a[data-value='Adresse']", class = "red-tab")
    addClass(selector = "a[data-value='Baujahr']", class = "red-tab")
    addClass(selector = "a[data-value='Sanierung']", class = "red-tab")
    addClass(selector = "a[data-value='Sanitärausstattung']", class = "red-tab")
    addClass(selector = "a[data-value='Beschaffenheit']", class = "red-tab")
  })

  # Update tab colors based on interactions
  observeEvent(input$groesse, {
    if (input$groesse != "") {
      removeClass(selector = "a[data-value='Wohnungsgröße']", class = "red-tab")
      addClass(selector = "a[data-value='Wohnungsgröße']", class = "green-tab")
    }
  })

  observeEvent(input$strasse, {
    if (input$strasse != "") {
      removeClass(selector = "a[data-value='Adresse']", class = "red-tab")
      addClass(selector = "a[data-value='Adresse']", class = "green-tab")
    }
  })

  observeEvent(input$hausnummer, {
    if (input$hausnummer != "") {
      removeClass(selector = "a[data-value='Adresse']", class = "red-tab")
      addClass(selector = "a[data-value='Adresse']", class = "green-tab")
    }
  })

  observeEvent(input$baujahr, {
    if (input$baujahr != "") {
      removeClass(selector = "a[data-value='Baujahr']", class = "red-tab")
      addClass(selector = "a[data-value='Baujahr']", class = "green-tab")
    }
  })

  observeEvent(input$sanierung, {
    if (length(input$sanierung) > 0) {
      removeClass(selector = "a[data-value='Sanierung']", class = "red-tab")
      addClass(selector = "a[data-value='Sanierung']", class = "green-tab")
    }
  })

  observeEvent(input$sanitär, {
    if (length(input$sanitär) > 0) {
      removeClass(selector = "a[data-value='Sanitärausstattung']", class = "red-tab")
      addClass(selector = "a[data-value='Sanitärausstattung']", class = "green-tab")
    }
  })

  observeEvent(input$ausstattung, {
    if (length(input$ausstattung) > 0) {
      removeClass(selector = "a[data-value='Beschaffenheit']", class = "red-tab")
      addClass(selector = "a[data-value='Beschaffenheit']", class = "green-tab")
    }
  })

  # Render the remaining UI components as before
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

  # Reactive output for Wohnlage with adjustment
  output$wohnlage <- renderText({
    req(input$strasse, input$hausnummer)

    # Filter the data for the selected street and house number using uppercase field names
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse, HAUSNUMMER == input$hausnummer)

    if (nrow(gefilterte_adr) > 0) {
      # Extract the necessary information
      lagekategorie <- gefilterte_adr$WL_2024
      adjustment <- wohnlage_adjustments[[lagekategorie]]
      percent_adjustment <- adjustment * 100

      # Construct the output string
      output_string <- paste(
        gefilterte_adr$STRASSE_HS,
        "liegt in Lagekategorie", lagekategorie,
        ", Abschlag", sprintf("%.2f", adjustment),
        "bzw", sprintf("%+.0f%%", percent_adjustment)
      )

      return(output_string)
    } else {
      return("Keine Daten gefunden")
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
