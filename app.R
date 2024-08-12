library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Load data for dropdown options
RefTab_Groesse <- read.csv("./data/RefTab_Groesse.csv", sep = ";")
values_list <- lapply(seq_len(nrow(RefTab_Groesse)), function(i) {
  list(
    low = RefTab_Groesse$low[i],
    med = RefTab_Groesse$med[i],
    hi = RefTab_Groesse$hi[i]
  )
})

dropdown_options <- setNames(
  seq_len(nrow(RefTab_Groesse)),
  paste(RefTab_Groesse$von, "bis unter", RefTab_Groesse$bis_unter, "m²")
)

# Load Shapefile data
shapefile_path <- "./data/SHP/adr2024.shp"
sf_data <- st_read(shapefile_path)

# Check the CRS (Coordinate Reference System)
print(st_crs(sf_data))

# Convert to WGS84 (EPSG:4326) if not already in that CRS
if (st_crs(sf_data)$epsg != 4326) {
  sf_data <- st_transform(sf_data, crs = 4326)
}

# Extract unique street names and sort them alphabetically
strassen <- sort(unique(sf_data$STRASSE))

# Define year ranges and corresponding percentages
year_ranges <- c(
  "bis 1918" = 0.00,
  "1919 - 1945" = -0.07,
  "1946 - 1977" = -0.10,
  "1978 - 1984" = -0.05,
  "1985 - 1995" = -0.01,
  "1996 - 2004" = 0.06,
  "2005 - 2012" = 0.12,
  "2013 - 2018" = 0.19,
  "2019 - 2023" = 0.24
)

display_labels <- c(
  "bis 1918" = "0%",
  "1919 - 1945" = "-7%",
  "1946 - 1977" = "-10%",
  "1978 - 1984" = "-5%",
  "1985 - 1995" = "-1%",
  "1996 - 2004" = "+6%",
  "2005 - 2012" = "+12%",
  "2013 - 2018" = "+19%",
  "2019 - 2023" = "+24%"
)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),

  fluidRow(
    column(3,
           h3("Wohnungsgröße auswählen"),
           selectInput("groesse", "Auswahl der Wohnungsgröße:", choices = dropdown_options),
           textOutput("GROESSE"),
           textOutput("low_value"),
           uiOutput("med_value"),
           textOutput("hi_value"),
           br(),
           h3("Adresse auswählen"),
           selectizeInput("strasse", "Straße:", choices = c("", strassen),
                          options = list(
                            create = TRUE,
                            highlight = TRUE,
                            placeholder = "Wählen oder suchen Sie eine Straße")),
           uiOutput("hausnummer_dropdown"),
           br(),
           h3("Altersklasse auswählen"),
           selectInput("baujahr", "Altersklasse:", choices = c("", names(year_ranges))),
           textOutput("baujahr_percent")  # Display percentage based on year range
    ),
    column(9,
           h3("Karte"),
           leafletOutput("map", height = 400)
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Reactively get the selected size values
  selected_values <- reactive({
    values_list[[as.numeric(input$groesse)]]
  })

  output$low_value <- renderText({
    paste("Basiswert (min):", selected_values()$low, " EUR/m²")
  })

  output$med_value <- renderUI({
    strong(paste("Basismittelwert:", selected_values()$med, " EUR/m²"),
           style = "font-weight: bold; font-size: 20px; color: #FF5733;")
  })

  output$hi_value <- renderText({
    paste("Basiswert (max):", selected_values()$hi, " EUR/m²")
  })

  output$GROESSE <- renderText({
    paste("Die Wohnungsgröße von ",
          names(dropdown_options)[as.numeric(input$groesse)],
          " ergibt folgende Basiswerte:")
  })

  # Function to create a sortable Hausnummer field
  create_hausnummer <- function(data) {
    data %>%
      mutate(Hausnummer = paste0(as.character(HNR), ifelse(is.na(HNRZ), "", HNRZ))) %>%
      arrange(as.numeric(HNR), HNRZ) %>%
      pull(Hausnummer)
  }

  # Function to create the full address
  create_full_address <- function(data) {
    data %>%
      mutate(FullAddress = paste(STRASSE, as.character(HNR), ifelse(is.na(HNRZ), "", HNRZ))) %>%
      arrange(FullAddress) %>%
      pull(FullAddress)
  }

  # Dynamically generate the house number dropdown based on the selected street
  output$hausnummer_dropdown <- renderUI({
    req(input$strasse)
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)
    hausnummern <- create_hausnummer(gefilterte_adr)
    selectInput("hausnummer", "Hausnummer:", choices = c("", unique(hausnummern)))
  })

  # Update street input when only one option is available
  observe({
    req(input$strasse)
    unique_streets <- strassen[grep(paste0("^", input$strasse), strassen)]
    if (length(unique_streets) == 1 && input$strasse != unique_streets) {
      updateSelectizeInput(session, "strasse", selected = unique_streets)
    }
  })

  # Render the Leaflet map based on the selected street and house number
  output$map <- renderLeaflet({
    req(input$strasse)
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)
    color_palette <- colorFactor(c("red", "blue", "green"), domain = c("A", "B", "C"))
    border_palette <- colorFactor(c("darkred", "darkblue", "darkgreen"), domain = c("A", "B", "C"))
    map <- leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = gefilterte_adr,
                       lng = ~st_coordinates(geometry)[,1],
                       lat = ~st_coordinates(geometry)[,2],
                       fillColor = ~color_palette(WL_2024),
                       color = NA,
                       radius = 5,
                       stroke = FALSE,
                       fillOpacity = 0.8)
    if (!is.null(input$hausnummer) && input$hausnummer != "") {
      gefilterte_adr <- gefilterte_adr %>%
        mutate(FullAddress = paste(STRASSE, paste0(HNR, ifelse(is.na(HNRZ), "", HNRZ))))
      selected_adr <- gefilterte_adr %>%
        filter(FullAddress == paste(input$strasse, input$hausnummer))
      if (nrow(selected_adr) > 0) {
        map <- map %>%
          addCircleMarkers(data = selected_adr,
                           lng = ~st_coordinates(geometry)[,1],
                           lat = ~st_coordinates(geometry)[,2],
                           fillColor = ~color_palette(WL_2024),
                           color = ~border_palette(WL_2024),
                           weight = 4,
                           radius = 10,
                           stroke = TRUE,
                           fillOpacity = 0.8,
                           popup = ~paste("Adresse:", FullAddress))
      }
    }
    map %>%
      addLegend(position = "bottomright",
                title = "Legende",
                colors = c("red", "blue", "green"),
                labels = c("Lage A", "Lage B", "Lage C"),
                opacity = 1) %>%
      fitBounds(lng1 = min(st_coordinates(gefilterte_adr$geometry)[,1]),
                lat1 = min(st_coordinates(gefilterte_adr$geometry)[,2]),
                lng2 = max(st_coordinates(gefilterte_adr$geometry)[,1]),
                lat2 = max(st_coordinates(gefilterte_adr$geometry)[,2]))
  })

  # Display the percentage based on the selected year range
  output$baujahr_percent <- renderText({
    req(input$baujahr)
    if (input$baujahr == "") {
      return(NULL)
    }
    percent_value <- year_ranges[input$baujahr]
    display_value <- display_labels[input$baujahr]
    paste("Prozentwert für das Baujahr ", input$baujahr, " ist ", display_value, " (", sprintf("%.2f", percent_value), ")", sep = "")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
