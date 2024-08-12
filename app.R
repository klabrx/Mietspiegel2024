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

# Define UI for the application
ui <- fluidPage(
  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),

  fluidRow(
    column(3,
           h3("Wohnungsgröße auswählen"),
           selectInput("groesse", "Auswahl der Wohnungsgröße:", choices = dropdown_options),
           textOutput("GROESSE"),
           textOutput("low_value"),
           uiOutput("med_value"),  # Emphasized Med value
           textOutput("hi_value"),
           br(),
           h3("Adresse auswählen"),
           selectInput("strasse", "Straße:", choices = c("", strassen)),
           uiOutput("hausnummer_dropdown")  # Dynamically created dropdown for house numbers
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

    # Filter Shapefile data based on the selected street
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)

    hausnummern <- create_hausnummer(gefilterte_adr)

    selectInput("hausnummer", "Hausnummer:", choices = c("", unique(hausnummern)))
  })

  # Render the Leaflet map based on the selected street and house number
  output$map <- renderLeaflet({
    req(input$strasse)

    # Define the exact color values to be used consistently
    fill_colors <- c("red" = "#FF0000", "blue" = "#0000FF", "green" = "#00FF00")
    border_colors <- c("red" = "#CC0000", "blue" = "#0000CC", "green" = "#009900")

    # Define color palettes using the exact color codes
    color_palette <- colorFactor(palette = fill_colors, domain = c("A", "B", "C"))
    border_palette <- colorFactor(palette = border_colors, domain = c("A", "B", "C"))

    # Filter Shapefile data based on the selected street
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)

    # Create the base map with all address points colored by WL_2024 without borders
    map <- leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = gefilterte_adr,
                       lng = ~st_coordinates(geometry)[,1],
                       lat = ~st_coordinates(geometry)[,2],
                       fillColor = ~color_palette(WL_2024),  # Fill color based on WL_2024
                       color = NA,  # No border for regular markers
                       radius = 5,
                       stroke = FALSE,  # Disable border
                       fillOpacity = 0.8)

    # If a house number is selected, add a highlighted marker for the specific address
    if (!is.null(input$hausnummer) && input$hausnummer != "") {
      # Construct FullAddress according to the format "Straße NummerZusatz"
      gefilterte_adr <- gefilterte_adr %>%
        mutate(FullAddress = paste(STRASSE, paste0(HNR, ifelse(is.na(HNRZ), "", HNRZ))))

      # Filter for the selected address
      selected_adr <- gefilterte_adr %>%
        filter(FullAddress == paste(input$strasse, input$hausnummer))

      # Check if selected_adr has valid coordinates
      if (nrow(selected_adr) > 0) {
        map <- map %>%
          addCircleMarkers(data = selected_adr,
                           lng = ~st_coordinates(geometry)[,1],
                           lat = ~st_coordinates(geometry)[,2],
                           fillColor = ~color_palette(WL_2024),  # Fill color based on WL_2024
                           color = ~border_palette(WL_2024),  # Border color based on WL_2024
                           weight = 4,  # Border width for the highlighted marker
                           radius = 10,  # Double the size of the regular markers
                           stroke = TRUE,  # Enable border
                           fillOpacity = 0.8,  # Adjust fill opacity
                           popup = ~paste("Adresse:", FullAddress))
      }
    }

    # Add a legend to the map explaining the color coding
    map <- map %>%
      addLegend(position = "bottomright",  # Position the legend below the zoom buttons
                title = "Legende",
                colors = fill_colors,  # Colors corresponding to WL_2024
                labels = c("Lage A", "Lage B", "Lage C"),  # Labels for each color
                opacity = 1)

    # Set the view based on the filtered data
    if (nrow(gefilterte_adr) > 0) {
      lat_range <- range(st_coordinates(gefilterte_adr$geometry)[,2], na.rm = TRUE)
      lng_range <- range(st_coordinates(gefilterte_adr$geometry)[,1], na.rm = TRUE)
      map %>%
        fitBounds(lng1 = lng_range[1], lat1 = lat_range[1],
                  lng2 = lng_range[2], lat2 = lat_range[2])
    } else {
      map
    }
  })









}

# Run the application
shinyApp(ui = ui, server = server)
