library(leaflet)
# library(geojsonio)
library(sf)
library(dplyr)  # For data manipulation

# Define the path to your shapefile
shapefile_path <- "data/SHP/adr2024.shp"




# Read the shapefile
sf_data <- st_read(shapefile_path)

# Check and transform CRS if needed
original_crs <- st_crs(sf_data)
if (original_crs$epsg != 4326) {
  sf_data <- st_transform(sf_data, crs = 4326) %>%
    filter(STRASSE == "Abteistraße")
}

bbox <- st_bbox(sf_data)

# # Filter data for a specific street, e.g., "Innstraße"
# filtered_data <- sf_data %>%
#   filter(STRASSE == "Innstraße")

# Extract coordinates for the filtered data
coords <- st_coordinates(sf_data)

# Define the UI
ui <- fluidPage(
  leafletOutput("map")
)

# Define the server logic
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(data = sf_data) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(
        lng = coords[, 1], lat = coords[, 2],
        popup = ~paste("Address:", STRASSE_HS, "<br>",
                       "PLZ:", PLZ, "<br>",
                       "City:", STADTTEIL),
        radius = 5,  # Adjust the size of the circle markers as needed
        color = "#FF5733",  # Customize marker color
        fillOpacity = 0.8  # Customize marker fill opacity
      ) %>%
      fitBounds(
        lng1 = bbox["xmin"], lat1 = bbox["ymin"],
        lng2 = bbox["xmax"], lat2 = bbox["ymax"]
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
