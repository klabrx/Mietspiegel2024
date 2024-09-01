# Load necessary libraries
library(sf)
library(dplyr)
library(stringr)

# Define paths for the RData and shapefile
rdata_path <- "./data/adr_2024.RData"
shapefile_path <- "./SHP/adr2024.shp"

# Load and process the shapefile
sf_data <- st_read(shapefile_path)

# Check and convert the CRS (Coordinate Reference System) if needed
if (st_crs(sf_data)$epsg != 4326) {
  sf_data <- st_transform(sf_data, crs = 4326)
}

# Extract relevant columns and add sorting logic
sf_data <- sf_data %>%
  arrange(STRASSE, as.numeric(substr(ADRESS_ID, 6, nchar(ADRESS_ID)))) %>%
  select(
    STRASSE,         # Street name
    HAUSNUMMER,      # House number
    STRASSE_HS,      # Combined street and house number for display
    WL_2024,         # Wohnlage category
    geometry,        # Geometry for mapping
    HNR,             # Numeric part of house number for sorting
    HNRZ             # Suffix part of house number for sorting
  )

# Save the processed sf_data to an RData file
save(sf_data, file = rdata_path)

cat("Shapefile processed, sorted, and saved as RData.")

