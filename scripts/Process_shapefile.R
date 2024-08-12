# Load necessary libraries
library(sf)

# Define paths for the RData and shapefile
rdata_path <- "./data/adr_2024.RData"
shapefile_path <- "./SHP/adr2024.shp"

# Load and process the shapefile
sf_data <- st_read(shapefile_path)

# Check and convert the CRS (Coordinate Reference System) if needed
if (st_crs(sf_data)$epsg != 4326) {
  sf_data <- st_transform(sf_data, crs = 4326)
}

# Save the processed sf_data to an RData file
save(sf_data, file = rdata_path)

cat("Shapefile processed and saved as RData.")
