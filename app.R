library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Load Data
load("./data/adr_2024.RData")

# Source external scripts
source("./scripts/load_data.R")
source("./scripts/define_options.R")

# Source all module scripts
source("./modules/Einleitung.UI.R")
source("./modules/Einleitung.server.R")
source("./modules/Wohnungsgroesse.UI.R")
source("./modules/Wohnungsgroesse.server.R")
source("./modules/Adresse.UI.R")
source("./modules/Adresse.server.R")
source("./modules/Ergebnis.UI.R")
source("./modules/Ergebnis.server.R")

# Extract unique street names and sort them alphabetically
strassen <- sort(unique(sf_data$STRASSE))

# Define Wohnlage Adjustments
wohnlage_adjustments <- list(
  "A" = 0.00,
  "B" = -0.07,
  "C" = -0.10
)

# Load JavaScript
js_path <- "scripts/custom.js"

# UI ----
ui <- fluidPage(
  tags$head(
    tags$script(src = js_path),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/custom_styles.css")
  ),
  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),
  tabsetPanel(
    id = "main_tabs",
    EinleitungUI("einleitung"),
    WohnungsgroesseUI("wohnungsGroesse"),
    AdresseUI("adresse"),
    ErgebnisUI("ergebnis")
  )
)

# Server Logic ----
server <- function(input, output, session) {
  # Reactive value storage
  report_data <- reactiveValues()

  # Handle the Einleitung module
  EinleitungServer("einleitung", report_data)

  # Handle the Wohnungsgröße module
  WohnungsgroesseServer("wohnungsGroesse", report_data)

  # Handle the Adresse module
  AdresseServer("adresse", report_data)

  # Handle the Ergebnis module
  ErgebnisServer("ergebnis", report_data)
}

# Run the application
shinyApp(ui = ui, server = server)
