library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Load Data
load("./data/adr_2024.RData")

# Source external scripts
source("./scripts/load_data.R")        # Load data script
source("./scripts/define_options.R")   # Define options script

# Source the modules
source("./modules/Einleitung.UI.R")
source("./modules/Einleitung.server.R")
source("./modules/Wohnungsgroesse.UI.R")
source("./modules/Wohnungsgroesse.server.R")
source("./modules/Ergebnis.UI.R")
source("./modules/Ergebnis.server.R")

# UI ----
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/custom_styles.css"),
    tags$script(src = "scripts/cookie_management.js")  # Externalized JavaScript
  ),
  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),
  tabsetPanel(
    id = "main_tabs",
    EinleitungUI("einleitung"),
    WohnungsgroesseUI("wohnungsGroesse"),
    ErgebnisUI("ergebnis")
  )
)

# Server ----
server <- function(input, output, session) {

  # Module servers
  EinleitungServer("einleitung")
  groesse_data <- WohnungsgroesseServer("wohnungsGroesse")
  ErgebnisServer("ergebnis", groesse_data)

}

# Run the application
shinyApp(ui = ui, server = server)
