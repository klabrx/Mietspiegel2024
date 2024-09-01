# app.R
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(stringr)  # Ensure stringr is loaded for str_pad

# Load Data
load("./data/adr_2024.RData")

# Source external scripts
source("./scripts/load_data.R")
source("./scripts/define_options.R")

# Source modules
source("./modules/Einleitung.UI.R")
source("./modules/Einleitung.server.R")
source("./modules/Wohnungsgroesse.UI.R")
source("./modules/Wohnungsgroesse.server.R")
source("./modules/Adresse.UI.R")
source("./modules/Adresse.server.R")
source("./modules/Ergebnis.UI.R")
source("./modules/Ergebnis.server.R")
source("./modules/Baujahr.UI.R")
source("./modules/Baujahr.server.R")

# JavaScript Path
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
    tabPanel(
      title = "Einleitung",
      EinleitungUI("einleitung")
    ),
    tabPanel(
      title = "Wohnungsgröße",
      WohnungsgroesseUI("wohnungsGroesse")
    ),
    tabPanel(
      title = "Adresse",
      AdresseUI("adresse")
    ),
    tabPanel(
      title = "Baujahr",
      BaujahrUI("baujahr")
    ),
    tabPanel(
      title = "Ergebnis",
      ErgebnisUI("ergebnis")
    )
  )
)

# Server Logic ----
server <- function(input, output, session) {
  report_data <- reactiveValues()

  EinleitungServer("einleitung", report_data)
  WohnungsgroesseServer("wohnungsGroesse", report_data)
  AdresseServer("adresse", report_data)
  BaujahrServer("baujahr", report_data)
  ErgebnisServer("ergebnis", report_data)
}

# Run the application
shinyApp(ui = ui, server = server)
