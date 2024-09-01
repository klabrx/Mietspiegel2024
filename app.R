# app.R
library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Load Data
load("./data/adr_2024.RData")

# Source external scripts
source("./scripts/load_data.R")
source("./scripts/define_options.R")

# Source section scripts
source("./sections/einleitung.R")
source("./sections/wohnungsgroesse.R")
source("./sections/adresse.R")
source("./sections/baujahr.R")
source("./sections/ergebnis.R")

# UI ----
ui <- fluidPage(
  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),
  div(id = "section_selector",
      uiOutput("section_ui") # Dynamic section UI
  ),
  fluidRow(
    div(id = "summary", uiOutput("sticky_summary"))
  ),
  fluidRow(
    uiOutput("section_content") # Dynamic section content
  )
)

# Server Logic ----
server <- function(input, output, session) {
  report_data <- reactiveValues()

  # Server logic for each section
  einleitung(input, output, session, report_data)
  wohnungsgroesse(input, output, session, report_data)
  adresse(input, output, session, report_data)
  # baujahr(input, output, session, report_data)
  # ergebnis(input, output, session, report_data)

  # Handle dynamic UI for section selection
  output$section_ui <- renderUI({
    selectInput("section", "Wählen Sie eine Sektion:",
                choices = c("Einleitung", "Wohnungsgröße", "Adresse", "Baujahr", "Ergebnis"))
  })

  # Display the content of the selected section
  output$section_content <- renderUI({
    switch(input$section,
           "Einleitung" = einleitungUI(),
           "Wohnungsgröße" = wohnungsgroesseUI(),
           "Adresse" = adresseUI(),
           "Baujahr" = baujahrUI(),
           "Ergebnis" = ergebnisUI()
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
