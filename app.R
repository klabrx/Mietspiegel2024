# app.R
library(shiny)
source("./modules/Ergebnis.UI.R")
source("./modules/Ergebnis.server.R")

ui <- fluidPage(
  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Ergebnis", ErgebnisUI("ergebnis"))
  )
)

server <- function(input, output, session) {
  callModule(ErgebnisServer, "ergebnis")
}

shinyApp(ui = ui, server = server)
