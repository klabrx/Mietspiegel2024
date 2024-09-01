# Ergebnis.UI.R

ErgebnisUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Ergebnis",
    fluidRow(
      column(
        width = 12,
        h3("Zusammenfassung"),
        tableOutput(ns("summaryTable"))
      )
    ),
    fluidRow(
      column(
        width = 12,
        downloadButton(ns("downloadReport"), "Ergebnisbericht herunterladen")
      )
    )
  )
}
