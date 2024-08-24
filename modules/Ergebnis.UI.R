# Ergebnis.UI.R

ErgebnisUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Ergebnis",
    fluidRow(
      column(
        width = 12,
        h3("Zusammenfassung"),
        verbatimTextOutput(ns("summaryOutput")),
        downloadButton(ns("downloadReport"), "Ergebnis herunterladen")
      )
    )
  )
}
