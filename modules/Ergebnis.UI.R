# Ergebnis.UI.R
ErgebnisUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        h3("Zusammenfassung"),
        uiOutput(ns("dynamicSummary")),
        downloadButton(ns("downloadReport"), "Download Ergebnisbericht")
      )
    )
  )
}
