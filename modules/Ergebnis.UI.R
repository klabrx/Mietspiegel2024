# Ergebnis.UI.R
ErgebnisUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Ergebnis",
    fluidRow(
      column(
        width = 12,
        h3("Zusammenfassung"),
        uiOutput(ns("summary_output")),  # Output for the summary text
        br(),  # Line break
        downloadButton(ns("download_report"), "Ergebnisbericht herunterladen")
      )
    )
  )
}
