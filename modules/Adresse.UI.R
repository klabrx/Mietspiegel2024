AdresseUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Adresse",
    fluidRow(
      column(
        width = 4,
        selectizeInput(ns("strasse"), "Straße:", choices = c("", strassen),
                       options = list(create = TRUE, highlight = TRUE, placeholder = "Wählen oder suchen Sie eine Straße")),
        uiOutput(ns("hausnummer_dropdown")),
        leafletOutput(ns("map"), height = 300)
      ),
      column(
        width = 8,
        h3("Adresse, Wohnlage"),
        uiOutput(ns("description_Adresse"))
      )
    )
  )
}
