# Wohnungsgroesse.UI.R

WohnungsgroesseUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Wohnungsgröße",
    fluidRow(
      column(
        width = 4,
        selectInput(ns("groesse"), "Auswahl der Wohnungsgröße:", choices = c("", dropdown_options)),
        textOutput(ns("GROESSE")),
        textOutput(ns("low_value")),
        uiOutput(ns("med_value")),
        textOutput(ns("hi_value"))
      ),
      column(
        width = 8,
        h3("Informationen zur Wohnungsgröße"),
        uiOutput(ns("description_Wohnungsgroesse"))
      )
    )
  )
}
