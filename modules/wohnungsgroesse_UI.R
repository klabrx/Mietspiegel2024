# wohnungsgroesse_ui.R

wohnungsGrosseUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Wohnungsgröße auswählen"),
    selectInput(ns("groesse"), "Auswahl der Wohnungsgröße:", choices = c("", dropdown_options)),
    textOutput(ns("GROESSE")),
    textOutput(ns("low_value")),
    uiOutput(ns("med_value")),
    textOutput(ns("hi_value"))
  )
}
