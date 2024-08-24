# modules/Einleitung.UI.R

EinleitungUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Einleitung",
    h3("Willkommen zum Mietspiegel der Stadt Passau ab 2024"),
    p("Bevor Sie die Anwendung nutzen, lesen Sie bitte die folgenden Informationen sorgfältig durch."),
    p("Diese Anwendung dient zur Berechnung der ortsüblichen Vergleichsmiete basierend auf verschiedenen Kriterien wie Wohnungsgröße, Baujahr, Sanierung und Ausstattung."),
    p("Durch die Nutzung dieser Anwendung stimmen Sie der Verarbeitung der von Ihnen eingegebenen Daten zu."),
    p("Wir speichern keine persönlichen Daten. Ihre Eingaben werden ausschließlich zur Berechnung der Mietspiegelwerte verwendet und nicht gespeichert."),
    p("Bitte bestätigen Sie, dass Sie die Informationen gelesen und verstanden haben."),
    actionButton(ns("accept_terms"), "Ich habe die Informationen gelesen und stimme zu."),
    checkboxInput(ns("skip_intro"), "Diese Seite künftig überspringen", value = FALSE)
  )
}
