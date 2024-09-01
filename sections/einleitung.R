# sections/einleitung.R

einleitungUI <- function() {
  fluidRow(
    column(
      width = 12,
      h3("Einleitung"),
      p("Willkommen zum qualifizierten Mietspiegel der Stadt Passau ab 2024."),
      p("Bitte lesen Sie die folgenden Informationen sorgfältig durch."),
      checkboxInput("accept_terms", "Ich habe die Informationen gelesen und stimme zu."),
      actionButton("proceed", "Weiter")
    )
  )
}

einleitung <- function(input, output, session, report_data) {
  observeEvent(input$proceed, {
    if (input$accept_terms) {
      updateSelectInput(session, "section", selected = "Wohnungsgröße")
    } else {
      showModal(modalDialog(
        title = "Einwilligung erforderlich",
        "Bitte stimmen Sie den Bedingungen zu, bevor Sie fortfahren.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
}
