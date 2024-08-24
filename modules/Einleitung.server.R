# modules/Einleitung.server.R

EinleitungServer <- function(id, session) {
  moduleServer(id, function(input, output, session) {
    # Check if the user has previously accepted the terms
    observe({
      session$sendCustomMessage("getCookie", "terms_accepted")
    })

    observeEvent(input$cookie, {
      if (input$cookie == "TRUE") {
        updateTabsetPanel(session, "main_tabs", selected = "Ergebnis")
      }
    })

    # Handle the acceptance of the terms
    observeEvent(input$accept_terms, {
      if (input$accept_terms > 0) {
        if (input$skip_intro) {
          session$sendCustomMessage("setCookie", list(name = "terms_accepted", value = "TRUE"))
        }
        updateTabsetPanel(session, "main_tabs", selected = "Ergebnis")
      }
    })
  })
}
