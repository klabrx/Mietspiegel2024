# sections/baujahr.R

baujahrUI <- function() {
  fluidRow(
    column(
      width = 4,
      selectInput("baujahr", "Altersklasse:", choices = c("", names(year_ranges))),
      textOutput("baujahr_percent")
    ),
    column(
      width = 8,
      h3("Informationen zur Altersklasse"),
      tableOutput("baujahr_table")
    )
  )
}

baujahr <- function(input, output, session, report_data) {
  # Update report_data with the selected Baujahr values
  observe({
    report_data$baujahr <- input$baujahr
    report_data$baujahr_percent <- year_ranges[[input$baujahr]]
  })

  output$baujahr_percent <- renderText({
    req(input$baujahr)
    percent_value <- year_ranges[[input$baujahr]] * 100
    paste("Zu-/Abschlag:", sprintf("%+.2f%%", percent_value))
  })

  output$baujahr_table <- renderTable({
    data.frame(
      "Altersklasse" = names(year_ranges),
      "Zu-/Abschlag in %" = sprintf("%+.2f%%", year_ranges * 100),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
}
