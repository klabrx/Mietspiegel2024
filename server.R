# server.R

server <- function(input, output, session) {
  report_data <- reactiveValues()

  # Einleitung Server logic
  section1_Server(input, output, session)

  # Wohnungsgröße Server logic
  section2_Server(input, output, session)

  # Additional sections can be added similarly
}
