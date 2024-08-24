BaujahrUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 4,
      selectInput(
        ns("baujahr"),
        "Altersklasse:",
        choices = c("", setNames(names(year_ranges), paste0(names(year_ranges), str_pad(display_labels, 10, side = "right")))),
        selected = ""
      ),
      textOutput(ns("baujahr_percent"))
    ),
    column(
      width = 8,
      h3("Baujahr, Altersklasse"),
      uiOutput(ns("description_Baujahr"))
    )
  )
}
