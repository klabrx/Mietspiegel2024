# Baujahr.UI.R

BaujahrUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        selectInput(ns("baujahr"), "Altersklasse:",
                    choices = c("", names(year_ranges)))
      ),
      column(
        width = 8,
        DT::dataTableOutput(ns("baujahr_table"))  # Updated to use DT for rendering the table
      )
    )
  )
}
