# ui.R
fluidPage(
  tags$head(
    # tags$script(src = js_path),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/custom_styles.css")
  ),
  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      title = "Einleitung",
      source("./sections/einleitung.R")$value
    ),
    tabPanel(
      title = "Wohnungsgröße",
      source("./sections/wohnungsgroesse.R")$value
    ),
    tabPanel(
      title = "Adresse",
      source("./sections/adresse.R")$value
    ),
    tabPanel(
      title = "Ergebnis",
      source("./sections/ergebnis.R")$value
    )
  )
)
