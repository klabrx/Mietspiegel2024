library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Load Data
load("./data/adr_2024.RData")

# Source external scripts
source("./scripts/load_data.R")        # Load data script
source("./scripts/define_options.R")   # Define options script

# Extract unique street names and sort them alphabetically
strassen <- sort(unique(sf_data$STRASSE))

# Define Wohnlage Adjustments
wohnlage_adjustments <- list(
  "A" = 0.00,
  "B" = -0.07,
  "C" = -0.10
)

# JavaScript code to manage cookies
jsCode <- "
Shiny.addCustomMessageHandler('setCookie', function(params) {
  document.cookie = params.name + '=' + params.value + ';path=/';
});

Shiny.addCustomMessageHandler('removeCookie', function(params) {
  document.cookie = params.name + '=;path=/;expires=Thu, 01 Jan 1970 00:00:00 UTC;';
});

function getCookie(name) {
  let cookieArr = document.cookie.split(';');
  for (let i = 0; i < cookieArr.length; i++) {
    let cookiePair = cookieArr[i].split('=');
    if (name == cookiePair[0].trim()) {
      return decodeURIComponent(cookiePair[1]);
    }
  }
  return null;
}

Shiny.addCustomMessageHandler('getCookie', function(name) {
  var cookieValue = getCookie(name);
  Shiny.setInputValue('cookie', cookieValue);
});
"

# UI ----
ui <- fluidPage(
  tags$head(
    tags$script(HTML(jsCode)),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/custom_styles.css")
  ),
  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      title = "Einleitung",
      h3("Willkommen zum Mietspiegel der Stadt Passau ab 2024"),
      p("Bevor Sie die Anwendung nutzen, lesen Sie bitte die folgenden Informationen sorgfältig durch."),
      p("Diese Anwendung dient zur Berechnung der ortsüblichen Vergleichsmiete basierend auf verschiedenen Kriterien wie Wohnungsgröße, Baujahr, Sanierung und Ausstattung."),
      p("Durch die Nutzung dieser Anwendung stimmen Sie der Verarbeitung der von Ihnen eingegebenen Daten zu."),
      p("Wir speichern keine persönlichen Daten. Ihre Eingaben werden ausschließlich zur Berechnung der Mietspiegelwerte verwendet und nicht gespeichert."),
      p("Bitte bestätigen Sie, dass Sie die Informationen gelesen und verstanden haben."),
      actionButton("accept_terms", "Ich habe die Informationen gelesen und stimme zu."),
      checkboxInput("skip_intro", "Diese Seite künftig überspringen", value = FALSE)
    ),
    tabPanel(
      title = "Wohnungsgröße",
      fluidRow(
        column(
          width = 4,
          selectInput("groesse", "Auswahl der Wohnungsgröße:", choices = c("", dropdown_options)),
          textOutput("GROESSE"),
          textOutput("low_value"),
          uiOutput("med_value"),
          textOutput("hi_value")
        ),
        column(
          width = 8,
          h3("Informationen zur Wohnungsgröße"),
          uiOutput("description_Wohnungsgroesse")
        )
      )
    ),
    tabPanel(
      title = "Adresse",
      fluidRow(
        column(
          width = 4,
          selectizeInput("strasse", "Straße:", choices = c("", strassen),
                         options = list(create = TRUE, highlight = TRUE, placeholder = "Wählen oder suchen Sie eine Straße")),
          uiOutput("hausnummer_dropdown"),
          leafletOutput("map", height = 300)
        ),
        column(
          width = 8,
          h3("Adresse, Wohnlage"),
          uiOutput("description_Adresse")
        )
      )
    ),
    tabPanel(
      title = "Baujahr",
      fluidRow(
        column(
          width = 4,
          selectInput("baujahr", "Altersklasse:",
                      choices = c("", setNames(names(year_ranges), paste(names(year_ranges), display_labels, sep = " "))),
                      selected = ""),
          textOutput("baujahr_percent")
        ),
        column(
          width = 8,
          h3("Baujahr, Altersklasse"),
          uiOutput("description_Baujahr")
        )
      )
    ),
    tabPanel(
      title = "Sanierung",
      fluidRow(
        column(
          width = 4,
          checkboxGroupInput("sanierung", "Wählen Sie durchgeführte Sanierungen aus:", choices = renovation_items),
          textOutput("sanierung_zuschlag")
        ),
        column(
          width = 8,
          p("Beschreibung der Sanierung.")
        )
      )
    ),
    tabPanel(
      title = "Sanitärausstattung",
      fluidRow(
        column(
          width = 4,
          checkboxGroupInput("sanitaer", "Wählen Sie aus:", choices = sanitaer_items),
          textOutput("sanitaer_zuschlag")
        ),
        column(
          width = 8,
          p("Beschreibung der Sanitärausstattung.")
        )
      )
    ),
    tabPanel(
      title = "Beschaffenheit",
      fluidRow(
        column(
          width = 4,
          checkboxGroupInput("ausstattung", "Wählen Sie aus:", choices = names(ausstattung_items)),
          textOutput("ausstattung_zuschlag")
        ),
        column(
          width = 8,
          p("Beschreibung der Beschaffenheit.")
        )
      )
    ),
    tabPanel(
      title = "Ergebnis",
      fluidRow(
        column(
          width = 4,
          h3("Zusammenfassung der Wohnungsgröße"),
          textOutput("summary_GROESSE"),
          textOutput("summary_low_value"),
          uiOutput("summary_med_value"),
          textOutput("summary_hi_value"),
          h3("Zusammenfassung der Adresse"),
          textOutput("zusammenfassung_wohnlage"),
          h3("Zusammenfassung des Baujahrs"),
          textOutput("zusammenfassung_baujahr"),
          h3("Zusammenfassung der Sanierung"),
          textOutput("zusammenfassung_sanierung"),
          h3("Zusammenfassung der Sanitärausstattung"),
          textOutput("zusammenfassung_sanitaer"),
          h3("Zusammenfassung der Beschaffenheit"),
          textOutput("zusammenfassung_beschaffenheit")
        ),
        column(
          width = 8,
          p("Dies ist die Ergebnisübersicht.")
        )
      )
    )
  )
)

# Define Server ----
server <- function(input, output, session) {

  # Check if the user has previously accepted the terms
  observe({
    session$sendCustomMessage("getCookie", "terms_accepted")
  })

  observeEvent(input$cookie, {
    if (input$cookie == "TRUE") {
      updateTabsetPanel(session, "main_tabs", selected = "Wohnungsgröße")
    }
  })

  # Handle the acceptance of the terms
  observeEvent(input$accept_terms, {
    if (input$accept_terms > 0) {
      if (input$skip_intro) {
        session$sendCustomMessage("setCookie", list(name = "terms_accepted", value = "TRUE"))
      }
      updateTabsetPanel(session, "main_tabs", selected = "Wohnungsgröße")
    }
  })

  # Define color palettes for map markers ----
  color_palette <- colorFactor(
    palette = marker_colors,
    domain = c("A", "B", "C")
  )

  border_palette <- colorFactor(
    palette = border_colors,
    domain = c("A", "B", "C")
  )

  # Wohnungsgröße Logic
  selected_values <- reactive({
    values_list[[as.numeric(input$groesse)]]
  })

  output$low_value <- renderText({
    req(input$groesse)
    paste("Basiswert (min):", selected_values()$low, " EUR/m²")
  })

  output$med_value <- renderUI({
    req(input$groesse)
    strong(paste("Basismittelwert:", selected_values()$med, " EUR/m²"),
           style = "font-weight: bold; font-size: 20px; color: #FF5733;")
  })

  output$hi_value <- renderText({
    req(input$groesse)
    paste("Basiswert (max):", selected_values()$hi, " EUR/m²")
  })

  output$GROESSE <- renderText({
    req(input$groesse)
    paste("Die Wohnungsgröße von ",
          names(dropdown_options)[as.numeric(input$groesse)],
          " ergibt folgende Basiswerte:")
  })

  # Summary Outputs for Wohnungsgröße
  output$summary_low_value <- renderText({
    req(input$groesse)
    paste("Basiswert (min):", selected_values()$low, " EUR/m²")
  })

  output$summary_med_value <- renderUI({
    req(input$groesse)
    strong(paste("Basismittelwert:", selected_values()$med, " EUR/m²"),
           style = "font-weight: bold; font-size: 20px; color: #FF5733;")
  })

  output$summary_hi_value <- renderText({
    req(input$groesse)
    paste("Basiswert (max):", selected_values()$hi, " EUR/m²")
  })

  output$summary_GROESSE <- renderText({
    req(input$groesse)
    paste("Die Wohnungsgröße von ",
          names(dropdown_options)[as.numeric(input$groesse)],
          " ergibt folgende Basiswerte:")
  })

  # Adresse Logic
  output$hausnummer_dropdown <- renderUI({
    req(input$strasse)
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)
    hausnummern <- gefilterte_adr$HAUSNUMMER
    selectInput("hausnummer", "Hausnummer:", choices = c("", unique(hausnummern)))
  })

  output$wohnlage <- renderText({
    req(input$strasse, input$hausnummer)
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse, HAUSNUMMER == input$hausnummer)
    if (nrow(gefilterte_adr) > 0) {
      lagekategorie <- gefilterte_adr$WL_2024
      adjustment <- wohnlage_adjustments[[lagekategorie]]
      percent_adjustment <- adjustment * 100
      paste(
        gefilterte_adr$STRASSE_HS,
        "liegt in Lagekategorie", lagekategorie,
        ", Abschlag", sprintf("%.2f", adjustment),
        "bzw", sprintf("%+.0f%%", percent_adjustment)
      )
    } else {
      return("Keine Daten gefunden")
    }
  })

  # Map Logic
  output$map <- renderLeaflet({
    req(input$strasse)

    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)

    # Base map with zoom controls disabled
    map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      addCircleMarkers(
        data = gefilterte_adr,
        lng = ~ st_coordinates(geometry)[, 1],
        lat = ~ st_coordinates(geometry)[, 2],
        fillColor = ~ color_palette(WL_2024),
        color = NA,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.8
      )

    # Check if a house number is selected
    if (!is.null(input$hausnummer) && input$hausnummer != "" && nrow(gefilterte_adr) > 0) {
      selected_adr <- gefilterte_adr %>%
        filter(HAUSNUMMER == input$hausnummer)

      if (nrow(selected_adr) > 0) {
        map <- map %>%
          addCircleMarkers(
            data = selected_adr,
            lng = ~ st_coordinates(geometry)[, 1],
            lat = ~ st_coordinates(geometry)[, 2],
            fillColor = ~ color_palette(WL_2024),
            color = ~ border_palette(WL_2024),  # Darker border color
            weight = 4,                         # Border thickness
            radius = 10,                        # Larger radius for selected address
            stroke = TRUE,
            fillOpacity = 0.8,
            popup = ~ paste("Adresse:", STRASSE_HS)  # Optional: Popup with address info
          )
      }
    }

    # Add a legend for Wohnlage A/B/C at the bottom right
    map <- map %>%
      addLegend(
        position = "bottomright",
        title = "Wohnlage",
        colors = c("#FF0000", "#0000FF", "#00FF00"),  # Match colors with marker_colors
        labels = c("Wohnlage A", "Wohnlage B", "Wohnlage C"),
        opacity = 1
      )

    map
  })

  # Baujahr Logic
  output$baujahr_percent <- renderText({
    req(input$baujahr)
    percent_value <- year_ranges[[input$baujahr]]
    display_value <- display_labels[[input$baujahr]]
    paste("Zu-/Abschlag für BJ ", input$baujahr, ": ", display_value, " (", sprintf("%.2f", percent_value), ")", sep = "")
  })

  # Sanierung Logic
  output$sanierung_zuschlag <- renderText({
    if ("Keine Sanierungsmaßnahme bekannt" %in% input$sanierung) {
      return("Sanierungszuschlag: 0%")
    } else if (length(input$sanierung) >= 3) {
      return("Sanierungszuschlag: +6%")
    } else {
      return("Sanierungszuschlag: 0%")
    }
  })

  # Sanitärausstattung Logic
  output$sanitaer_zuschlag <- renderText({
    if (length(input$sanitaer) >= 3) {
      return("Sanitärausstattungszuschlag: +6%")
    } else {
      return("Sanitärausstattungszuschlag: 0%")
    }
  })

  # Beschaffenheit Logic
  output$ausstattung_zuschlag <- renderText({
    total_percentage <- sum(unlist(ausstattung_items[input$ausstattung]))
    paste("Weitere Ausstattungszuschläge: ", sprintf("%.2f%%", total_percentage * 100), sep = "")
  })

  # DynText Wohnungsgröße ----
  output$description_Wohnungsgroesse <- renderUI({
    req(input$groesse)  # Ensure that a selection is made

    # Get the selected values
    selected_value <- values_list[[as.numeric(input$groesse)]]
    von <- RefTab_Groesse$von[as.numeric(input$groesse)]
    bis_unter <- RefTab_Groesse$bis_unter[as.numeric(input$groesse)]
    med_value <- selected_value$med
    lo_value <- selected_value$low
    hi_value <- selected_value$hi

    # Create the description text with dynamic values
    description_text <- paste0(
      "Sie haben eine Wohnungsgröße von ", von, " bis unter ", bis_unter, " m² ausgewählt. ",
      "Daraus ergibt sich für die weiteren Berechnungen ein Basiswert von ", med_value, " EUR/m², ",
      "von dem aus Zu- und Abschläge für Lage, Alter, Sanierung, Ausstattung und Beschaffenheit berechnet werden.",
      "<br><br>",
      "Dieser Basiswert entspricht einer ortsüblichen Vergleichsmiete für eine Wohnung der angegebenen Größenordnung in zentraler Lage und mit mittlerer Ausstattung/Beschaffenheit.",
      "<br><br>",
      "Die Spanne der Ortsüblichkeit reicht von ", lo_value, " EUR/m² bis ", hi_value, " EUR/m²."
    )

    # Render the text as HTML
    HTML(description_text)
  })

  # DynText Adresse ----
  output$description_Adresse <- renderUI({
    req(input$strasse, input$hausnummer)  # Ensure that both street and house number are selected

    # Filter the data for the selected address
    selected_adr <- sf_data %>%
      filter(STRASSE == input$strasse, HAUSNUMMER == input$hausnummer)

    if (nrow(selected_adr) > 0) {
      strasse_hs <- selected_adr$STRASSE_HS
      lagekategorie <- selected_adr$WL_2024

      # Define the description text for the selected location category
      lage_text <- switch(
        lagekategorie,
        "A" = "Zentrale Lage in der Nähe von Innenstadt, Universität und Klinikum, beste infrastrukturelle Ausstattung (ÖPNV, Einkaufsmöglichkeiten, ...), Zu-/Abschlag +- 0%",
        "B" = "Gürtellage im Anschluss an das Zentrum, gute Anbindung und Einkaufsmöglichkeiten, Abschlag -7%",
        "C" = "Außenlage mit reduzierter ÖPNV-Anbindung und ggf. längeren Wegen zu Einkaufsmöglichkeiten und urbaner Infrastruktur, Abschlag -10%"
      )

      # Create the description text with dynamic values
      description_text <- paste0(
        "Sie haben die Adresse ", strasse_hs, " angegeben, ",
        "diese entspricht der Lagekategorie ", lagekategorie, ".<br><br>",
        "Die Lagekategorien sind folgende:<br><br>",
        "<strong>A:</strong> Zentrale Lage in der Nähe von Innenstadt, Universität und Klinikum, beste infrastrukturelle Ausstattung (ÖPNV, Einkaufsmöglichkeiten, ...), Zu-/Abschlag +- 0%<br><br>",
        "<strong>B:</strong> Gürtellage im Anschluss an das Zentrum, gute Anbindung und Einkaufsmöglichkeiten, Abschlag -7%<br><br>",
        "<strong>C:</strong> Außenlage mit reduzierter ÖPNV-Anbindung und ggf. längeren Wegen zu Einkaufsmöglichkeiten und urbaner Infrastruktur, Abschlag -10%."
      )

      # Render the text as HTML
      HTML(description_text)
    } else {
      HTML("Bitte wählen Sie eine gültige Adresse aus.")
    }
  })

  # DynText Altersklasse ----
  output$description_Baujahr <- renderUI({
    req(input$baujahr)  # Ensure that a selection is made

    # Get the selected Altersklasse (which should directly match a key in year_ranges)
    altersklasse <- input$baujahr

    # Use the extracted year range to get the correct adjustment value
    adjustment <- year_ranges[[altersklasse]]
    percent_adjustment <- sprintf("%+.0f%%", adjustment * 100)

    # Create the description text with dynamic values
    description_text <- paste0(
      "Sie haben den Baujahresbereich \"", altersklasse, "\" angegeben. ",
      "Dieser führt zu einem Zu-/Abschlag von ", percent_adjustment, ". <br><br>",
      " Die Baujahresklassen berücksichtigen, welcher allgemeine Zustand einer ",
      "Wohnung aus den angegebenen Zeitraum erwartet werden kann. Insbesondere ",
      "der gesamte bauliche Aufwand, der im jeweiligen Zeitraum betrieben werden ",
      "konnte/musste, um zeitgemäßen Wohnraum herzustellen, schlägt sich hier ",
      "nieder, aber auch z.B. die für den jeweiligen Zeitraum typischen ",
      "Wohnungszuschnitte."
    )

    # Render the text as HTML
    HTML(description_text)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
