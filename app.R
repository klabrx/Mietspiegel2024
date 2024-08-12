library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Source external scripts
source("./scripts/load_data.R")
source("./scripts/define_options.R")
source("./modules/wohnungsgroesse_module.R")  # Load the Wohnungsgröße module

# Define UI
ui <- fluidPage(
  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),

  # Include custom styles
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles/custom_styles.css")),

  fluidRow(
    column(
      4,
      div(
        class = "custom-accordion",

        # Wohnungsgröße accordion
        div(
          class = "card",
          div(
            class = "card-header",
            actionButton("tabWohnungsgröße", "Wohnungsgröße", class = "btn btn-light-red", `data-toggle` = "collapse", `data-target` = "#collapseWohnungsgröße")
          ),
          div(
            id = "collapseWohnungsgröße", class = "collapse",
            div(
              class = "card-body",
              wohnungsgroesseUI("wohnungsGrosse") # Call the UI part of the module
            )
          )
        ),

        # Adresse accordion
        div(
          class = "card",
          div(
            class = "card-header",
            actionButton("tabAdresse", "Adresse", class = "btn btn-light-red", `data-toggle` = "collapse", `data-target` = "#collapseAdresse")
          ),
          div(
            id = "collapseAdresse", class = "collapse",
            div(
              class = "card-body",
              h3("Adresse auswählen"),
              selectizeInput("strasse", "Straße:",
                             choices = c("", strassen),
                             options = list(
                               create = TRUE,
                               highlight = TRUE,
                               placeholder = "Wählen oder suchen Sie eine Straße"
                             )),
              uiOutput("hausnummer_dropdown")
            )
          )
        ),

        # Baujahr accordion
        div(
          class = "card",
          div(
            class = "card-header",
            actionButton("tabBaujahr", "Baujahr", class = "btn btn-light-red", `data-toggle` = "collapse", `data-target` = "#collapseBaujahr")
          ),
          div(
            id = "collapseBaujahr", class = "collapse",
            div(
              class = "card-body",
              h3("Altersklasse auswählen"),
              selectInput("baujahr", "Altersklasse:", choices = c("", names(year_ranges))),
              textOutput("baujahr_percent")
            )
          )
        ),

        # Sanierung accordion
        div(
          class = "card",
          div(
            class = "card-header",
            actionButton("tabSanierung", "Sanierung", class = "btn btn-light-red", `data-toggle` = "collapse", `data-target` = "#collapseSanierung")
          ),
          div(
            id = "collapseSanierung", class = "collapse",
            div(
              class = "card-body",
              h3("Sanierung auswählen"),
              checkboxGroupInput("sanierung", "Wählen Sie durchgeführte Sanierungen aus:", choices = renovation_items),
              textOutput("sanierung_zuschlag")
            )
          )
        ),

        # Ausstattung accordion
        div(
          class = "card",
          div(
            class = "card-header",
            actionButton("tabAusstattung", "Ausstattung, Beschaffenheit", class = "btn btn-light-red", `data-toggle` = "collapse", `data-target` = "#collapseAusstattung")
          ),
          div(
            id = "collapseAusstattung", class = "collapse",
            div(
              class = "card-body",
              h3("Sanitärausstattung"),
              checkboxGroupInput("sanitär", "Wählen Sie aus:", choices = sanitär_items),
              textOutput("sanitär_zuschlag"),
              br(),
              h3("Weitere Ausstattungsmerkmale"),
              checkboxGroupInput("ausstattung", "Wählen Sie aus:", choices = names(ausstattung_items)),
              textOutput("ausstattung_zuschlag")
            )
          )
        )
      )
    ),

    column(
      8,
      h3("Karte"),
      leafletOutput("map", height = 400)
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Call the Wohnungsgröße module server part
  wohnungsgroesseServer("wohnungsGrosse")

  # Custom message handler for tab color updates
  observe({
    session$onFlushed(function() {
      session$sendCustomMessage(type = "updateTabColor", list(tabId = "tabWohnungsgröße", colorClass = "btn-light-green"))
    }, once = TRUE)
  })

  # Define the create_hausnummer function
  create_hausnummer <- function(data) {
    data %>%
      mutate(Hausnummer = paste0(as.character(HNR), ifelse(is.na(HNRZ), "", HNRZ))) %>%
      arrange(as.numeric(HNR), HNRZ) %>%
      pull(Hausnummer)
  }

  # Address dropdown dynamic generation
  output$hausnummer_dropdown <- renderUI({
    req(input$strasse)
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)
    hausnummern <- create_hausnummer(gefilterte_adr)
    selectInput("hausnummer", "Hausnummer:", choices = c("", unique(hausnummern)))
  })

  # Observe other tab actions and update colors accordingly
  observeEvent(input$strasse, {
    if (input$strasse != "") {
      session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabAdresse", colorClass = "btn-light-green"))
    }
  })

  observeEvent(input$baujahr, {
    if (input$baujahr != "") {
      session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabBaujahr", colorClass = "btn-light-green"))
    }
  })

  observeEvent(input$sanierung, {
    if ("Keine Sanierungsmaßnahme bekannt" %in% input$sanierung) {
      updateCheckboxGroupInput(session, "sanierung", selected = "Keine Sanierungsmaßnahme bekannt", choices = renovation_items)
      session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabSanierung", colorClass = "btn-light-green"))
      output$sanierung_zuschlag <- renderText({
        "Sanierungszuschlag: 0%"
      })
    } else {
      updateCheckboxGroupInput(session, "sanierung", selected = input$sanierung, choices = renovation_items[-1])
      if (length(input$sanierung) >= 3) {
        session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabSanierung", colorClass = "btn-light-green"))
        output$sanierung_zuschlag <- renderText({
          paste("Sanierungszuschlag: +6%")
        })
      } else {
        output$sanierung_zuschlag <- renderText({
          "Sanierungszuschlag: 0%"
        })
      }
    }
  })

  observeEvent(input$sanitär, {
    if (length(input$sanitär) >= 3) {
      session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabAusstattung", colorClass = "btn-light-green"))
      output$sanitär_zuschlag <- renderText({
        paste("Sanitärausstattungszuschlag: +6%")
      })
    } else {
      output$sanitär_zuschlag <- renderText({
        "Sanitärausstattungszuschlag: 0%"
      })
    }
  })

  observeEvent(input$ausstattung, {
    total_percentage <- sum(unlist(ausstattung_items[input$ausstattung]))
    if (length(input$ausstattung) > 0) {
      session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabAusstattung", colorClass = "btn-light-green"))
    }
    output$ausstattung_zuschlag <- renderText({
      paste("Weitere Ausstattungszuschläge: ", sprintf("%.2f%%", total_percentage * 100), sep = "")
    })
  })

  # Render the Leaflet map based on the selected street and house number
  output$map <- renderLeaflet({
    req(input$strasse)
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)
    color_palette <- colorFactor(marker_colors, domain = c("A", "B", "C"))
    border_palette <- colorFactor(border_colors, domain = c("A", "B", "C"))
    map <- leaflet() %>%
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
    if (!is.null(input$hausnummer) && input$hausnummer != "") {
      gefilterte_adr <- gefilterte_adr %>%
        mutate(FullAddress = paste(STRASSE, paste0(HNR, ifelse(is.na(HNRZ), "", HNRZ))))
      selected_adr <- gefilterte_adr %>%
        filter(FullAddress == paste(input$strasse, input$hausnummer))
      if (nrow(selected_adr) > 0) {
        map <- map %>%
          addCircleMarkers(
            data = selected_adr,
            lng = ~ st_coordinates(geometry)[, 1],
            lat = ~ st_coordinates(geometry)[, 2],
            fillColor = ~ color_palette(WL_2024),
            color = ~ border_palette(WL_2024),
            weight = 4,
            radius = 10,
            stroke = TRUE,
            fillOpacity = 0.8,
            popup = ~ paste("Adresse:", FullAddress)
          )
      }
    }
    map %>%
      addLegend(
        position = "bottomright",
        title = "Legende",
        colors = marker_colors,
        labels = c("Lage A", "Lage B", "Lage C"),
        opacity = 1
      ) %>%
      fitBounds(
        lng1 = min(st_coordinates(gefilterte_adr$geometry)[, 1]),
        lat1 = min(st_coordinates(gefilterte_adr$geometry)[, 2]),
        lng2 = max(st_coordinates(gefilterte_adr$geometry)[, 1]),
        lat2 = max(st_coordinates(gefilterte_adr$geometry)[, 2])
      )
  })

  # Display the percentage based on the selected year range
  output$baujahr_percent <- renderText({
    req(input$baujahr)
    if (input$baujahr == "") {
      return(NULL)
    }
    percent_value <- year_ranges[input$baujahr]
    display_value <- display_labels[input$baujahr]
    paste("Zu-/Abschlag für BJ ", input$baujahr, ": ", display_value, " (", sprintf("%.2f", percent_value), ")", sep = "")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
