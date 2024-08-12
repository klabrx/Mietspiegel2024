library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(shinyjs)

# Source external scripts
source("scripts/Process_Shapefile.R")  # Processes and loads the shapefile
source("scripts/load_data.R")  # Loads the necessary data
source("scripts/define_options.R")  # Defines dropdown options and year ranges
source("modules/wohnungsgroesse_ui.R")  # Module UI for Wohnungsgröße
source("modules/wohnungsgroesse_server.R")  # Module Server for Wohnungsgröße

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs

  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),

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
            tags$button(id = "tabWohnungsgröße", class = "btn btn-light-red", "Wohnungsgröße", `data-toggle` = "collapse", `data-target` = "#collapseWohnungsgröße")
          ),
          div(
            id = "collapseWohnungsgröße", class = "collapse",
            div(
              class = "card-body",
              wohnungsGrosseUI("wohnungsGrosse")
            )
          )
        ),
        # Adresse accordion
        div(
          class = "card",
          div(
            class = "card-header",
            tags$button(id = "tabAdresse", class = "btn btn-light-red", "Adresse", `data-toggle` = "collapse", `data-target` = "#collapseAdresse")
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
                             )
              ),
              uiOutput("hausnummer_dropdown")
            )
          )
        ),
        # Baujahr accordion
        div(
          class = "card",
          div(
            class = "card-header",
            tags$button(id = "tabBaujahr", class = "btn btn-light-red", "Baujahr", `data-toggle` = "collapse", `data-target` = "#collapseBaujahr")
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
            tags$button(id = "tabSanierung", class = "btn btn-light-red", "Sanierung", `data-toggle` = "collapse", `data-target` = "#collapseSanierung")
          ),
          div(
            id = "collapseSanierung", class = "collapse",
            div(
              class = "card-body",
              h3("Sanierung auswählen"),
              checkboxGroupInput("sanierung", "Wählen Sie durchgeführte Sanierungen aus:", choices = renovation_items),
              textOutput("sanierung_zuschlag") # Display surcharge based on selected renovations
            )
          )
        ),
        # Ausstattung, Beschaffenheit accordion
        div(
          class = "card",
          div(
            class = "card-header",
            tags$button(id = "tabAusstattung", class = "btn btn-light-red", "Ausstattung, Beschaffenheit", `data-toggle` = "collapse", `data-target` = "#collapseAusstattung")
          ),
          div(
            id = "collapseAusstattung", class = "collapse",
            div(
              class = "card-body",
              h3("Sanitärausstattung"),
              checkboxGroupInput("sanitär", "Wählen Sie aus:", choices = sanitär_items),
              textOutput("sanitär_zuschlag"), # Display surcharge based on selected sanitär options
              br(),
              h3("Weitere Ausstattungsmerkmale"),
              checkboxGroupInput("ausstattung", "Wählen Sie aus:", choices = names(ausstattung_items)),
              textOutput("ausstattung_zuschlag") # Display surcharge based on selected ausstattung options
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

server <- function(input, output, session) {
  # Function to update tab color
  updateTabColor <- function(tab_id, new_class) {
    runjs(paste0('$("#', tab_id, '").removeClass("btn-light-red").addClass("', new_class, '");'))
  }

  # Call the WohnungsGrosse module
  wohnungsGrosseServer("wohnungsGrosse", updateTabColor)

  # Observe selections and change tab color when a selection is made
  observeEvent(input$strasse, {
    if (input$strasse != "") {
      updateTabColor("tabAdresse", "btn-light-green")
    }
  })

  observeEvent(input$baujahr, {
    if (input$baujahr != "") {
      updateTabColor("tabBaujahr", "btn-light-green")
    }
  })

  observeEvent(input$sanierung, {
    if ("Keine Sanierungsmaßnahme bekannt" %in% input$sanierung) {
      updateCheckboxGroupInput(session, "sanierung", selected = "Keine Sanierungsmaßnahme bekannt", choices = renovation_items)
      updateTabColor("tabSanierung", "btn-light-green")
      output$sanierung_zuschlag <- renderText({
        "Sanierungszuschlag: 0%"
      })
    } else {
      updateCheckboxGroupInput(session, "sanierung", selected = input$sanierung, choices = renovation_items[-1])
      if (length(input$sanierung) >= 3) {
        updateTabColor("tabSanierung", "btn-light-green")
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
      updateTabColor("tabAusstattung", "btn-light-green")
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
      updateTabColor("tabAusstattung", "btn-light-green")
    }
    output$ausstattung_zuschlag <- renderText({
      paste("Weitere Ausstattungszuschläge: ", sprintf("%.2f%%", total_percentage * 100), sep = "")
    })
  })

  # Function to create a sortable Hausnummer field
  create_hausnummer <- function(data) {
    data %>%
      mutate(Hausnummer = paste0(as.character(HNR), ifelse(is.na(HNRZ), "", HNRZ))) %>%
      arrange(as.numeric(HNR), HNRZ) %>%
      pull(Hausnummer)
  }

  # Function to create the full address
  create_full_address <- function(data) {
    data %>%
      mutate(FullAddress = paste(STRASSE, as.character(HNR), ifelse(is.na(HNRZ), "", HNRZ))) %>%
      arrange(FullAddress) %>%
      pull(FullAddress)
  }

  # Dynamically generate the house number dropdown based on the selected street
  output$hausnummer_dropdown <- renderUI({
    req(input$strasse)
    gefilterte_adr <- sf_data %>%
      filter(STRASSE == input$strasse)
    hausnummern <- create_hausnummer(gefilterte_adr)
    selectInput("hausnummer", "Hausnummer:", choices = c("", unique(hausnummern)))
  })

  # Update street input when only one option is available
  observe({
    req(input$strasse)
    unique_streets <- strassen[grep(paste0("^", input$strasse), strassen)]
    if (length(unique_streets) == 1 && input$strasse != unique_streets) {
      updateSelectizeInput(session, "strasse", selected = unique_streets)
    }
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

shinyApp(ui = ui, server = server)
