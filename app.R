library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Define paths for the RData and shapefile
rdata_path <- "./data/adr_2024.RData"
shapefile_path <- "./data/SHP/adr2024.shp"

# Check if the RData file exists
if (file.exists(rdata_path)) {
  # Load the RData file
  load(rdata_path)
} else {
  # Import the shapefile
  sf_data <- st_read(shapefile_path)

  # Check the CRS (Coordinate Reference System)
  print(st_crs(sf_data))

  # Convert to WGS84 (EPSG:4326) if not already in that CRS
  if (st_crs(sf_data)$epsg != 4326) {
    sf_data <- st_transform(sf_data, crs = 4326)
  }

  # Save the processed sf_data to an RData file
  save(sf_data, file = rdata_path)
}

# Now sf_data is available for use in the Shiny app
# Extract unique street names and sort them alphabetically
strassen <- sort(unique(sf_data$STRASSE))

# Load data for dropdown options
RefTab_Groesse <- read.csv("./data/RefTab_Groesse.csv", sep = ";")
values_list <- lapply(seq_len(nrow(RefTab_Groesse)), function(i) {
  list(
    low = RefTab_Groesse$low[i],
    med = RefTab_Groesse$med[i],
    hi = RefTab_Groesse$hi[i]
  )
})

dropdown_options <- setNames(
  seq_len(nrow(RefTab_Groesse)),
  paste(RefTab_Groesse$von, "bis unter", RefTab_Groesse$bis_unter, "m²")
)

# Define year ranges and corresponding percentages
year_ranges <- c(
  "bis 1918" = 0.00,
  "1919 - 1945" = -0.07,
  "1946 - 1977" = -0.10,
  "1978 - 1984" = -0.05,
  "1985 - 1995" = -0.01,
  "1996 - 2004" = 0.06,
  "2005 - 2012" = 0.12,
  "2013 - 2018" = 0.19,
  "2019 - 2023" = 0.24
)

display_labels <- c(
  "bis 1918" = "0%",
  "1919 - 1945" = "-7%",
  "1946 - 1977" = "-10%",
  "1978 - 1984" = "-5%",
  "1985 - 1995" = "-1%",
  "1996 - 2004" = "+6%",
  "2005 - 2012" = "+12%",
  "2013 - 2018" = "+19%",
  "2019 - 2023" = "+24%"
)

# Define the renovation items
renovation_items <- c(
  "Sanitärbereich (mind. Fliesen, Wanne, WC) erneuert",
  "Elektroinstallation (zeitgemäß) erneuert",
  "Heizanlage/Warmwasserversorgung erneuert",
  "Schallschutz eingebaut",
  "Fußböden erneuert",
  "Fenster-/Rahmenerneuerung",
  "Innen- und Wohnungstüren erneuert",
  "Treppenhaus, Eingangsbereich erneuert",
  "barrierearme Ausstattung geschaffen (Mindestvoraussetzung: schwellenfrei (max. 4cm Höhe), stufenloser Zugang, bodengleiche Dusche)",
  "Grundriss verbessert",
  "Dachsanierung",
  "Fassadensanierung"
)

# Add "Keine Sanierungsmaßnahme bekannt" as the first item
renovation_items <- c("Keine Sanierungsmaßnahme bekannt", renovation_items)

# Define Sanitärausstattung items
sanitär_items <- c(
  "zwei oder mehr abgeschlossene Badezimmer in der Wohnung vorhanden",
  "zweites WC/Gäste-WC vorhanden",
  "(separate) Einzeldusche",
  "Fußbodenheizung",
  "Belüftung(sanlage)",
  "separater WC-Raum vorhanden",
  "Handtuchheizkörper",
  "zweites Waschbecken im selben Badezimmer"
)

# Define Ausstattung items with corresponding percentages
ausstattung_items <- list(
  "Einbauküche mit mindestens zwei Elektroeinbaugeräten (z. B. Herd/Ofen, Gefrierschrank/-truhe, Kühlschrank, Geschirrspülmaschine) wird vom Vermieter ohne zusätzlichen Mietzuschlag gestellt." = 0.04,
  "Terrasse oder Dachterrasse" = 0.06,
  "Aufzug in Gebäuden mit weniger als 5 Stockwerken" = 0.07,
  "Überwiegend Parkett-, Dielen- oder Steinfußboden im überwiegenden Teil des Wohn-/Schlafbereichs, abgesehen von Flur/Bad verbaut" = 0.03,
  "Energiebedarfsklasse lt. Energiebedarfsausweis lautet F, G oder H; bzw. der Wert kWh/m2a ist größer oder gleich 200" = -0.09,
  "Teppichboden, PVC- oder Linoleum-Boden im überwiegenden Teil des Wohn-/Schlafbereichs, abgesehen von Flur/Bad verbaut, welcher seit 2013 nicht modernisiert bzw. saniert wurde" = -0.11
)

# Define consistent color codes for map markers and legend
marker_colors <- c("#FF0000", "#0000FF", "#00FF00") # Red, Blue, Green
border_colors <- c("#8B0000", "#00008B", "#006400") # Darker Red, Darker Blue, Darker Green

# Define UI for the application
ui <- fluidPage(
  titlePanel("Qualifizierter Mietspiegel der Stadt Passau ab 2024"),

  # Custom CSS to control the accordion and tab colors
  tags$head(
    tags$style(HTML("
      .custom-accordion .card-header {
        padding: 0;
        border: none;
        background: none;
      }
      .custom-accordion .btn {
        text-align: left;
        width: 100%;
        padding: 10px;
        border: 1px solid #ddd;
        color: #333;
        font-weight: bold;
      }
      .custom-accordion .btn-light-red {
        background-color: #f8d7da;
      }
      .custom-accordion .btn-light-green {
        background-color: #d4edda;
      }
      .custom-accordion .btn:focus {
        outline: none;
        box-shadow: none;
      }
      .custom-accordion .btn:hover {
        background-color: #e7e7e7;
      }
      .custom-accordion .collapse.show {
        margin-bottom: 10px;
      }
    "))
  ),

  # JavaScript to close other tabs when one is opened and to handle tab colors
  tags$script(HTML("
    $(document).on('click', '.collapse.show', function(e) {
      $('.collapse').not(this).collapse('hide');
    });

    Shiny.addCustomMessageHandler('updateTabColor', function(message) {
      $('#' + message.tabId).removeClass('btn-light-red').addClass('btn-light-green');
    });
  ")),
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
              h3("Wohnungsgröße auswählen"),
              selectInput("groesse", "Auswahl der Wohnungsgröße:", choices = c("", dropdown_options)), # Set default value to empty
              textOutput("GROESSE"),
              textOutput("low_value"),
              uiOutput("med_value"),
              textOutput("hi_value")
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

# Define server logic
server <- function(input, output, session) {
  # Reactively get the selected size values
  selected_values <- reactive({
    values_list[[as.numeric(input$groesse)]]
  })

  output$low_value <- renderText({
    req(input$groesse) # Ensure a selection is made before displaying values
    paste("Basiswert (min):", selected_values()$low, " EUR/m²")
  })

  output$med_value <- renderUI({
    req(input$groesse) # Ensure a selection is made before displaying values
    strong(paste("Basismittelwert:", selected_values()$med, " EUR/m²"),
           style = "font-weight: bold; font-size: 20px; color: #FF5733;"
    )
  })

  output$hi_value <- renderText({
    req(input$groesse) # Ensure a selection is made before displaying values
    paste("Basiswert (max):", selected_values()$hi, " EUR/m²")
  })

  output$GROESSE <- renderText({
    req(input$groesse) # Ensure a selection is made before displaying values
    paste(
      "Die Wohnungsgröße von ",
      names(dropdown_options)[as.numeric(input$groesse)],
      " ergibt folgende Basiswerte:"
    )
  })

  # Observe selections and change tab color when a selection is made
  observeEvent(input$groesse, {
    if (input$groesse != "") {
      session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabWohnungsgröße"))
    }
  })

  observeEvent(input$strasse, {
    if (input$strasse != "") {
      session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabAdresse"))
    }
  })

  observeEvent(input$baujahr, {
    if (input$baujahr != "") {
      session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabBaujahr"))
    }
  })

  observeEvent(input$sanierung, {
    if ("Keine Sanierungsmaßnahme bekannt" %in% input$sanierung) {
      updateCheckboxGroupInput(session, "sanierung", selected = "Keine Sanierungsmaßnahme bekannt", choices = renovation_items)
      session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabSanierung"))
      output$sanierung_zuschlag <- renderText({
        "Sanierungszuschlag: 0%"
      })
    } else {
      updateCheckboxGroupInput(session, "sanierung", selected = input$sanierung, choices = renovation_items[-1])
      if (length(input$sanierung) >= 3) {
        session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabSanierung"))
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
      session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabAusstattung"))
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
      session$sendCustomMessage(type = "updateTabColor", message = list(tabId = "tabAusstattung"))
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

# Run the application
shinyApp(ui = ui, server = server)
