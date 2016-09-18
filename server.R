library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)
library(USAboundaries)

# purple: 57234D
# yellow: F1AD0D
# red: B53D35
# green: 99AD48
# blue: 31426B

source("helpers.R")

shinyServer(function(input, output, session) {

  cities_by_year <- reactive({
    cities %>%
      filter(year == input$year)
  })

  output$cities_hist <- renderPlot({
    pops <- cities_by_year()
    ggplot(pops, aes(x = population)) +
      geom_histogram() +
      scale_x_log10(labels = comma) +
      theme_minimal() +
      labs(x = "Population", y = "# of cities",
           title = paste("Midwest cities in", input$year))
  })

  output$cities_map <- renderLeaflet({
    map <- leaflet() %>%
            addProviderTiles(provider = "CartoDB.Positron",
                             providerTileOptions(detectRetina = FALSE,
                                                 reuseTiles = TRUE,
                                                 minZoom = 4,
                                                 maxZoom = 8)) %>%
            setView(lat = 44.94, lng = -95.72, zoom = 5)

    # Initally draw the map without relying on cities_by_year(). Because if we
    # rely on that, then Shiny will make drawing the map reactive, and every
    # time cities_by_year() changes, the entire map will be redrawn.
    map %>%
      draw_cities(filter(cities, year == 1850))
  })

  observe({
    map <- leafletProxy("cities_map", session, deferUntilFlush = TRUE)
  })

  observe({
    map <- leafletProxy("cities_map", session, deferUntilFlush = FALSE)
    if (input$state_boundaries) {
      date   <- as.Date(paste(input$year, 1, 1, sep = "-"))
      if (date > as.Date("2000-12-31")) date <- as.Date("2000-01-01")
      counties <- us_counties(date, states=c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin", "Dakota Territory"))
      state <- us_states(date, states=c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin", "Dakota Territory"))
      map %>%
        clearShapes() %>%
        addPolygons(data = counties,
                    fill = FALSE, color = "#31426B", fillOpacity = 0.5, weight = 0.3) %>%
        addPolygons(data = state,
                    fill = FALSE, color= "#31426B", weight = 1)
    } else {
     map %>% clearShapes()
    }
  })
  
  observe({
    leafletProxy("cities_map", session, deferUntilFlush = FALSE) %>%
      draw_cities(cities_by_year())
  })

})
