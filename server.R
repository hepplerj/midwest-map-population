library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)
library(USAboundaries)

# color palette
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
  
  demographics_by_year <- reactive({
    counties %>%
      filter(year == counties[[input$year]])
  })
  
  # Histogram plot of population 
  # ---------------------------------------------------------------------------
  output$cities_hist <- renderPlot({
    pops <- cities_by_year()
    ggplot(pops, aes(x = population)) +
      geom_histogram() +
      scale_x_log10(labels = comma) +
      theme_minimal() +
      theme(text = element_text(family="Open Sans"),
            plot.title = element_text(family = "Open Sans Semibold")) +
      labs(x = "Population", y = "# of cities",
           title = paste("Midwest cities in", input$year))
  })

  # Map setup
  # ---------------------------------------------------------------------------
  output$cities_map <- renderLeaflet({
    map <- leaflet() %>%
            addProviderTiles(provider = "CartoDB.Positron",
                             providerTileOptions(detectRetina = FALSE,
                                                 reuseTiles = TRUE,
                                                 minZoom = 4,
                                                 maxZoom = 8)) %>%
            setView(lat = 43.25, lng = -94.30, zoom = 6)
    
    # Initally draw the map without relying on cities_by_year()
    map %>%
      draw_cities(filter(cities, year == 1860))
    
    map %>%
      draw_demographics(filter(counties, year == 1860 & population == "totalPopulation"))
  })
  
  observe({
    map <- leafletProxy("cities_map", session, deferUntilFlush = TRUE)
  })

  # County boundaries
  # ---------------------------------------------------------------------------
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
                    fill = FALSE, color = "#57234D", fillOpacity = 0.5, weight = 0.3) %>%
        addPolygons(data = state,
                    fill = FALSE, color= "#57234D", weight = 1)
    } else {
     map %>% clearShapes()
    }
  })
  
  # Update cities by year
  # ---------------------------------------------------------------------------
  observe({
    leafletProxy("cities_map", session, deferUntilFlush = FALSE) %>%
      draw_cities(cities_by_year()) #%>%
      draw_demographics(demographics_by_year())
  })
  
  # Provide data for download
  # ---------------------------------------------------------------------------
  data_for_dl <- reactive({
    dat <- select(metro()@data, GISJOIN, state, county, totalWhitePopulation, totalAfAmPopulation, totalAsianPopulation, totalIndianPopulation, totalDensity, totalPopulation)
  })
  output$downloadCSV <- downloadHandler(
    filename = 'data.csv',
    content = function(file) {
      write_csv(data_for_dl(), file)
    }
  )
  downloadLink('downloadCSV', label = "Download CSV for active timeframe.")

})
