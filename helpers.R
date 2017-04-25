cities <- read_csv("midwest-cities.csv")
counties <- read_rds("midwest-counties.rds")
names(counties) <- c(1860, 1870, 1880, 1890, 1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)

radius_scale <- function(x) {
  max_pop <- sqrt(max(cities$population))
  x %>%
    sqrt() %>%
    rescale(to = c(1, 55), from = range(0, max_pop))
}

# Handle pop-ups
popup_maker <- function(name, year, population, county) {
  paste0("<h5>", name, "</h5>",
         "<b>", "County: ", "</b>", county, "<br>",
         "<b>", "Population in ", year, ": ", "</b>", comma(population))
}

draw_cities <- function(map, data) {
  map %>%
    clearMarkers() %>%
    addCircleMarkers(data = data,
                     fillColor = "#B53D35", fillOpacity = 0.6, weight = 0.5,
                     color = "#fff",
                     radius = ~radius_scale(population),
                     lng = ~lng, lat = ~lat, layerId = ~id,
                     popup = popup_maker(data$cityst,
                                         data$year,
                                         data$population,
                                         data$county_name),
                     options = markerOptions(zIndexOffset = 100))
}
  
draw_demographics <- function(map, data, population) {
  
  pal <- colorQuantile(
    palette = "Blues",
    domain = data,
    n = 9
  )
  
  map %>%
    clearShapes() %>%
    addPolygons(data = data,
                fillColor = ~pal(dat),
                fillOpacity = 0.7,
                color = "white",
                weight = 1)# %>%
    #addLegend(pal = pal,
    #          values = data$dat,
    #          position = "bottomright",
    #          title = "Population")
}