cities <- read_csv("midwest-cities.csv")
counties <- read_csv("midwest-census.csv")

radius_scale <- function(x) {
  max_pop <- sqrt(max(cities$population))
  x %>%
    sqrt() %>%
    rescale(to = c(1, 55), from = range(0, max_pop))
}

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
  
draw_demographics <- function(map, data) {
  
  pal <- colorNumeric(
    palette = "Blues",
    domain = data$dat
  )
  
  map %>%
    clearShapes() %>%
    addPolygons(data = data,
                fillColor = ~pal(dat),
                fillOpacity = 0.7,
                color = "white",
                weight = 1) %>%
    addLegend(pal = pal,
              values = data$dat,
              position = "bottomright",
              title = "Population")
}