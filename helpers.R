cities <- read_csv("data/midwest-cities.csv")
fn <- Sys.glob("shp/*.shp")
counties <- lapply(fn, read_sf)
names(counties) <- c("1810", "1820","1830","1840","1850","1860","1870","1880","1890","1900",
                     "1910","1920","1930","1940","1950","1960","1970","1980","1990","2000","2010")

radius_scale <- function(x) {
  max_pop <- sqrt(max(cities$population))
  x %>%
    sqrt() %>%
    rescale(to = c(1, 55), from = range(0, max_pop))
}

## Handle pop-ups
# For cities
popup_cities <- function(name, year, population, county) {
  paste0("<h5>", name, "</h5>",
         "<b>", "County: ", "</b>", county, "<br>",
         "<b>", "Population in ", year, ": ", "</b>", comma(population))
}

## Functions

# draw_cities draws population bubbles
draw_cities <- function(map, data) {
  map %>%
    clearMarkers() %>%
    addCircleMarkers(data = data,
                     fillColor = "#B53D35", fillOpacity = 0.6, weight = 0.5,
                     color = "#fff",
                     radius = ~radius_scale(population),
                     lng = ~lng, lat = ~lat, layerId = ~id,
                     options = markerOptions(zIndexOffset = 100))
}

# draw_demographics draws the choropleth  
draw_demographics <- function(map, input, data, demographic_choice) {
  pal <- colorNumeric("Blues", NULL, n = 9)
  
  map %>%
    clearShapes() %>% 
    addPolygons(data = data,
                fillColor = ~pal(input$population),
                fillOpacity = 0.4,
                color = "#BDBDC3",
                weight = 1)

}