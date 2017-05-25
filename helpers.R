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

## Functions --------------------------------------------------------------------

# try out various ways to get an acceptable color palette function
getpal <- function(cpop,nmax){
  if (length(cpop)>1){
    # try out value from nmax down to 1
    for (n in nmax:1){
      qpct <- 0:n/n
      cpopcuts <- quantile(cpop,qpct, na.rm = TRUE)
      # here we test to see if all the cuts are unique
      if (length(unique(cpopcuts))==length(cpopcuts)){
        if (n==1){ 
          # The data is very very skewed.
          # using quantiles will make everything one color in this case (bug?)
          # so fall back to colorBin method
          return(colorBin("YlGnBu",cpop, bins=nmax))
        }
        return(colorQuantile("YlGnBu", cpop, probs=qpct))
      }
    }
  }
  # if all values and methods fail make everything white
  pal <- function(x) { return("white") }
}

# draw_cities draws population bubbles
draw_cities <- function(map, data) {
  map %>%
    clearMarkers() %>%
    addCircleMarkers(data = data,
                     fillColor = "#B53D35", fillOpacity = 0.7, weight = 0.5,
                     color = "#fff",
                     radius = ~radius_scale(population),
                     lng = ~lng, lat = ~lat, layerId = ~id,
                     options = markerOptions(zIndexOffset = 100))
}

# draw_demographics draws the choropleth  
draw_demographics <- function(map, input, data) {
  req(input$population)
  cpop <- data[[input$population]]
  
  if (length(cpop)==0) return(map) # no pop data so just return (much faster)
  if(input$population == "None") { map %>% clearShapes() %>% clearControls() }
  
  pal <- getpal(cpop, 7)

  map %>%
    clearShapes() %>% 
    addPolygons(data = data,
                fillColor = ~pal(cpop),
                fillOpacity = 0.4,
                color = "#BDBDC3",
                weight = 1) %>% 
    addLegend(position = "bottomright",
              pal = pal, values = cpop)

}