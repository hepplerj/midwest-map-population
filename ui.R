library(shiny)
library(leaflet)
library(shinythemes)
 
shinyUI(
  fluidPage(
    tags$head(includeCSS("styles.css")),
    
    # Draw map
    fixedPanel(id = "fullscreen",
               top = 0, left = 0, width = "100%", height = "100%",
               leafletOutput("cities_map", width = "100%", height = "100%")
               ),
   
    # Draw sidebar 
    absolutePanel(id = "controls",
               top = 0, left = 0, width = 350,
               h3("Midwest Population Growth"),
               
               # Slider for year selection
               sliderInput("year", label = "Year", value = 1860, sep = "",
                           min = 1810, max = 2010, step = 10,
                           width = "100%",
                           animate=animationOptions(interval=1400, loop=FALSE)),
              
               # Draw county boundaries 
               checkboxInput("state_boundaries", label = "County boundaries", FALSE),
              
               # Select demographics 
               selectInput("population", label = "Demographics",  
                           choices = list("None" = 1, "All population" = 2, 
                                          "Black population" = 3, "Enslaved population (1790-1860)" = 4, 
                                          "Free black population (1790-1860)" = 5, "Asian population" = 6, 
                                          "Hispanic population" = 7, "Native population" = 8, "Population density" = 9), 
                           selected = 1),
              
               # Output 
               plotOutput("cities_hist", height = 200),
               tags$p(tags$small(includeHTML("cesta_attr.html"))),
               tags$p(tags$small(includeHTML("attr.html")))
               )
    )
)
