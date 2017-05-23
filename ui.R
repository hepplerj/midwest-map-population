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
               sliderInput("year", label = "Year", value = 1810, sep = "",
                           min = 1810, max = 2010, step = 10,
                           width = "100%",
                           animate=animationOptions(interval=1400, loop=FALSE)),
              
               # Draw county boundaries 
               checkboxInput("state_boundaries", label = "County boundaries", FALSE),
              
               # Select demographics 
               uiOutput("select_population"),
               checkboxInput("legend", "Show legend", FALSE),
              
               # Histogram plot 
               plotOutput("cities_hist", height = 200),
              
               # Credits 
               tags$p(tags$small(includeHTML("cesta_attr.html"))),
               tags$p(tags$small(includeHTML("attr.html")))
               )
    )
)
