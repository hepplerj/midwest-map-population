library(shiny)
library(leaflet)

shinyUI(
  fluidPage(
    tags$head(includeCSS("styles.css")),
    
    fixedPanel(id = "fullscreen",
               top = 0, left = 0, width = "100%", height = "100%",
               leafletOutput("cities_map", width = "100%", height = "100%")
               ),
    
    absolutePanel(id = "controls",
               top = 0, left = 0, width = 350,
               h3("Midwest Population Growth"),
               sliderInput("year", label = "Year", value = 1860, sep = "",
                           min = 1860, max = 2010, step = 10,
                           width = "100%",
                           animate=animationOptions(interval=1400, loop=FALSE)),
               checkboxInput("state_boundaries", label = "County boundaries", FALSE),
               #selectInput("population", label = "Demographics",  
               #            choices = list("None" = 1, "All population" = 2, "African American" = 3, "Asian American" = 4, "Mexican American" = 5), 
               #            selected = 1),
               plotOutput("cities_hist", height = 200),
               tags$p(tags$small(includeHTML("cesta_attr.html"))),
               tags$p(tags$small(includeHTML("attr.html")))
               )
    )
)

