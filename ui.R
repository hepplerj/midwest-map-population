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
               h3("Midwestern Population Growth"),
               p(strong("UNDER DEVELOPMENT", style = "color: red;")),
               sliderInput("year", "Year", value = 1850, sep = "",
                           min = 1850, max = 2010, step = 10,
                           width = "100%",
                           animate=animationOptions(interval=1000, loop=FALSE)),
               checkboxInput("state_boundaries", "County boundaries", FALSE),
               selectInput("select", label = "Demographics",  
                           choices = list("All population" = 1, "African American" = 2, "Asian American" = 3, "Mexican American" = 3), 
                           selected = 1),
               plotOutput("cities_hist", height = 200),
               tags$p(tags$small(includeHTML("cesta_attr.html"))),
               tags$p(tags$small(includeHTML("my_attr.html")))
               )
    )
)

