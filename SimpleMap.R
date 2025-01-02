library(shiny)
library(leaflet)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
            h2 {text-align: center;}
        "))
  ),
  
  titlePanel(h2("Interactive Map")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Select an Indian City:", 
                  choices = list("Mumbai" = "mumbai", 
                                 "Delhi" = "delhi", 
                                 "Bangalore" = "bangalore",
                                 "Kolkata" = "kolkata",
                                 "Chennai" = "chennai",
                                 "Hyderabad" = "hyderabad",
                                 "Pune" = "pune",
                                 "Ahmedabad" = "ahmedabad",
                                 "Jaipur" = "jaipur",
                                 "Lucknow" = "lucknow"),
                  selected = "bangalore"),
      sliderInput("zoom", "Zoom Level:", min = 5, max = 25, value = 10)
    ),
    
    mainPanel(
      
      leafletOutput("map"),
      br(),
      p("Use the dropdown to select an Indian city and adjust the zoom level using the slider."),
      p("Explore the map using different layers like OpenStreetMap and satellite view.")
    )
  )
)

server <- function(input, output, session) {
  
  cities <- reactive({
    switch(input$city,
           "mumbai" = list(lat = 19.0760, lng = 72.8777, name = "Mumbai"),
           "delhi" = list(lat = 28.7041, lng = 77.1025, name = "Delhi"),
           "bangalore" = list(lat = 12.9716, lng = 77.5946, name = "Bangalore"),
           "kolkata" = list(lat = 22.5726, lng = 88.3639, name = "Kolkata"),
           "chennai" = list(lat = 13.0827, lng = 80.2707, name = "Chennai"),
           "hyderabad" = list(lat = 17.3850, lng = 78.4867, name = "Hyderabad"),
           "pune" = list(lat = 18.5204, lng = 73.8567, name = "Pune"),
           "ahmedabad" = list(lat = 23.0225, lng = 72.5714, name = "Ahmedabad"),
           "jaipur" = list(lat = 26.9124, lng = 75.7873, name = "Jaipur"),
           "lucknow" = list(lat = 26.8467, lng = 80.9462, name = "Lucknow"))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%  
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = cities()$lng, lat = cities()$lat, zoom = input$zoom) %>%
      addMarkers(lng = cities()$lng, lat = cities()$lat, 
                 popup = paste("Welcome to", cities()$name, "!")) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      setView(lng = cities()$lng, lat = cities()$lat, zoom = input$zoom) %>%
      addMarkers(lng = cities()$lng, lat = cities()$lat, 
                 popup = paste("Welcome to", cities()$name, "!"))
  })
}

shinyApp(ui, server)
