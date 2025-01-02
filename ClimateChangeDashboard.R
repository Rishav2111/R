
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(leaflet.extras) 
library(readr)


set.seed(123)
years <- 1970:2020
temperature <- runif(length(years), 14, 20) + 0.03 * (years - 1970) # Simulated temperature changes
co2_levels <- runif(length(years), 300, 450) + 0.5 * (years - 1970) # Simulated CO2 levels
sea_levels <- runif(length(years), 0, 5) + 0.01 * (years - 1970) # Simulated sea-level changes


climate_data <- data.frame(
  year = years,
  average_temperature = temperature,
  co2_emissions = co2_levels,
  sea_level_rise = sea_levels
)


num_points <- 100
geographical_data <- data.frame(
  latitude = runif(num_points, -60, 60),
  longitude = runif(num_points, -180, 180),
  average_temperature = runif(num_points, 14, 20),
  co2_emissions = runif(num_points, 300, 450),
  sea_level_rise = runif(num_points, 0, 5)
)
ui <- fluidPage(
  titlePanel("Climate Change Dashboard"),
  sidebarLayout(
    sidebarPanel(
      helpText("Visualize temperature, CO2 emissions, and sea-level rise trends over time."),
      selectInput("selectedVariable", "Select Data to Visualize:",
                  choices = c("Average Temperature (°C)" = "average_temperature",
                              "CO2 Emissions (ppm)" = "co2_emissions",
                              "Sea Level Rise (cm)" = "sea_level_rise")),
      sliderInput("timeRange", "Select Year Range:",
                  min = min(climate_data$year),
                  max = max(climate_data$year),
                  value = c(1970, 2020),
                  step = 1),
      hr(),
      h4("Geographical Heatmap"),
      selectInput("selectedHeatmapVariable", "Select Data for Geographical Heatmap:",
                  choices = c("Average Temperature (°C)" = "average_temperature",
                              "CO2 Emissions (ppm)" = "co2_emissions",
                              "Sea Level Rise (cm)" = "sea_level_rise"))
    ),
    mainPanel(
      plotOutput("timeSeriesPlot"),
      br(),
      h4("Statistical Summary:"),
      verbatimTextOutput("minimumValue"),
      verbatimTextOutput("maximumValue"),
      verbatimTextOutput("meanValue"),
      verbatimTextOutput("standardDeviation"),
      hr(),
      h4("Geographical Heatmap:"),
      leafletOutput("geographicalHeatmap", height = "500px")
    )
  )
)
server <- function(input, output) {
  filteredClimateData <- reactive({
    climate_data %>%
      filter(year >= input$timeRange[1], year <= input$timeRange[2])
  })
  

  output$timeSeriesPlot <- renderPlot({
    req(input$selectedVariable)
    ggplot(filteredClimateData(), aes(x = year)) +
      geom_line(aes_string(y = input$selectedVariable), size = 1, color = "blue") +
      labs(
        title = paste("Trends in", gsub("_", " ", input$selectedVariable)),
        x = "Year",
        y = gsub("_", " ", input$selectedVariable)
      ) +
      theme_minimal()
  })
  output$minimumValue <- renderText({
    min_value <- min(filteredClimateData()[[input$selectedVariable]])
    paste("Minimum Value:", round(min_value, 2))
  })
  
  output$maximumValue <- renderText({
    max_value <- max(filteredClimateData()[[input$selectedVariable]])
    paste("Maximum Value:", round(max_value, 2))
  })
  
  output$meanValue <- renderText({
    mean_value <- mean(filteredClimateData()[[input$selectedVariable]])
    paste("Mean Value:", round(mean_value, 2))
  })
  
  output$standardDeviation <- renderText({
    std_dev_value <- sd(filteredClimateData()[[input$selectedVariable]])
    paste("Standard Deviation:", round(std_dev_value, 2))
  })
  output$geographicalHeatmap <- renderLeaflet({
    selected_variable <- input$selectedHeatmapVariable
    leaflet(geographical_data) %>%
      addTiles() %>%
      addHeatmap(
        lng = ~longitude,
        lat = ~latitude,
        intensity = ~get(selected_variable),
        radius = 20,
        blur = 15,
        minOpacity = 0.2
      ) %>%
      addLegend(position = "bottomright", pal = colorNumeric("YlOrRd", geographical_data[[selected_variable]]),
                values = ~get(selected_variable),
                title = gsub("_", " ", selected_variable))
  })
}

shinyApp(ui = ui, server = server)
