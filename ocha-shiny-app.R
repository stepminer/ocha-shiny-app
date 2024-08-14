#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load necessary packages
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(readr)

# Load the data
data <- read_csv("PDI_2024_geocoded.csv")

# UI
ui <- fluidPage(
  titlePanel("Haitians in Need Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("department", "Select Department:",
                  choices = unique(data$department),
                  selected = unique(data$department)[1]),
      uiOutput("city_selector")
    ),
    
    mainPanel(
      textOutput("total_in_need"),
      plotlyOutput("bar_plot"),
      leafletOutput("map")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Dynamic UI for city selection based on department
  output$city_selector <- renderUI({
    req(input$department)
    cities <- unique(data %>% filter(department == input$department) %>% pull(city))
    selectInput("city", "Select City:", choices = cities)
  })
  
  # Reactive data filtering
  filtered_data <- reactive({
    req(input$department)
    data %>%
      filter(department == input$department, city == input$city)
  })
  
  # Display total in need
  output$total_in_need <- renderText({
    total <- sum(filtered_data()$pdi)
    paste("Total in Need:", total)
  })
  
  # Bar plot of people in need by city
  output$bar_plot <- renderPlotly({
    plot_data <- data %>%
      filter(department == input$department) %>%
      group_by(city) %>%
      summarize(TotalInNeed = sum(pdi))
    
    ggplot(plot_data, aes(x = city, y = TotalInNeed)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
        theme(axis.text.x = element_text(color="darkblue", size=10, angle = 45))+
      labs(title = paste("Number of Haitians in Need in", input$department),
           x = "City", y = "Total in Need")
  })
  
  # Interactive map
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                 radius = ~pdi * 0.15, popup = ~paste(city, ": ", pdi))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
