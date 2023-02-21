#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(tigris)
library(leaflet)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DAC Mapping Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("pov",
                        "Poverty Threshold:",
                        min = 1,
                        max = 100,
                        value = 30),
            sliderInput("poc",
                        "People of Color Threshold:",
                        min = 1,
                        max = 100,
                        value = 30),
            sliderInput("lep",
                        "Limited English Proficiency Threshold:",
                        min = 1,
                        max = 100,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  block_group_data <- st_read("code/code_communication/DAC_Thresholds/block_group_data_sf.shp")

  print("success")
  filtered <- reactive({
    lep_threshold <- as.numeric(input$lep)/100
    pov_threshold <- as.numeric(input$pov)/100
    poc_threshold <- as.numeric(input$poc)/100
    
    block_group_data_sf %>% 
      filter(propHispanicOrNonWhite >= poc_threshold | 
               propPoverty >= pov_threshold | 
               propLEP >= lep_threshold)
  })
    
  output$map <- renderLeaflet({
    leaflet(filtered()) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5, fillColor = "Red")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
