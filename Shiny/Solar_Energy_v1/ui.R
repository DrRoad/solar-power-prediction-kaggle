library(data.table)
library(tidyverse)
library(shiny)
library(DT)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  h3("Solar Energy Production"),
  h4("Sebastian Montero"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("file_upload", "Choose RDS File"),
      textInput("s_selection", "Select station:", value = ""),
      radioButtons(inputId = "time_type", label = "Select time aggregate:", choices = c("Day", "Month","Year")),
      sliderInput("station_n", "Select top stations:",min = 1, max = 30,value = 15),
      width = 2
    ),
    
      mainPanel(
        tabsetPanel(
          tabPanel("Analysis",
                   h5("Locations"),leafletOutput("map",height = "300px"),
                   br(),
                   fluidRow(
                   column(7,
                   h5("Time Series Analysis"),plotOutput("line")), 
                   column(5,
                   h5("Bar Chart Analysis"),plotOutput("bar"))
                   )),
          
          tabPanel("Data Table",
                   h5("Raw Data File"),DTOutput("data"))
        )
        )
    )
  )
)
