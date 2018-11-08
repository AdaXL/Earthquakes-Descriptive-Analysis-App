library(shiny)
library(leaflet)
library(shinythemes)

# Define UI for application
shinyUI(fluidPage(
  
  # Change theme as user preference
  shinythemes::themeSelector(),
  
  # Set original theme
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Global Significant Earthquakes 2000-2018"),
  
  # Three sidebars for uploading files, selecting variabes
  sidebarLayout(
    sidebarPanel(
      
      # Create a file input
      fileInput("file","Choose A CSV File Please:",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Create date range input for time select
      dateRangeInput("date", "Date Range:", 
                     start = "2000-01-01", end = NULL, 
                     min = "2000-01-01", max = NULL, 
                     format = "yyyy-mm-dd", 
                     startview = "year", 
                     weekstart = 0, 
                     separator = " to "),
      
      # Horizontal line ----
      tags$hr(),
      
      # Create slider input for earthquake magnitude range selection
      sliderInput("mag",
                  "Magnitude:",
                  min = 0.0,
                  max = 9.9,
                  value = c(0.0, 9.9)),
      # Explain the variable
      helpText("Magnitude measures the energy released at the source of the earthquake."),
      
      hr(),
      
      # Create slider input for depth of the earthquake
      sliderInput("dep",
                  "Focal Depth:",
                  min = 0.0,
                  max = 700,
                  value = c(0, 700)),
      # Explain the variable
      helpText("Focal depth is the depth of the earthquake. It is given in kilometers."),
      
      # Horizontal line ----
      tags$hr(),
      
      # Create a multiple checkbox input for associated tsu or vol
      checkboxGroupInput("tsu_vol",
                         "Associated Events:",
                         c("Tsunami", "Volcano")),
      # Explain the variable
      helpText("Select Earthquakes that have associated tsunami or volcanic eruption."),
      
      # Horizontal line ----
      tags$hr(),
      
      # Create a checkbox input for pop-up window to show details on map
      checkboxInput("pop", "Click Marker on Map to See Pop-up Detrails", value = TRUE)
    ),
    
    # Make the sidebar on the right of the webpage
    position = "left",
    fluid = TRUE,
    
    # Create main panel for display
    mainPanel(
      
      # Create navigating page
      navbarPage(
        
        # Add title
        title = "My App",
        # Add 3 tab panels
        tabPanel("About", htmlOutput("text")),
        tabPanel("Descriptive Analysis",
                 # Add 3 subtabs
                 tabsetPanel(
                   tabPanel("Summary", verbatimTextOutput("table1")),
                   tabPanel("Top 5's of Dataset",verbatimTextOutput("table2")),
                   tabPanel("Visualization",plotOutput("plot", height = "800px"))
                 )
        ),
        tabPanel("Map", leafletOutput("map", height=630))
      )
      
    )
  )
))