library(shiny)
library(shinyIncubator)

weightInput <- function (inputId, label=paste0(inputId, ":"), value=1.0, ...) {
  sliderInput(inputId, label, value=value, min=0, max=1, step=0.1, ...)
}

multiSelectInput <- function (inputId, label, choices, ...) {
  selectInput(inputId, label, choices, selected=choices, multiple=TRUE, ...)
}

REGION_NAMES <- c("San Joaquin", "South Coast", "Bay Area", "Other")
POPCHAR_VARS <- c("Age", "Asthma", "LBW", "Edu", "LingIso", "Pov", "Unemp")
POLLUTION_VARS <- c("Ozone", "PM25", "DieselPM", "DrinkWat", "PestUse", "ToxRel", 
                    "Traffic", "Cleanup", "GndWat", "HazWst", "WatBod", "SolWst")

bootstrapPage(
  fluidPage(
    
    headerPanel("CalEnviroScreen 2.0 Explorer"),
    
    sidebarPanel(
      multiSelectInput("pollution_vars", "Pollution burden:", choices = POLLUTION_VARS),
      multiSelectInput("popchar_vars", "Population characteristics:", choices = POPCHAR_VARS),
      sliderInput("impacted_percentile", "Impacted (% statewide):", min=5, max=95, value=20, step=5)
    ),
    
    mainPanel(

      tabsetPanel(
        
        tabPanel("Chart", 
                 plotOutput("barchart")),      
        
        tabPanel("Map", 
                 selectInput("region_name", "", choices = REGION_NAMES, selected = "Bay Area"),
                 plotOutput("map", height="100%")),      
        
        tabPanel("Scatterplot", 
                 fluidRow(
                   column(9,
                          plotOutput("scatterplot")), #, height="100%")),
                   column(3,
                          br(),
                          checkboxInput("DensityPath", "Contours", value=FALSE),
                          checkboxInput("DensityFill", "Shading", value=TRUE),
                          sliderInput("SampleTracts", "Points (%):", min=0, max=100, step=10, value=30)))),     
        
        tabPanel("Weights", 
                 fluidRow(
                   column(3,
                          h4("Exposure"),
                          weightInput("Ozone"), 
                          weightInput("PM25"), 
                          weightInput("DieselPM"),
                          weightInput("DrinkWat"), 
                          weightInput("PestUse"), 
                          weightInput("ToxRel"), 
                          weightInput("Traffic")),
                   
                   column(3,
                          h4("Environment"),
                          weightInput("Cleanup",  value = 0.5), 
                          weightInput("GndWat",   value = 0.5),
                          weightInput("HazWaste", value = 0.5), 
                          weightInput("ImpWat",   value = 0.5),
                          weightInput("SolWaste", value = 0.5)),
                   
                   column(3,
                          h4("Sensitivity"),
                          weightInput("Age"), 
                          weightInput("Asthma"), 
                          weightInput("LBW")),
                   
                   column(3,
                          h4("Socioecon"),
                          weightInput("Edu"), 
                          weightInput("LingIso"),
                          weightInput("Pov"), 
                          weightInput("Unemp")))),
        
        tabPanel("Data", 
                 dataTableOutput("data"))
        
      )
    )
  )
)
