library(shiny)

weightInput <- function (inputId, label=inputId, value=1.0, ...) {
  sliderInput(inputId, label, value=value, min=0, max=1, step=0.1, ...)
}

multiSelectInput <- function (inputId, label, choices, ...) {
  selectInput(inputId, label, choices, selected=choices, multiple=TRUE, ...)
}

POPCHAR_VARS <- c("Age", "Asthma", "LBW", "Edu", "LingIso", "Pov", "Unemp")
POLLUTION_VARS <- c("Ozone", "PM25", "DieselPM", "DrinkWat", "PestUse", "ToxRel", 
                    "Traffic", "Cleanup", "GndWat", "HazWst", "WatBod", "SolWst")

bootstrapPage(
  fluidPage(
    
    headerPanel("CalEnviroScreen 2.0 Explorer"),
    
    sidebarPanel(
      multiSelectInput("pollution_vars", "Pollution variables:", choices = POLLUTION_VARS),
      multiSelectInput("popchar_vars", "Population variables:", choices = POPCHAR_VARS),
      sliderInput("impacted_percentile", "Impacted (%):", min=0, max=100, value=20, step=5)
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Tally", 
                 plotOutput("barchart")),      
        
        tabPanel("Scatterplot", 
                 fluidRow(
                   column(9,
                          plotOutput("scatterplot")),
                   column(3,
                          br(),
                          checkboxInput("DensityPath", "Contours", value=FALSE),
                          checkboxInput("DensityFill", "Shading", value=TRUE),
                          sliderInput("SampleTracts", "Points (%):", min=0, max=100, step=10, value=30)))),     
        
        tabPanel("Weights", 
                 fluidRow(
                   column(3,
                          h4("Exposure"),
                          weightInput("Ozone"), weightInput("PM25"), weightInput("DieselPM"),
                          weightInput("DrinkWat"), weightInput("PestUse"), weightInput("ToxRel"), weightInput("Traffic")),
                   
                   column(3,
                          h4("Environment"),
                          weightInput("Cleanup",  "Cleanup:",         value=0.5),
                          weightInput("GndWat",   "GndWat:",           value=0.5),
                          weightInput("HazWst",   "HazWst:",       value=0.5),
                          weightInput("WatBod",   "WatBod:",          value=0.5),
                          weightInput("SolWst",   "SolWst:",           value=0.5)),
                   
                   column(3,
                          h4("Sensitivity"),
                          weightInput("Age",      "Age:"),
                          weightInput("Asthma",   "Asthma:"),
                          weightInput("LBW",      "Low birth weight:")),
                   
                   column(3,
                          h4("Socioecon"),
                          weightInput("Edu",      "Education:"),
                          weightInput("LingIso",  "Linguistic isolation:"),
                          weightInput("Pov",      "Poverty:"),
                          weightInput("Unemp",    "Unemployment:")))),
        
        tabPanel("Data", 
                 dataTableOutput("data"))
        
      )
    )
  )
)
