suppressPackageStartupMessages({
  library(sp)
  library(reshape2)
  library(tidyr)
  library(functional)
  library(ggplot2)
  library(grid)
  library(rgeos)
  library(scales)
  library(plyr)
  library(dplyr)
  library(ggvis)
  library(CalEnviroScreen)
})

set.seed(0)

data(CalEnviroScreen2, package="CalEnviroScreen")
data(California, package="CalEnviroScreen")

theme_set(theme_bw())
theme_update(
  plot.title = element_text(size=rel(1), face="bold", vjust=1.75),
  axis.title.x = element_text(size=rel(0.9), lineheight=1.1, face="bold", vjust=-0.5),
  axis.title.y = element_text(size=rel(0.9), lineheight=1.1, face="bold", angle=90)
)

options(digits=3)

scale_color_regions <- function (...) scale_color_manual("Region", values=region_colors)
scale_fill_regions <- function (...) scale_fill_manual("Region", values=region_colors)
scale_x_score <- function (...) scale_x_continuous(..., limits=c(0, 10), expand=c(0, 0))
scale_y_score <- function (...) scale_y_continuous(..., limits=c(0, 10), expand=c(0, 0))

region_colors <- c(`Bay Area`="#009E73", `South Coast`="#0072B2", `San Joaquin`="#D55E00", `Other`="#999999")

tract_tbl <- select(tract_regions, FIPS, Region)
with_region <- function (x) inner_join(x, tract_tbl, by="FIPS")

CA_tracts$FIPS <- row.names(CA_tracts)
BayArea_tracts <- gBuffer(subset(merge(CA_tracts, tract_tbl, by="FIPS"), Region == "Bay Area"), width=0.001, byid=TRUE)
BayArea_region <- gUnaryUnion(BayArea_tracts)

POPCHAR_VARS <- c("Age", "Asthma", "LBW", "Edu", "LingIso", "Pov", "Unemp")
POLLUTION_VARS <- c("Ozone", "PM25", "DieselPM", "DrinkWat", "PestUse", "ToxRel", 
                    "Traffic", "Cleanup", "GndWat", "HazWst", "WatBod", "SolWst")

###############################################################################
# Define server logic
###############################################################################

shinyServer(function(input, output) {
  
  .variables <- reactive({
    union(input$pollution_vars, input$popchar_vars)
  })
  
  .weights <- reactive({
    c(Ozone=input$Ozone, PM25=input$PM25, DieselPM=input$DieselPM, DrinkWat=input$DrinkWat, PestUse=input$PestUse, ToxRel=input$ToxRel, Traffic=input$Traffic,
      Cleanup=input$Cleanup, GndWat=input$GndWat, HazWst=input$HazWst, WatBod=input$WatBod, SolWst=input$SolWst,
      Age=input$Age, Asthma=input$Asthma, LBW=input$LBW,
      Edu=input$Edu, LingIso=input$LingIso, Pov=input$Pov, Unemp=input$Unemp)
  })
  
  .impacted_percentile <- reactive({
    100 - input$impacted_percentile # reverse the scale
  })
  
  .subscores <- reactive({
    CES2_data %>%
      inner_join(CES2_metadata, by = "Variable") %>%
      filter(Variable %in% .variables()) %>%
      mutate(Weight = .weights()[Variable]) %>%
      group_by(FIPS, Group) %>%
      compute_CES2_subscores(min_obs = 1) %>%
      spread(Group, Subscore)
  })
  
  .scores <- reactive({
    subscore_data <- .subscores() 
    if (is.null(subscore_data$PopChar)) {
      subscore_data$PopChar <- 1
    } else {
      if (is.null(subscore_data$Pollution)) 
        subscore_data$Pollution <- 1 
    }
    subscore_data %>% 
      compute_CES2_scores() %>%
      filter(!is.na(Score)) %>%
      arrange(desc(Score)) %>%
      with_region()
  })
  
  .pollution_maximum <- reactive(with(.scores(), max(Pollution, na.rm=TRUE)))
  .popchar_maximum <- reactive(with(.scores(), max(PopChar, na.rm=TRUE)))
  .score_cutoff <- reactive(with(.scores(), quantile(Score, .impacted_percentile() / 100, na.rm=TRUE)))
  .popchar_intercept <- reactive(.score_cutoff() / .pollution_maximum())
  
  .tally <- reactive({
    .scores() %>% 
      group_by(Region) %>% 
      summarise(Tracts=n(), No=sum(Percentile < .impacted_percentile()), Yes=Tracts-No)
  })
  
  .scatterplot <- reactive({
    fig_tbl <- .scores() %>% mutate(Sampled = as.logical(rbinom(n(), 1, prob = input$SampleTracts / 100)))
    cutoff_function <- function (x) {
      ifelse(x < .popchar_intercept() | x > .popchar_maximum(), NA, .score_cutoff() / x)
    }
    fig <- ggplot(fig_tbl, aes(PopChar, Pollution)) + 
      coord_equal() + scale_x_score("Population Characteristics\n") + scale_y_score("Pollution Burden") +
      scale_color_regions() + scale_fill_regions() +
      geom_abline(slope=1, intercept=0, alpha=0.15) +
      geom_point(aes(color=Region), alpha=I(0.3), size=I(2), subset=.(Sampled)) +
      scale_alpha(range=c(0.3, 0.6)) + 
      stat_function(fun=cutoff_function, geom="line", linetype="dashed", alpha=0.7) + 
      annotate("text", x=.popchar_intercept(), y=.pollution_maximum(), label=str_c("Top ", 100 - .impacted_percentile(), "%"), hjust=1.1, vjust=1.1) + 
      guides(alpha=FALSE, color=guide_legend("Region", override.aes = list(alpha = 0.5)), fill=FALSE)
    if (input$DensityPath) {
      fig <- fig + stat_density2d(aes(color=Region, alpha=..level..), subset=.(Region != "Other")) 
    }
    if (input$DensityFill) {
      fig <- fig + stat_density2d(aes(fill=Region), alpha=I(0.03), color=NA, geom="polygon", subset=.(Region != "Other"))
    }
    fig + theme(legend.position="bottom", legend.direction="horizontal")
  })
  
  .barchart <- reactive({
    .tally() %>% 
      melt(measure.vars = c("No", "Yes"), variable.name = "Impacted", value.name = "Freq") %>% 
      transform(Frac = Freq / Tracts) %>% 
      ggplot(aes(x=Region, y=Freq)) + 
      geom_bar(aes(fill=Impacted), stat="identity") + 
      scale_fill_manual(values=c(gray(0.7), gray(0.4))) +
      scale_y_continuous(limits=c(0, 4500), expand=c(0, 0)) +
      geom_text(aes(y=Tracts, label=str_c(percent(round(Yes/Tracts, 2)), " (n=", Yes, ")")), data=.tally(), vjust=-0.5) + 
      theme(legend.position="none", axis.title=element_blank())
  })
  
  plot_map <- function (x, ...) plot(x, col=gray(0.9), border=NA, ...)
  
  plot_tracts <- function (x, ..., color=gray(0.5)) {
    i <- which(with(x@data, ...))
    plot(x[i,], col=color, border=NA, add=TRUE)
  }
  
  .impacted_scores <- reactive({
    .scores() %>% filter(Percentile > .impacted_percentile())
  })
  
  .map_BayArea <- reactive({
    i <- intersect(row.names(BayArea_tracts), .impacted_scores()$FIPS)
    par(mar=c(0.1, 0.1, 0.1, 0.1))
    plot(BayArea_tracts[i, ], col=gray(0), border=NA)
    plot(BayArea_region, col="#88888888", border=NA, add=TRUE)
    dev.off()
  })
  
  output$tally <- renderDataTable(.tally())
  
  output$map_BayArea <- renderPlot(show(.map_BayArea()), height=600)
  
  output$scatterplot <- renderPlot(show(.scatterplot()))
  
  output$barchart <- renderPlot(show(.barchart()))

  output$data <- renderDataTable(.impacted_scores() %>% 
                                   select(FIPS, Pollution, PopChar, Score, Pctl=Percentile, PctlRange=PercentileRange), 
                                 options = list(bSortClasses=TRUE, iDisplayLength=10))
  
})
