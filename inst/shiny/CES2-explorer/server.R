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
  library(shinyIncubator)
  library(CalEnviroScreen)
})

set.seed(0)

data(CES2, package = "CalEnviroScreen")
data(CES2_metadata, package = "CalEnviroScreen")
data(California, package = "CalEnviroScreen")

CES2_VARS <- c(CES2_POLLUTION_VARS, CES2_POPCHAR_VARS)

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

region_tbl <- do.call(rbind, lapply(names(CA_regions), function (x) as.tbl(data.frame(FIPS = as.character(CA_regions[[x]]$FIPS), Region = x))))
with_region <- function (.data) .data %>% inner_join(region_tbl, by = "FIPS")

###############################################################################
# Define server logic
###############################################################################

shinyServer(function(input, output, session) {
  
  .map_tracts <- reactive({
    #subset(CA_tracts, Region == input$region_name)
    fips <- filter(tract_tbl, Region == input$region_name)$FIPS
    CA_tracts[fips,]
  })
  
  .map_region <- reactive({
    region <- CA_regions[[input$region_name]]
    region$boundary
  })
  
  .variables <- reactive({
    c(input$pollution_vars, input$popchar_vars)
  })
  
  .pollution_weights <- reactive({
    c(Ozone=input$Ozone, PM25=input$PM25, DieselPM=input$DieselPM, DrinkWat=input$DrinkWat, PestUse=input$PestUse, ToxRel=input$ToxRel, Traffic=input$Traffic,
      Cleanup=input$Cleanup, GndWat=input$GndWat, HazWaste=input$HazWaste, ImpWat=input$ImpWat, SolWaste=input$SolWaste)
  })
  
  .popchar_weights <- reactive({
      c(Age=input$Age, Asthma=input$Asthma, LBW=input$LBW, Edu=input$Edu, LingIso=input$LingIso, Poverty=input$Poverty, Unemp=input$Unemp)
  })
  
  .group_tbl <- reactive({
    as.tbl(data.frame(Variable = c(CES2_POLLUTION_VARS, CES2_POPCHAR_VARS))) %>%
      mutate(Group = factor(ifelse(Variable %in% CES2_POPCHAR_VARS, "PopChar", "Pollution"))
  })
  
  .weight_tbl <- reactive({
    w <- c(.pollution_weights(), .popchar_weights())
    as.tbl(data.frame(Variable = names(w), Weight = w))
  })
  
  .pctls_tbl <- reactive({
    CES2_pctls %>% 
      gather(Variable, Pctl, -FIPS) %>%
      inner_join(.group_tbl(), by = "Variable") %>%
      group_by(FIPS, Group)  
  })
  
  .subscore_tbl <- reactive({
    .pctls_tbl() %>% 
      inner_join(.weight_tbl(), by = "Variable") %>%
      filter(Variable %in% .variables()) %>%
      compute_CES2_subscores(min_obs = 4) %>%
      spread(Group, Subscore) %>%
      arrange(desc(FIPS))
  })
  
  .score_tbl <- reactive({
    subscores <- .subscore_tbl() 
    if (is.null(subscores$PopChar)) {
      subscore_data$PopChar <- 1
    } else {
      if (is.null(subscores$Pollution)) 
        subscores$Pollution <- 1 
    }
    subscores %>% 
      compute_CES2_scores() %>%
      arrange(desc(Score)) %>%
      with_region()
  })
  
  .impacted_percentile <- reactive({
    100 - input$impacted_percentile # reverse the scale
  })
  
  .pollution_maximum <- reactive(with(.score_tbl(), max(Pollution, na.rm=TRUE)))
  .popchar_maximum <- reactive(with(.score_tbl(), max(PopChar, na.rm=TRUE)))
  .score_cutoff <- reactive(with(.score_tbl(), quantile(Score, .impacted_percentile() / 100, na.rm=TRUE)))
  .popchar_intercept <- reactive(.score_cutoff() / .pollution_maximum())
  
  .tally <- reactive({
    .score_tbl() %>% 
      group_by(Region) %>% 
      summarise(Tracts=n(), Yes=sum(Percentile > .impacted_percentile()), No=Tracts-Yes)
  })
  
  .scatterplot <- reactive({
    fig_tbl <- .score_tbl() %>% mutate(Sampled = as.logical(rbinom(n(), 1, prob = input$SampleTracts / 100)))
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
    fig <- fig + theme(legend.position="bottom", legend.direction="horizontal")
    return(fig)
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
    .score_tbl() %>% filter(Percentile > .impacted_percentile())
  })
  
  .map <- reactive({
    withProgress(session, {
      setProgress(message = "Calculating, please wait",
                  detail = "This may take a few moments...")
      i <- intersect(row.names(.map_tracts()), .impacted_scores()$FIPS)
      par(mar=c(0.1, 0.1, 0.1, 0.1))
      plot(.map_tracts()[i, ], col=gray(0), border=NA)
      plot(.map_region(), col="#88888888", border=NA, add=TRUE)
      dev.off()
    })
  })
  
  output$tally <- renderDataTable(.tally())
  
  output$map <- renderPlot(show(.map()), height=500)
  
  output$scatterplot <- renderPlot(show(.scatterplot()))
  
  output$barchart <- renderPlot(show(.barchart()))

  output$data <- renderDataTable(.impacted_scores() %>% 
                                   select(FIPS, Pollution, PopChar, Score, Pctl=Percentile, PctlRange=PercentileRange), 
                                 options = list(bSortClasses=TRUE, iDisplayLength=10))
  
})
