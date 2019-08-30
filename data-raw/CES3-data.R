library(devtools); load_all()

library(tidyverse)
library(stringr)
library(readxl)
library(purrr)
library(ensurer)

pkg_path <- function (...) 
  normalizePath(file.path("~/Dropbox/R-packages/CalEnviroScreen", ...))

f <- function (x) map(x, . %>% c(., str_c(., " Pctl"))) %>% unlist()

CES3_POLLUTION_METRICS <- c(
  "Ozone",
  "PM2.5",
  "Diesel PM",
  "Drinking Water",
  "Pesticides",
  "Tox. Release",
  "Traffic",
  "Cleanup",
  "Groundwater",
  "Haz. Waste",
  "Imp. Water Bodies",
  "Solid Waste")

CES3_POPULATION_METRICS <- c(
  "Asthma",
  "Low Birth Weight",
  "Cardiovascular Disease",
  "Education",
  "Linguistic Isolation",
  "Poverty",
  "Unemployment",
  "Rent-Adjusted Income")

CES3_COLNAMES <- c(
  
  "FIPS" = "Census Tract",
  "Population" = "Total Population",
  "County" = "California County",
  "ZIP" = "ZIP",
  "City" = "City",
  "Longitude" = "Longitude",
  "Latitude" = "Latitude",
  
  "Score" = "DRAFT CES 3.0 Score",
  "PctlRange" = "DRAFT CES 3.0 Percentile Range",
  
  f(CES3_POLLUTION_METRICS),
  "Pollution Burden",
  "Pollution Burden Score",
  "Pollution Burden Pctl",
  
  f(CES3_POPULATION_METRICS),
  "Pop. Char.",
  "Pop. Char. Score",
  "Pop. Char. Pctl"
  
)

CES3_COLTYPES <- 
  rep("text", length(CES3_COLNAMES)) %>%
  setNames(CES3_COLNAMES)

CES3_COLTYPES[c(f(CES3_POLLUTION_METRICS), f(CES3_POPULATION_METRICS))] <- 
  "numeric"

REGION_LEVELS <- 
  c("Bay Area", "South Coast", "San Joaquin", "Other")

CES_region_data <-
  CalEnviroScreen::CA_regions %>% 
  map_df(. %>% .$FIPS %>% data_frame(FIPS = .), .id = "Region") %>%
  bind_rows(data_frame(Region = "South Coast", FIPS = "06037930401")) %>%
  distinct() %>%
  mutate(Region = factor(Region, REGION_LEVELS))

CES3_tbl <- local({

  xls_data <-
    pkg_path("data-raw/ces30draftresults.xlsx") %>%
    read_excel(sheet = "CES3.0DRAFTresults",
               na = "NA",
               skip = 1,
               col_names = CES3_COLNAMES,
               col_types = CES3_COLTYPES)
  
  renamed <- xls_data %>%
    rename_(.dots = CES3_COLNAMES %>% keep(nchar(names(.)) > 0) %>% sapply(as.name)) 
  
  parsed <- renamed %>% mutate_each(
    funs(parse_number), 
    Population, 
    Longitude, Latitude,
    Score, 
    one_of(CES3_POLLUTION_METRICS), 
    one_of(CES3_POPULATION_METRICS),
    dplyr::matches(" Pctl")) 
  
  parsed %>%
    mutate(FIPS = str_replace(FIPS, "^6", "06")) %>%
    left_join(CES_region_data, by = "FIPS") %>%
    ensure(nrow(.) == 8035) %>%
    ensure(!any(is.na(.$Region))) %>%
    arrange(desc(FIPS))

})

library(formattable)

TOP_PCTL_RANGES <- 
  c("76-80%", "81-85%", "86-90%", "91-95%", "96-100% (highest scores)")

TOP_SCORE_CUTOFF <- 
  with(CES3_tbl, quantile(Score, 0.75, na.rm = TRUE))

is_true <- function (x) sapply(x, isTRUE)

regional_tally <-
  CES3_tbl %>%
  group_by(Region) %>%
  mutate(is_DAC_PctlRange = is_true(PctlRange %in% TOP_PCTL_RANGES)) %>%
  mutate(is_DAC_Score = is_true(Score > TOP_SCORE_CUTOFF)) %>%
  ensure(all.equal(.$is_DAC_Score, .$is_DAC_PctlRange)) %>%
  summarise(
    Tracts = n(), 
    DACs = sum(is_DAC_Score, na.rm = TRUE)) %>%
  mutate(
    Tracts = accounting(Tracts, digits = 0),
    DACs = accounting(DACs, digits = 0),
    Share = percent(DACs / sum(DACs), digits = 1))

regional_tally %>% show()
regional_tally %>% copydat::Copy()

CES3_CODEC <- c(
  PM25 = "PM2.5",
  ToxRel = "Tox. Release",
  DieselPM = "Diesel PM",
  Traffic = "Traffic",
  DrinkWat = "Drinking Water",
  Ozone = "Ozone",
  PestUse = "Pesticides",
  HazWaste = "Haz. Waste",
  Cleanup = "Cleanup",
  GndWat = "Groundwater",
  SolWaste = "Solid Waste",
  ImpWat = "Imp. Water Bodies",
  LBW = "Low Birth Weight",
  LingIso = "Linguistic Isolation",
  Unemp = "Unemployment",
  Asthma = "Asthma",
  Poverty = "Poverty",
  Edu = "Education",
  Rent = "Rent-Adjusted Income",
  CVD = "Cardiovascular Disease")

CES3_data <- select_(CES3_tbl, "FIPS", .dots = sapply(CES3_CODEC, as.name))
CES3_pctls <- select_(CES3_tbl, "FIPS" ,.dots = sapply(CES3_CODEC, . %>% str_c(" Pctl") %>% as.name))

CES3_scores <- transmute(
  CES3_tbl, FIPS, County, 
  Lat = parse_number(Latitude), 
  Lon = parse_number(Longitude),
  Pollution = parse_number(`Pollution Burden Score`),
  PopChar = parse_number(`Pop. Char. Score`),
  Score = parse_number(Score), 
  Range = PctlRange)

CES3_POPCHAR_VARS <- c(
  "CVD", "Rent", "LBW", "LingIso", "Unemp", "Asthma", "Poverty", "Edu")

CES3_POLLUTION_VARS <- c(
  "PM25", "ToxRel", "DieselPM", "Traffic", "DrinkWat", "Ozone", 
  "PestUse", "HazWaste", "Cleanup", "GndWat", "SolWaste", "ImpWat")

ENV_WT <- 0.5 # half-weighting for "environmental exposures"

CES3_WEIGHTS <- c(
  Ozone = 1, PM25 = 1, DieselPM = 1, DrinkWat = 1, PestUse = 1, ToxRel = 1, Traffic = 1,
  Cleanup = ENV_WT, GndWat = ENV_WT, HazWaste = ENV_WT, ImpWat = ENV_WT, SolWaste = ENV_WT,
  set_names(rep(1.0, length(CES3_POPCHAR_VARS)), CES3_POPCHAR_VARS))

CES3_VARS <- c(CES3_POLLUTION_VARS, CES3_POPCHAR_VARS)
stopifnot(setequal(CES3_VARS, names(CES3_WEIGHTS)))

devtools::use_data(
  CES_region_data,
  CES3_data,
  CES3_pctls,
  CES3_scores,
  CES3_VARS,
  CES3_WEIGHTS,
  CES3_POPCHAR_VARS,
  CES3_POLLUTION_VARS,
  overwrite = TRUE)
