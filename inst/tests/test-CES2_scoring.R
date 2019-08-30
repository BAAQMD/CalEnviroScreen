# Here we check that our method for calculating percentiles of score components will yield results
# consistent with what is in the published XLS file.

library(CalEnviroScreen)
library(testthat)
library(tidyr)

context("CES2 scoring (to 8 digits)")

data(CES2, package = "CalEnviroScreen")

round.tbl <- function (.data, digits) {
  f <- function (x) round(x, digits = digits)
  .data %>% mutate_each(funs(f), -FIPS)
}

test_that("percentiles agree", {
  
  computed_pctls <- CES2_data %>% 
    mutate_each(funs(CES_pctl), -FIPS)
  
  published_pctls <- CES2_pctls
  
  expect_equal(
    computed_pctls %>% round(digits = 8),
    published_pctls %>% round(digits = 8))
  
})

# Relevel based on correlations with PopChar and Pollution, respectively
CES2_POPCHAR_VARS <- c(
  "Age", "LBW", "LingIso", "Unemp", "Asthma", "Poverty", "Edu")

CES2_POLLUTION_VARS <- c(
  "PM25", "ToxRel", "DieselPM", "Traffic", "DrinkWat", "Ozone", 
  "PestUse", "HazWaste", "Cleanup", "GndWat", "SolWaste", "ImpWat")

CES2_VARS <- c(CES2_POLLUTION_VARS, CES2_POPCHAR_VARS)

CES2_meta_tbl <- 
  data_frame(Variable = CES2_VARS) %>%
  mutate(Group = ifelse(Variable %in% CES2_POPCHAR_VARS, "PopChar", "Pollution"),
         Weight = CES2_WEIGHTS[CES2_VARS])

computed_subscores <- 
  CES2_pctls %>% 
  gather(Variable, Pctl, -FIPS) %>%
  mutate_each(funs(as.character), FIPS, Variable) %>%
  inner_join(CES2_meta_tbl, by = "Variable") %>%
  compute_CES2_subscores(min_obs = 4) %>%
  select(FIPS, Pollution = Pollution_Subscore, PopChar = PopChar_Subscore) %>%
  arrange(desc(FIPS))

test_that("subscores agree", {
  
  published_subscores <- CES2_scores %>%
    select(FIPS, Pollution, PopChar) %>%
    arrange(desc(FIPS))
  
  expect_equal(
    computed_subscores %>% round(digits = 8), 
    published_subscores %>% round(digits = 8))
  
})

computed_scores <- 
  computed_subscores %>% 
  compute_CES2_scores() %>%
  arrange(desc(FIPS))

test_that("scores and ranges agree", {
  
  published_scores <- 
    CES2_scores %>% 
    arrange(desc(FIPS))
  
  expect_equal(
    computed_scores %>% select(FIPS, Score) %>% round(digits = 8),
    published_scores %>% select(FIPS, Score) %>% round(digits = 8))

  expect_equivalent(
    computed_scores %>% select(FIPS, Range) %>% mutate(Range = as.character(Range)),
    published_scores %>% select(FIPS, Range) %>% mutate(Range = str_extract(Range, "[0-9]+-[0-9]+%")))
  
})