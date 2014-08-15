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
  
  pctls <- CES2_data %>% 
    mutate_each(funs(pctl), -FIPS)
  
  published_pctls <- CES2_pctls
  
  expect_equal(
    pctls %>% round(digits = 8),
    published_pctls %>% round(digits = 8))
  
})

#CES2_VARS <- c(CES2_POLLUTION_VARS, CES2_POPCHAR_VARS)
#CES2_meta_tbl <- as.tbl(data.frame(Variable = CES2_VARS)) %>%
#  mutate(Group = factor(ifelse(Variable %in% CES2_POPCHAR_VARS, "PopChar", "Pollution")),
#         Weight = CES2_WEIGHTS[CES2_VARS])

test_that("subscores agree", {
  
  subscores <- CES2_pctls %>% 
    gather(Variable, Pctl, -FIPS) %>%
    inner_join(CES2_meta_tbl, by = "Variable") %>%
    group_by(FIPS, Group) %>%
    calc_CES2_subscores(min_obs = 4) %>%
    arrange(desc(FIPS))
  
  published_subscores <- CES2_scores %>%
    select(FIPS, Pollution, PopChar) %>%
    arrange(desc(FIPS))
  
  expect_equal(
    subscores %>% round(digits = 8), 
    published_subscores %>% round(digits = 8))
  
})

test_that("scores agree", {
  
  scores <- subscores %>% 
    compute_CES2_scores() %>%
    arrange(desc(FIPS))
  
  published_scores <- CES2_scores %>% 
    arrange(desc(FIPS))
  
  expect_equal(
    scores %>% select(FIPS, Score) %>% round(digits = 8),
    published_scores %>% select(FIPS, Score) %>% round(digits = 8))
  
})

test_that("ranges agree", {
  
  expect_equal(
    scores %>% select(FIPS, Range),
    published_scores %>% select(FIPS, Range))
  
})