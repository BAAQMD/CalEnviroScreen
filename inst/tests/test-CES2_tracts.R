# Here we check that our method for calculating percentiles of score components will yield results
# consistent with what is in the published XLS file.

library(CalEnviroScreen)
library(dplyr)
library(testthat)
library(tidyr)

context("CES2 tracts")
data(CES2, package = "CalEnviroScreen")

# FIPS codes of selected (test case) tracts
TEST_CASES <- c(
  "Long Beach" = "06037576402",
  "San Francisco" = "06075023103")

CES2_data %>%
  filter(FIPS %in% TEST_CASES)

CES2_pctls %>%
  filter(FIPS %in% TEST_CASES)

CES2_scores %>%
  filter(FIPS %in% TEST_CASES)