library(testthat)
library(dplyr)
library(CalEnviroScreen)

data(CES2_data, package = "CalEnviroScreen")

context("CES2 data (example tract)")

# Check that raw values agree for the example tract given in OEHHA documentation 
# FIXME: rounding error: 
# Traffic in OEHHA example worksheet is "1484.8"
# HazWst in OEHHA example worksheet is "0.73"

is_approximately <- function (value) equals(value, tolerance=0.005, scale=1)

example_FIPS <- "06071004900"
example_data <- subset(CES2_data, FIPS == example_FIPS)

with(example_data, {
  
  expect_that(Ozone,    is_approximately(0.79))
  expect_that(PM25,     is_approximately(14.68))
  expect_that(DieselPM, is_approximately(23.35))
  expect_that(DrinkWat, is_approximately(64.34))
  expect_that(PestUse,  equals(0))
  expect_that(ToxRel,   is_approximately(851.43))
  expect_that(Traffic,  is_approximately(1484.85))
  
  expect_that(Cleanup,  is_approximately(21.3))
  expect_that(GndWat,   is_approximately(5.75))
  expect_that(HazWaste, is_approximately(0.735))
  expect_that(ImpWat,   equals(1))
  expect_that(SolWaste, equals(0))
  
  expect_that(Age,      is_approximately(25.9))
  expect_that(Asthma,   is_approximately(104.45))
  expect_that(LBW,      is_approximately(0.05))
  
  expect_that(Edu,      equals(54))
  expect_that(LingIso,  is_approximately(26.1))
  expect_that(Poverty,  is_approximately(70.5))
  expect_that(Unemp,    is_approximately(19.84))
  
})
