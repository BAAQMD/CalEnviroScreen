library(testthat)
library(dplyr)
library(CalEnviroScreen)

data(CES3, package = "CalEnviroScreen")

context("CES3 data (for example tract)")

# Check that raw values agree for the example tract given in OEHHA documentation
# FIXME: rounding error:
# Traffic in OEHHA example worksheet is "1484.8"
# HazWst in OEHHA example worksheet is "0.73"

is_approximately <- function (value) testthat::equals(value, tolerance=0.005, scale=1)

example_FIPS <- "06071004900"
example_data <- subset(CES3_data, FIPS == example_FIPS)
example_scores <- subset(CES3_scores, FIPS == example_FIPS)

with(example_data, {

  expect_that(Ozone,    is_approximately(0.0649))
  expect_that(PM25,     is_approximately(12.47))  # was: 14.68
  expect_that(DieselPM, is_approximately(32.45))
  expect_that(DrinkWat, is_approximately(855.33))  # was: 64.34
  expect_that(PestUse,  equals(0))
  expect_that(ToxRel,   is_approximately(1111.57))
  expect_that(Traffic,  is_approximately(847.17))

  expect_that(Cleanup,  is_approximately(16.1))
  expect_that(GndWat,   is_approximately(1.0))
  expect_that(HazWaste, is_approximately(2.3))
  expect_that(ImpWat,   equals(1))
  expect_that(SolWaste, equals(0.2))

  expect_that(CVD,      is_approximately(15.23))
  expect_that(Rent,     is_approximately(16699))
  expect_that(LBW,      is_approximately(5.97))

  expect_that(Edu,      is_approximately(60.2))
  expect_that(LingIso,  is_approximately(24.0))
  expect_that(Poverty,  is_approximately(74.5))
  expect_that(Unemp,    is_approximately(21.6))

})