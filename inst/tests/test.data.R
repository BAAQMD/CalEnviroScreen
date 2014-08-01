library(testthat)

context("CES2 data (example tract)")

# Check that raw values agree for the example tract given in OEHHA documentation 
# FIXME: rounding error: 
# Traffic in OEHHA example worksheet is "1484.8"
# HazWst in OEHHA example worksheet is "0.73"

CES2_values <- CES2_data %>% 
  select(FIPS, Variable, Value) %>%
  spread(Variable, Value)

example_FIPS <- "06071004900"

example_values <- filter(CES2_values, FIPS == example_FIPS)

is_approximately <- function (value) equals(value, tolerance=0.005, scale=1)

with(example_values, {
  
  expect_that(Ozone,    is_approximately(0.79))
  expect_that(PM25,     is_approximately(14.68))
  expect_that(DieselPM, is_approximately(23.35))
  expect_that(DrinkWat, is_approximately(64.34))
  expect_that(PestUse,  equals(0))
  expect_that(ToxRel,   is_approximately(851.43))
  expect_that(Traffic,  is_approximately(1484.85))
  
  expect_that(Cleanup,  is_approximately(21.3))
  expect_that(GndWat,   is_approximately(5.75))
  expect_that(HazWst,   is_approximately(0.735))
  expect_that(WatBod,   equals(1))
  expect_that(SolWst,   equals(0))
  
  expect_that(Age,      is_approximately(25.9))
  expect_that(Asthma,   is_approximately(104.45))
  expect_that(LBW,      is_approximately(0.05))
  
  expect_that(Edu,      equals(54))
  expect_that(LingIso,  is_approximately(26.1))
  expect_that(Pov,      is_approximately(70.5))
  expect_that(Unemp,    is_approximately(19.84))
  
})
