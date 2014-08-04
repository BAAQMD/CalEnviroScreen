# Here we check that our method for calculating percentiles of score components will yield results
# consistent with what is in the published XLS file.

library(CalEnviroScreen)
library(testthat)
library(tidyr)

data(CES2, package="CalEnviroScreen")

round.tbl <- function (.data, digits) {
  f <- function (x) round(x, digits = digits)
  .data %>% mutate_each(funs(f), -FIPS)
}

context("CES2 pctls (to 8 digits)")
expect_equal(
  round(CES2_pctls, 8), 
  round(CES2_data %>% mutate_each(funs(pctl), -FIPS), 8)
)
