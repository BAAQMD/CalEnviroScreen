# Here we check that our method for calculating percentiles of score components will yield results
# consistent with what is in the published XLS file.

library(CalEnviroScreen)
library(testthat)
library(tidyr)

data(CES2, package="CalEnviroScreen")

.spread <- function (.data, ...) {
  .data %>% 
    select(FIPS, Variable, ...) %>% 
    spread(Variable, ...) %>%
    arrange(desc(FIPS))
}

round.tbl <- function (.data, digits) {
  f <- function (x) round(x, digits = digits)
  .data %>% mutate_each(funs(f), -FIPS)
}

context("CES2 pctls (to 8 digits)")

published_pctls <- CES2_data %>% .spread(Pctl)
computed_pctls <- CES2_data %>% .spread(Value) %>% mutate_each(funs(pctl), -FIPS)

expect_equal(round(published_pctls, 8), round(computed_pctls, 8))
