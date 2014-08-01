#
# Here we check that our method for calculating scores (from percentiles) will yield results
# consistent with what is in the published XLS file.
#

library(CalEnviroScreen)
library(testthat)
library(dplyr)
library(tidyr)

round.tbl <- function (.data, digits) {
  f <- function (x) round(x, digits = digits)
  .data %>% mutate_each(funs(f), -FIPS)
}

computed_subscores <- CES2_data %>% ungroup() %>%
    inner_join(CES2_metadata, by = "Variable") %>%
    group_by(FIPS, Group) %>%
    compute_CES2_subscores(min_obs = 4) %>%
    spread(Group, Subscore) %>%
    arrange(desc(FIPS))

published_subscores <- CES2_scores %>%
  select(FIPS, Pollution, PopChar) %>%
  arrange(desc(FIPS))

context("CES2 subscores (to 4 digits)")

expect_equal(round(computed_subscores, 4), round(published_subscores, 4))

computed_scores <- computed_subscores %>% 
  compute_CES2_scores() %>%
  arrange(desc(FIPS))

published_scores <- CES2_scores %>% 
  arrange(desc(FIPS))

context("CES2 scores (to 8 digits)")

expect_equal(
  published_scores %>% select(FIPS, Score), 
  computed_scores %>% select(FIPS, Score) %>% round(digits=8)
)

context("CES2 percentile ranges")

expect_equal(
  published_scores %>% select(FIPS, PercentileRange),
  computed_scores %>% select(FIPS, PercentileRange)
)
