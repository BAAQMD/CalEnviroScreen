#
# Here we check that our method for calculating scores (from percentiles) will yield results
# consistent with what is in the published XLS file.
#

library(testthat)
library(dplyr)
library(tidyr)

library(CalEnviroScreen)
data(CES2, package="CalEnviroScreen")
data(CES2_metadata, package="CalEnviroScreen")

round.tbl <- function (.data, digits) {
  f <- function (x) round(x, digits = digits)
  .data %>% mutate_each(funs(f), -FIPS)
}

CES2_VARS <- c(CES2_POLLUTION_VARS, CES2_POPCHAR_VARS)
CES2_meta_tbl <- as.tbl(data.frame(Variable = CES2_VARS)) %>%
  mutate(Group = factor(ifelse(Variable %in% CES2_POPCHAR_VARS, "PopChar", "Pollution")),
         Weight = CES2_WEIGHTS[CES2_VARS])

context("CES2 subscores (to 4 digits)")

published_subscores <- CES2_scores %>%
  select(FIPS, Pollution, PopChar) %>%
  arrange(desc(FIPS))

computed_subscores <- CES2_pctls %>% 
  gather(Variable, Pctl, -FIPS) %>%
  inner_join(CES2_meta_tbl, by = "Variable") %>%
  group_by(FIPS, Group) %>%
  compute_CES2_subscores(min_obs = 4) %>%
  spread(Group, Subscore) %>%
  arrange(desc(FIPS))

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
  published_scores %>% select(FIPS, Range),
  computed_scores %>% select(FIPS, Range)
)
