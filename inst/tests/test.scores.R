#
# Here we check that our method for calculating scores (from percentiles) will yield results
# consistent with what is in the published XLS file.
#

context("CES2 scores (all tracts)")

library(testthat)

computed_subscores <- CES2_data %>%
    inner_join(CES2_metadata, by = "Variable") %>%
    group_by(FIPS, Group) %>%
    compute_CES2_subscores() %>%
    spread(Group, Score) %>%
    arrange(desc(Pollution))

expected_subscores <- as.tbl(CES2_xls) %>% 
  select(FIPS, Pollution = PollutionBurdenScore, PopChar = PopCharScore) %>%
  arrange(desc(Pollution))

expect_that(
  mutate(computed_subscores, Pollution = round(Pollution, 4), PopChar = round(PopChar, 4)),
  equals(expected_subscores))

computed_scores <- computed_subscores %>% 
  compute_CES2_scores() %>%
  select(FIPS, Score, PercentileRange) %>%
  filter(!is.na(Score)) %>%
  arrange(desc(Score))

expected_scores <- as.tbl(CES2_xls) %>% 
  select(FIPS, Score = CES20Score, PercentileRange) %>%
  filter(!is.na(Score)) %>%
  arrange(desc(Score))

expect_that(
  mutate(computed_scores, Score = round(Score, 8)),
  equals(expected_scores))
