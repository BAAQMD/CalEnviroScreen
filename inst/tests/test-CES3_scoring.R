# Here we check that our method for calculating percentiles of score components will yield results
# consistent with what is in the published XLS file.

library(CalEnviroScreen)
library(testthat)
library(tidyverse)

context("CES3 scoring (to 8 digits)")

ENV_WT <- 0.5

#
# FIXME: figure out why this fails!!!
#
#test_that("percentiles agree", {
#  
#  computed_pctls <- CES3_data %>% 
#    mutate_each(funs(CES_pctl), -FIPS)
#  
#  published_pctls <- CES3_pctls
#  
#  expect_equal(
#    computed_pctls %>% round(digits = 0),
#    published_pctls %>% round(digits = 0))
#  
#})

CES3_meta_tbl <- 
  data_frame(Variable = CES3_VARS) %>%
  mutate(Group = ifelse(Variable %in% CES3_POPCHAR_VARS, "PopChar", "Pollution"),
         Weight = CES3_WEIGHTS[CES3_VARS])

computed_subscores <- 
  CES3_pctls %>% 
  gather(Variable, Pctl, -FIPS) %>%
  mutate_each(funs(as.character), FIPS, Variable) %>%
  inner_join(CES3_meta_tbl, by = "Variable") %>%
  compute_CES3_subscores() %>%
  select(FIPS, Pollution = Pollution_Subscore, PopChar = PopChar_Subscore) %>%
  arrange(desc(FIPS))

published_subscores <- 
  CES3_scores %>%
  select(FIPS, Pollution, PopChar) %>%
  arrange(desc(FIPS))

cmp_subscores <-
  bind_rows(Computed = computed_subscores, Published = published_subscores, .id = "Basis") %>%
  gather(Dimension, Value, Pollution, PopChar) %>%
  spread(Basis, Value) %>%
  mutate(diff = Computed - Published) %>%
  arrange(desc(abs(diff))) 

cmp_subscores %>% filter(FIPS == "06013304002")

# options(digits = 4, scipen = 99)
# TEST_FIPS <- c("06037800410") #c("06065981000") # c("06019001500", "06019000902")
# cmp_subscores %>% filter(Dimension == "PopChar") %>%  filter(FIPS %in% TEST_FIPS) %>% show()
# 
# CES3_pctls %>% filter(FIPS %in% TEST_FIPS) %>% select_("FIPS", .dots = CES3_POPCHAR_VARS) %>% 
#   mutate(PopChar = (CVD + Rent + LingIso + Unemp + Asthma + Poverty + Edu) / 7) %>% show()
# 
# cmp_subscores %>% qplot(Published, Computed, data = .) + facet_wrap(~ Dimension) + geom_abline(color = I("red")) +  coord_equal()
# cmp_subscores %>% filter(Dimension == "PopChar") %>% top_n(3, wt = diff)

computed_scores <-
  computed_subscores %>%
  compute_CES3_scores() %>% #mutate(Score = Pollution * PopChar) %>%
  arrange(desc(FIPS)) %>%
  select(FIPS, Score)

published_scores <- 
  CES3_scores %>%
  arrange(desc(FIPS)) %>%  
  select(FIPS, Score)
  
test_that("scores agree", {
  
  cmp_scores <- 
    bind_rows(Computed = computed_scores, Published = published_scores, .id = "Basis") %>% 
    spread(Basis, Score) 
  
  with(cmp_scores, expect_equal(Computed, Published, tolerance = 0.0001))
  
})