# Here we check that our method for calculating percentiles of score components will yield results
# consistent with what is in the published XLS file.

context("CES2 percentiles (all tracts)")

library(testthat)
library(dplyr)

expected_percentiles <- local({
  
  column_metadata <- CES2_metadata %>%
    mutate(ExcelColumn = str_c(ExcelColumn, "Pctl"))
  
  xls_percentiles <- as.tbl(CES2_xls) %>% 
    select(FIPS, ends_with("Pctl")) %>%
    gather(ExcelColumn, Percentile, -FIPS) %>%
    filter(Percentile != "NA")
  
  xls_percentiles %>%
    inner_join(column_metadata, by = c("ExcelColumn")) %>%
    mutate(Percentile = as.numeric(Percentile)) %>%
    select(FIPS, ExcelColumn, Variable, Expected=Percentile)
  
})

calculated_percentiles <- CES2_data %>% group_by(Variable) %>%
  select(FIPS, Variable, Value) %>%
  mutate(Calculated = percentiles(Value, which(Value > 0)))

with(
  left_join(expected_percentiles, calculated_percentiles, by=c("FIPS", "Variable")),
  expect_that(Calculated, equals(Expected))
)
