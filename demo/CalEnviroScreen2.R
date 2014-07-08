library(CalEnviroScreen)
library(dplyr)
library(tidyr)

data(CalEnviroScreen2, package="CalEnviroScreen")

# Indicator values
(CES2_values <- CES2_data %>% select(-Percentile) %>% 
  spread(Variable, Value))

# Calculation of scores
(CES2_scores <- CES2_data %>%
  inner_join(CES2_metadata, by = "Variable") %>%
  group_by(FIPS, Group) %>%
  compute_CES2_subscores(min_obs = 4) %>%
  spread(Group, Subscore) %>% 
  compute_CES2_scores() %>% 
  arrange(desc(Score)))

# Example tract
CES2_scores %>% filter(FIPS == "06071004900")

# Calculation of percentiles
CES2_values %>% 
  gather(Variable, Value, -FIPS) %>%
  group_by(Variable) %>%
  mutate(Rank = ranks(Value),
         Percentile = percentiles(Value))

