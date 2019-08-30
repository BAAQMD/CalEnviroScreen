CES2_POPCHAR_VARS <- c(
  "Age", "LBW", "LingIso", "Unemp", "Asthma", "Poverty", "Edu")

CES2_POLLUTION_VARS <- c(
  "PM25", "ToxRel", "DieselPM", "Traffic", "DrinkWat", "Ozone", "PestUse", "HazWaste", "Cleanup", "GndWat", "SolWaste", "ImpWat")

CES2_VARS <- 
  c(CES2_POLLUTION_VARS, CES2_POPCHAR_VARS)

devtools::use_data(
  CES2_POLLUTION_VARS,
  CES2_POPCHAR_VARS,
  overwrite = TRUE)