library(dplyr)
library(tidyr)
library(CalEnviroScreen)

data(CES2, package="CalEnviroScreen")

library(testthat)

# (Re-)construct metadata
CES2_group_data <- bind_rows(
  Pollution = data_frame(Variable = CES2_POLLUTION_VARS), # note: names() carry information too
  PopChar = data_frame(Variable = CES2_POPCHAR_VARS),  # note: names() carry information too
  .id = "Group")

CES2_weight_data <- CES2_WEIGHTS %>% data_frame(Variable = names(.), Weight = .)

CES2_metadata <- left_join(CES2_group_data, CES2_weight_data, by = "Variable")
expect_identical(names(CES2_metadata), c("Group", "Variable", "Weight"))

CES2_values <- CES2_data
tol <- 0.001

# Check that `pctl` computes perecntiles as expected
list(object = CES2_values %>% mutate_each(funs(pctl), -FIPS), expected = CES2_pctls) %>%
  lapply(. %>% mutate_each(funs(round(., digits = -log10(tol))), -FIPS)) %>%
  do.call(expect_identical, .)

# Computation of scores
computed_scores <-
  CES2_pctls %>% #CES2_data %>%
  gather(Variable, Pctl, -FIPS) %>%
  inner_join(CES2_metadata, by = "Variable") %>%
  group_by(FIPS, Group) %>%
  filter(!is.na(Pctl)) %>%
  compute_CES2_subscores(min_obs = 4) %>%
  compute_CES2_scores() %>%
  arrange(desc(Score))

# Example tract
computed_scores %>% filter(FIPS == "06071004900")
CES2_scores %>% filter(FIPS == "06071004900")

# Table of ranks
(CES2_ranks <- CES2_values %>% mutate_each(funs(comp_rank), -FIPS))

cut_quantile <- function (x, ...) {
  q <- quantile(x, seq(0, 1, len=21))
  cut(x, breaks = q, labels = names(q)[-1])
}

# Rank product
summarise_ranks <- function (.data) {
  .data %>%
    group_by(Variable) %>%
    mutate(Rank = rank(-Value) + 1, Frac = Rank / n()) %>%
    ungroup() %>% group_by(FIPS) %>%
    summarise(logRP = sum(-log(Frac)))
}

rank_tbl <- CES2_data %>%
  gather(Variable, Value, -FIPS) %>%
  summarise_ranks() %>% ungroup() %>%
  mutate(QlogRP = cut_quantile(logRP)) %>%
  with_region()

shade_polygons <- function (spobj, col = gray(0.6), add = TRUE, ...) {
  plot(spobj, col = col, border = NA, add = add, ...)
}

draw_outlines <- function (spobj, border = "#BB0000", add = TRUE, ...) {
  plot(spobj, border = border, add = add, ...)
}

#rank_data <- data.frame(rank_tbl, row.names = rank_tbl$FIPS)
#geom <- spTransform(CA_tracts, CRS("+proj=longlat +datum=WGS84"))
#i <- intersect(IDs(geom), row.names(rank_data))
#CES2_tracts <- SpatialPolygonsDataFrame(geom[i,], rank_data[i,])
CES2_tracts <- merge(CA_tracts, region_tbl, by = "FIPS")

par(mar = c(0, 0, 0, 0))
rgn <- "Bay Area"
top_FIPS <- filter(rank_tbl, logRP > quantile(logRP, 0.80))$FIPS
shade_polygons(subset(CES2_tracts, Region == rgn), gray(0.8), add = FALSE)
shade_polygons(subset(CES2_tracts, Region == rgn & FIPS %in% top_FIPS))

#rgdal::writeOGR(subset(CES2_tracts, FIPS %in% top_FIPS), dsn="CES2-tracts-top20", layer="RankProd", driver="GeoJSON", overwrite_layer=TRUE)

#library(plotKML)
#plotKML(CES2_tracts)
