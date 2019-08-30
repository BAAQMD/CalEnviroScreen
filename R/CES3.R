#' @include utils.R

#' Calculate ranks (as in CES3)
#'
#' @param x values
#' @param i (optional) expression indicating which values to include in the calculation 
#' @param \dots further arguments to base::rank
#' 
#' @rdname CES3
#' @export
CES3_rank <- function (x, i=which(x > 0), ...) {
  r <- base::rank(x[i], ties.method = "max", ...)
  replace(x, i, r)
}

#' Calculate percentiles (as in CES3)
#'
#' @param x values
#' @param \dots further arguments to \link{CES3_rank}
#' 
#' @rdname CES3
#' @export
CES3_pctl <- function (x, ...) {
  r <- CES3_rank(x, ...)
  r / max(r, na.rm=TRUE) * 100
}

#' To compute CES3 subscores
#' 
#' @param .data a dplyr::tbl
#' @param min_obs minimum number of observations in a group (else NA)
#' @param unit_var character
#' @param group_var character
#' @param \dots ignored
#' 
#' @rdname CES3
#' @export
compute_CES3_subscores <- function (.data, min_obs = 5, unit_var = "FIPS", group_var = "Group", ...) {
  
  .data %>% 
    group_by_(.dots = c(unit_var, group_var)) %>%
    filter(!is.na(Pctl)) %>% 
    summarise(Avg = ifelse(n() < min_obs, NA, weighted.mean(Pctl, Weight, na.rm = TRUE))) %>%
    ungroup() %>%
    spread_(key_col = group_var, value_col = "Avg") %>%
    mutate_each(funs(Subscore = 10 * CalEnviroScreen:::CES_rescale(., digits = 8, lower = 0)), Pollution, PopChar) 
  
}

#' Compute scores (as in CES 3.0)
#' 
#' @param .data data frame
#' @param breaks named numeric vector
#' 
#' @rdname CES3
#' @export
compute_CES3_scores <- function (.data, breaks = CES_PERCENTILE_BREAKS) {
  .data %>% mutate(
    Score = Pollution * PopChar,
    Percentile = 100 * CES_rescale(CES3_rank(Score) - 1),
    Range = CES_bin_ranges(Percentile, breaks))
}


