#' @include utils.R

#' Calculate ranks (as in CES 2.x)
#'
#' @param x values
#' @param i (optional) expression indicating which values to include in the calculation 
#' @param \dots further arguments to base::rank
#' 
#' 
#' @rdname CES2
#' @export
CES2_rank <- function (x, i=which(x > 0), ...) {
  r <- base::rank(x[i], ties.method = "max", ...)
  replace(x, i, r)
}

#' Calculate percentiles (as in CES 2.x)
#'
#' @param x values
#' @param \dots further arguments to \code{CES2_rank}
#' 
#' @rdname CES2
#' @export
CES2_pctl <- function (x, ...) {
  r <- CES2_rank(x, ...)
  r / max(r, na.rm=TRUE) * 100
}

#' Compute subscores (as in CES 2.x)
#' 
#' @param .data data frame
#' @param min_obs minimum number of observations in a group (else NA)
#' @param unit_var character
#' @param group_var character
#' @param \dots ignored
#' 
#' @rdname CES2
#' @export
compute_CES2_subscores <- function (.data, min_obs, unit_var = "FIPS", group_var = "Group", ...) {
  
  .data %>% 
    group_by_(.dots = c(unit_var, group_var)) %>%
    filter(!is.na(Pctl)) %>% 
    summarise(Avg = ifelse(n() < min_obs, NA, weighted.mean(Pctl, Weight, na.rm = TRUE))) %>%
    ungroup() %>%
    spread_(key_col = group_var, value_col = "Avg") %>%
    mutate_each(funs(Subscore = 10 * CES_rescale(., digits = 3, lower = 0)), Pollution, PopChar)
}

#' Compute scores (as in CES 2.x)
#' 
#' @param .data data frame
#' @param breaks named numeric vector
#' 
#' @rdname CES2
#' @export
compute_CES2_scores <- function (.data, breaks = CES_PERCENTILE_BREAKS) {
  .data %>% mutate(
    Score = Pollution * PopChar,
    Percentile = 100 * CES_rescale(CES2_rank(Score) - 1),
    Range = CES_bin_ranges(Percentile, breaks))
}

