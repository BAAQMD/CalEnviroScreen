#' To compute a weighted average
#' @param x numeric vector
#' @param w weights
#' @param na.rm logical
#' @export
weighted_average <- function (x, w, na.rm=TRUE) {
  sum(x * w, na.rm=na.rm) / sum(w, na.rm=na.rm)
}

#' To compute CES2 subscores (requires pre-calculated Pctl column)
#' @param .data a dplyr::tbl
#' @param min_obs minimum number of observations in a group (else NA)
#' @param \dots ignored
#' @export
compute_CES2_subscores <- function (.data, min_obs=4, ...) {
  .data %>% 
    filter(!is.na(Pctl)) %>%
    summarise(Subscore = ifelse(n() < min_obs, NA, weighted_average(Pctl, Weight) / 10))
}

#' To normalize a vector
#' @param x numeric
#' @param na.rm logical
#' @export
normalize <- function (x, na.rm=TRUE) {
  (x - min(x, na.rm=na.rm)) / max(x, na.rm=na.rm)
}

#' To bin percentiles
#' @param x vector
#' @param breaks breaks
#' @export
binned <- function (x, breaks) {
  cut(x, c(breaks, Inf), names(breaks), include.lowest=TRUE, ordered=TRUE)
}

#' To compute CES2 scores
#' @param .data a dplyr::tbl
#' @export
compute_CES2_scores <- function (.data) {
  .data %>% mutate(
    Score = Pollution * PopChar,
    Percentile = 100 * normalize(ranks(Score) - 1),
    PercentileRange = binned(Percentile, CES2_percentile_breaks))
}

