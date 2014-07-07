#' Calculate ranks, as in CalEnviroScreen 2.0
#'
#' @param x values
#' @param i (optional) expression indicating which values to include in the calculation 
#' @param \dots further arguments to base::rank
ranks <- function (x, i=which(x > 0), ...) {
  r <- rank(x[i], ties.method = "max", ...)
  replace(x, i, r)
}

#' Calculate percentiles, as in CalEnviroScreen 2.0
#'
#' @param x values
#' @param \dots futher arguments to \link{ranks}
#' @export
percentiles <- function (x, ...) {
  r <- ranks(x, ...)
  r / max(r, na.rm=TRUE) * 100
}