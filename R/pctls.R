#' Calculate ranks (as in CalEnviroScreen 2.0)
#'
#' @param x values
#' @param i (optional) expression indicating which values to include in the calculation 
#' @param \dots further arguments to base::rank
#' @export
comp_rank <- function (x, i=which(x > 0), ...) {
  r <- rank(x[i], ties.method = "max", ...)
  replace(x, i, r)
}

#' Calculate percentiles (as in CalEnviroScreen 2.0)
#'
#' @param x values
#' @param \dots futher arguments to \link{comp_rank}
#' @export
pctl <- function (x, ...) {
  r <- comp_rank(x, ...)
  r / max(r, na.rm=TRUE) * 100
}