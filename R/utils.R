#' To normalize a vector
#' 
#' @param x numeric
#' @param digits numeric
#' @param lower numeric
#' @param na.rm logical
#' 
#' @note upper is always 1.0
#' 
#' @export
CES_rescale <- function (x, digits = Inf, lower = min(x, na.rm = TRUE)) {
  round(x - lower, digits = digits) / round(max(x - lower, na.rm = TRUE), digits = digits)
}


#' To bin percentiles
#' 
#' @param x vector
#' @param breaks named numeric vector
#' 
#' @export
CES_bin_ranges <- function (x, breaks) {
  cut(x, c(breaks, Inf), names(breaks), include.lowest = TRUE, ordered = TRUE)
}