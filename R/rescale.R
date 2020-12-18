
#' rescale
#'
#' @description rescale the series
#' @param x series
#'
#'
#' @examples rescale(seq(1:100,2))
#' @export
#'
rescale <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
