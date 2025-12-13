#' Continuous prevalence colour palette
#'
#' Creates a function that maps numeric prevalence values to colours
#' along a slategray→green→khaki→orange→red gradient.
#'
#' @param n Integer. Number of colours to generate.
#' @return A character vector of hex colours of length \code{n}.
#' @examples
#' prevalence_palette(5)
#' @export
prevalence_palette <- function(n) {
  stopifnot(length(n) == 1L, is.numeric(n), n > 0)
  grDevices::colorRampPalette(c("slategray3", "palegreen2", "khaki2", "orange", "red"))(n)
}
