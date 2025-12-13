#' Bin numeric prevalence into labelled intervals
#'
#' @param v Numeric vector of prevalence values (e.g., percentages).
#' @return An ordered factor with levels from \code{PREV_LEVELS()}.
#' @examples
#' bin_prevalence(c(0, 0.3, 2, 7, 15, 25, 35, 60))
#' @export
bin_prevalence <- function(v) {
  labs <- PREV_LEVELS()
  cut(
    v,
    breaks = c(-Inf, 0, 1, 5, 10, 20, 30, 40, Inf),
    labels = labs,
    right = TRUE,
    ordered_result = TRUE
  )
}
