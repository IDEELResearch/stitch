#' Bin numeric prevalence into labelled intervals
#'
#' @param v Numeric vector of prevalence values (e.g., percentages).
#' @return An ordered factor with levels from \code{PREV_LEVELS()}.
#' @examples
#' bin_prevalence(c(0, 0.3, 2, 7, 15, 25, 35, 60))
#' @export
bin_prevalence <- function(v, mut) {
  labs <- PREV_LEVELS(mut)

  if (mut == "k13"){
    bin_breaks = c(-Inf, 0, 1, 5, 10, 20, 30, 40, Inf)
  } else{
    labels_to_parse <- labs[-length(labs)]
    upper_bound_strings <- sub(".*-", "", labels_to_parse)
    upper_bounds <- as.numeric(upper_bound_strings)
    bin_breaks <- c(-Inf, upper_bounds, Inf)
  }

  cut(
    v,
    breaks = bin_breaks,
    labels = labs,
    right = TRUE,
    ordered_result = TRUE
  )
}
