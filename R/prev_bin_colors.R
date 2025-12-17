#' Discrete colours for binned prevalence
#'
#' Returns a named vector (levels -> colour) for use with \code{scale_color_manual()}.
#'
#' @return A named character vector of hex colours keyed by \code{PREV_LEVELS()}.
#' @examples
#' scale_color_manual(values = prev_bin_colors())
#' @export
prev_bin_colors <- function(mut) {
  labs <- PREV_LEVELS(mut)
  nonzero_labs <- setdiff(labs, "0")

  pal <- prevalence_palette(length(nonzero_labs))

  cols <- c("0" = "grey90")
  cols[nonzero_labs] <- pal

  cols[labs]
}
