#' Discrete colours for binned prevalence
#'
#' Returns a named vector (levels -> colour) for use with \code{scale_color_manual()}.
#'
#' @return A named character vector of hex colours keyed by \code{PREV_LEVELS()}.
#' @examples
#' scale_color_manual(values = prev_bin_colors())
#' @export
prev_bin_colors <- function() {
  pal <- prevalence_palette(8)
  labs <- PREV_LEVELS()
  cols <- c(
    "0"     = "grey90",
    "0-1"   = pal[1],
    "1-5"   = pal[2],
    "5-10"  = pal[3],
    "10-20" = pal[4],
    "20-30" = pal[5],
    "30-40" = pal[6],
    "40+"   = pal[8]
  )
  # ensure order/names match canonical levels (helps legends be stable)
  cols[labs]
}
