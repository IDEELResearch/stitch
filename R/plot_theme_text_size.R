#' Standardize text sizes for ggplot facets and legends
#'
#' Applies a consistent theme adjustment to a ggplot object, setting
#' text sizes for facet strip labels and legend titles/text. This is
#' intended as a lightweight post-processing step to ensure visual
#' consistency across figures.
#'
#' @param plot A `ggplot` object to which the text size theme will be applied.
#'
#' @details
#' The function modifies the following theme elements:
#' \itemize{
#'   \item `strip.text`: facet label text size (set to 10)
#'   \item `legend.title`: legend title text size (set to 9)
#'   \item `legend.text`: legend item text size (set to 8)
#' }
#'
#' This function returns a modified ggplot object and does not alter the
#' original plot in place.
#'
#' @return A `ggplot` object with updated text sizing.
#'
#' @examples
#' \dontrun{
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p <- plot_theme_text_size(p)
#' p
#' }
#'
#' @importFrom ggplot2 theme element_text
#'
#' @export
plot_theme_text_size <- function(plot){
  plot <- plot + theme(
    axis.title = element_text(size = 9),
    axis.text  = element_text(size = 8),
    strip.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8)
  )
  plot
}
