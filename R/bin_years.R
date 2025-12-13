#' Bin rows into fixed-width year groups based on non-zero prevalence span
#'
#' Computes 3-year (by default) bins from the first to last year with
#' \emph{non-zero} prevalence, starting one year before the first non-zero year.
#' Adds a \code{year_group} factor column.
#'
#' @param df Data frame with \code{year} and \code{prevalence} columns.
#' @param bin_size Integer. Width of bins in years (default 3).
#' @param year_col Unquoted column name containing year (numeric/integer).
#' @param prev_col Unquoted column name containing prevalence (numeric).
#' @return Input data frame with an added \code{year_group} factor (rows outside
#'   the computed bins are dropped).
#' @examples
#' d <- data.frame(year = 2012:2023, prevalence = c(0,0,1,2,0,3,0,0,5,0,0,0))
#' bin_years(d, bin_size = 3)
#' @export
bin_years <- function(df, bin_size = 3, year_col = year, prev_col = prevalence) {
  df <- dplyr::mutate(df, year = as.numeric(as.character({{ year_col }})))
  prev_name <- rlang::as_name(rlang::enquo(prev_col))
  nz <- df$year[df[[prev_name]] > 0]
  if (length(nz) == 0) return(dplyr::mutate(df, year_group = factor(NA_character_)))
  first_year   <- min(nz, na.rm = TRUE) - 1
  current_year <- max(nz, na.rm = TRUE)

  breaks <- seq(first_year, current_year, by = bin_size)
  breaks <- if (current_year > max(breaks)) c(breaks, current_year + 1) else c(breaks, max(breaks) + bin_size)

  labels <- vapply(seq_along(breaks[-1]), function(i) {
    start <- breaks[i]; end <- breaks[i + 1] - 1
    if (start == end) as.character(start) else paste0(start, "-", end)
  }, character(1))

  dplyr::mutate(
    df,
    year_group = cut(year, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
  ) |>
    dplyr::filter(!is.na(year_group))
}
