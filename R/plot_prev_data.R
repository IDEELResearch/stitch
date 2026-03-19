#' Add year groups to a data frame
#'
#' Adds a \code{year_group} column based on fixed ranges.
#'
#' @param df A data frame.
#' @param year_col Unquoted column that contains the year (numeric/integer).
#' @return \code{df} with an added character column \code{year_group}.
#' @examples
#' df <- data.frame(year = 2012:2023)
#' add_year_group(df, year)
#' @export
add_year_group <- function(df, year_col = year) {
  dplyr::mutate(
    df,
    year_group = dplyr::case_when(
      {{ year_col }} %in% 2012:2013 ~ "2012-2013",
      {{ year_col }} %in% 2014:2015 ~ "2014-2015",
      {{ year_col }} %in% 2016:2017 ~ "2016-2017",
      {{ year_col }} %in% 2018:2019 ~ "2018-2019",
      {{ year_col }} %in% 2020:2021 ~ "2020-2021",
      {{ year_col }} %in% 2022:2023 ~ "2022-2023",
      TRUE ~ NA_character_
    )
  )
}

#' Canonical prevalence bins (labels)
#'
#' @return A character vector of bin labels in desired order.
#' @examples
#' PREV_LEVELS()
#' @export
PREV_LEVELS <- function() {
  c("0", "0-1", "1-5", "5-10", "10-20", "20-30", "30-40", "40+")
}

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
