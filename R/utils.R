`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#' Save Figures
#'
#' @param name Name of figure
#' @param fig ggplot or similar figure object
#' @param width Width of plot in inches. Default = 6
#' @param height Height of plot in inches. Default = 6
#' @param plot_dir Plotting directory. Defaults to "analysis/plots"
#' @param pdf_plot Logical for plotting pdf too. Default = TRUE
#' @param svg_plot Logical for plotting svg too. Default = TRUE
#' @param font_family If specified, sets all font family. Default = NULL
#' @param res Image resolution in dpi. Default = 300
#' @param ... Other parameters to pass to ragg::agg_png
#' @importFrom grDevices dev.off pdf
save_figs <- function(name,
                      fig,
                      width = 6,
                      height = 6,
                      plot_dir = file.path(here::here(), "analysis/plots"),
                      pdf_plot = TRUE,
                      svg_plot = TRUE,
                      font_family = "Helvetica",
                      res = 300,
                      ...) {

  if(!is.null(font_family)) {
    fig <- fig + ggplot2::theme(text = ggplot2::element_text(family = font_family))
  }

  dir.create(plot_dir, showWarnings = FALSE)
  fig_path <- function(name) {paste0(plot_dir, "/", name)}

  # PNG
  ragg::agg_png(fig_path(paste0(name,".png")),
                width = width,
                height = height,
                units = "in",
                res = res,
                ...)
  print(fig)
  dev.off()

  # PDF
  if (pdf_plot) {
    grDevices::pdf(file = fig_path(paste0(name,".pdf")),
                   width = width, height = height)
    print(fig)
    dev.off()
  }

  # SVG
  if (svg_plot) {
    svglite::svglite(file = fig_path(paste0(name,".svg")),
                     width = width, height = height)
    print(fig)
    dev.off()
  }
}

#' Save CSV (mirrors save_figs style)
#'
#' @param name Base name of the file (without extension)
#' @param df   Data frame to write
#' @param data_dir Output directory (default: "analysis/data_derived/prev_summary_tables")
#' @param na   String to use for missing values (default: "")
#' @param append_date If TRUE, append YYYY-MM-DD to the file name (default: FALSE)
#' @param gzip If TRUE, write a gzipped CSV (.csv.gz) (default: FALSE)
#' @param ...  Other args passed to readr::write_csv()
#' @return (invisible) path to the written file
save_csv <- function(name,
                      df,
                      data_dir = file.path(here::here(), "analysis/data_derived/prev_summary_tables"),
                      na = "",
                      append_date = FALSE,
                      gzip = FALSE,
                      ...) {

  stopifnot(!missing(name), !missing(df))

  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  base <- if (isTRUE(append_date)) paste0(name, "_", format(Sys.Date(), "%Y-%m-%d")) else name
  path <- file.path(data_dir, paste0(base, ".csv", if (isTRUE(gzip)) ".gz" else ""))

  readr::write_csv(df, path, na = na, ...)
  message("Wrote: ", path)
  invisible(path)
}

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
      {{ year_col }} %in% 2012:2014 ~ "2012-2014",
      {{ year_col }} %in% 2015:2017 ~ "2015-2017",
      {{ year_col }} %in% 2018:2020 ~ "2018-2020",
      {{ year_col }} %in% 2021:2023 ~ "2021-2023",
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
