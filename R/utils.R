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
