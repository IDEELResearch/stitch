#' Canonical prevalence bin definitions by mutation
#'
#' Returns ordered prevalence bin labels used for mapping and plotting
#' prevalence data. Bin definitions can differ by mutation (e.g. K13 vs.
#' partner-drug resistance markers).
#'
#' If `mutation` is `NULL`, the function returns a named list containing
#' bin definitions for all supported mutations. If a specific `mutation`
#' is provided, only the corresponding character vector of bin labels
#' is returned.
#'
#' @param mutation Optional character string specifying the mutation
#'   for which prevalence bins should be returned. Supported values include
#'   `"k13"`, `"mdr1C:86:N"`, and `"crt:76:T"`. If `NULL` (default), all
#'   bin definitions are returned as a named list.
#'
#' @details
#' Bin labels are returned in the desired plotting order and are intended
#' to be used as factor levels (e.g. via
#' `factor(prevalence, levels = PREV_LEVELS("k13"))`).
#'
#' The function is designed to centralize bin definitions to ensure
#' consistency across analyses, plots, and summaries.
#'
#' @return
#' If `mutation` is `NULL`, a named list of character vectors, one per
#' supported mutation.
#'
#' If `mutation` is provided, a character vector of bin labels in the
#' appropriate order.
#'
#' @examples
#' # Get bins for all mutations
#' PREV_LEVELS()
#'
#' # Get bins for K13 prevalence
#' PREV_LEVELS("k13")
#'
#' # Use bins as factor levels
#' factor(c("0", "1-5", "5-10"), levels = PREV_LEVELS("k13"))
#'
#' @export
PREV_LEVELS <- function(mutation = NULL) {

  levels_list <- list(
    "mdr1" = c(
      "0","0-10","10-20","20-30","30-40","40-50",
      "50-60","60-70","70-80","80-90","90-95","100"
    ),
    "crt" = c(
      "0","0-10","10-20","20-30","30-40","40-50",
      "50-60","60-70","70-80","80-90","90-95","100"
    ),
    "k13" = c(
      "0", "0-1", "1-5", "5-10", "10-20", "20-30", "30-40", "40+"
      )
  )

  if (is.null(mutation)) {
    return(levels_list)
  }

  if (!mutation %in% names(levels_list)) {
    stop("Unknown partner-drug mutation: ", mutation, call. = FALSE)
  }

  levels_list[[mutation]]
}
