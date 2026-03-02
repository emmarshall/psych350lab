# =============================================================================
# univariate_stats.R
# Descriptive Statistics — Summary Stats & Interactive Checker Table
# =============================================================================

#' Compute Summary Statistics for All Variables
#'
#' Calculates mean, standard deviation, sample size, and standard error of the
#' mean for every column in a data frame. Useful for preparing data for
#' \code{create_stats_table()}.
#'
#' @param data A data frame. All columns will be summarised.
#' @param digits Integer. Number of decimal places to round to. Default is 2.
#'
#' @return A tibble with columns `variable`, `mean`, `sd`, `n`, and `sem`.
#'
#' @examples
#' data(superman)
#' library(dplyr)
#'
#' my_data <- superman |>
#'   select(year, clark_height_in, height_diff) |>
#'   filter(!is.na(height_diff))
#'
#' compute_summary_stats(my_data)
#'
#' @export
compute_summary_stats <- function(data, digits = 2) {
  data |>
    dplyr::summarise(dplyr::across(dplyr::everything(), list(
      mean = \(x) mean(x, na.rm = TRUE),
      sd   = \(x) stats::sd(x, na.rm = TRUE),
      n    = \(x) sum(!is.na(x))
    ))) |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to  = c("variable", ".value"),
      names_pattern = "(.+)_(mean|sd|n)"
    ) |>
    dplyr::mutate(
      sem  = sd / sqrt(n),
      dplyr::across(c(mean, sd, sem), \(x) round(x, digits))
    )
}


#' Descriptive Statistics Analysis
#'
#' Computes descriptive statistics (mean, SD, n, SEM) for specified variables.
#' Returns a list structure consistent with other `*_answers()` functions
#' for use with [create_descriptives_checker()].
#'
#' @param data A data frame.
#' @param vars Character vector. Variable names to summarize. If `NULL`,
#'   all columns are used.
#' @param digits Integer. Number of decimal places to round to. Default is 2.
#'
#' @return A list with elements:
#' \describe{
#'   \item{Descriptives}{A tibble with columns `variable`, `mean`, `sd`, `n`, and `sem`.}
#'   \item{Sample_Size}{Total number of rows in the data.}
#' }
#'
#' @examples
#' data(superman)
#'
#' # Specific variables
#' result <- descriptives_answers(superman,
#'   vars = c("year", "clark_height_in", "height_diff"))
#' result$Descriptives
#' result$Sample_Size
#'
#' # All variables
#' result_all <- descriptives_answers(superman)
#'
#' @export
descriptives_answers <- function(data, vars = NULL, digits = 2) {

  # Select variables
  if (is.null(vars)) {
    analysis_data <- data
  } else {
    analysis_data <- data |>
      dplyr::select(dplyr::all_of(vars))
  }

  # Compute stats using existing function
  desc_stats <- compute_summary_stats(analysis_data, digits = digits)

  # Reorder to match vars if specified
  if (!is.null(vars)) {
    desc_stats <- desc_stats |>
      dplyr::slice(match(vars, variable))
  }

  results_list <- list(
    Descriptives = desc_stats,
    Sample_Size = nrow(data)
  )

  invisible(results_list)
}
