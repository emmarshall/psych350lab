# =============================================================================
# univariate_stats.R
# Descriptive Statistics — Summary Stats & Interactive Checker Table
# =============================================================================

#' Compute Summary Statistics for All Variables
#'
#' Calculates mean, standard deviation, sample size, and standard error of the
#' mean for every column in a data frame. All numeric values stored unrounded.
#'
#' @param data A data frame. All columns will be summarised.
#'
#' @return A tibble with columns `variable`, `mean`, `sd`, `n`, and `sem`.
#'   All values are unrounded.
#'
#' @examples
#' data(superman, package = "psych350data")
#' library(dplyr)
#'
#' my_data <- superman |>
#'   select(year, clark_height_in, height_diff) |>
#'   filter(!is.na(height_diff))
#'
#' compute_summary_stats(my_data)
#'
#' @export
compute_summary_stats <- function(data) {
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
      sem = sd / sqrt(n)
      # NO rounding - let display functions handle it
    )
}


#' Descriptive Statistics Analysis
#'
#' Computes descriptive statistics for selected variables and returns a
#' structured results list consistent with the other `*_answers()` functions
#' in the package. This is a convenience wrapper around
#' [compute_summary_stats()] that selects the requested variables and packages
#' the output for use with [create_descriptives_checker()] and
#' [create_apa_descriptives_table()].
#'
#' @param data A data frame.
#' @param vars Character vector. Variable names to include in the analysis.
#'   If `NULL` (default), all columns are used.
#'
#' @return A list with elements:
#' \describe{
#'   \item{`Descriptives`}{A tibble with columns `variable`, `mean`, `sd`,
#'     `n`, and `sem`. All values are unrounded.}
#'   \item{`Sample_Size`}{Integer. Number of rows in the (subsetted) data.}
#' }
#'
#' @seealso [compute_summary_stats()], [create_descriptives_checker()],
#'   [create_apa_descriptives_table()]
#'
#' @examples
#' data(superman, package = "psych350data")
#' library(dplyr)
#'
#' my_data <- superman |>
#'   select(num, year, clark_height_in, clark_grp, height_diff) |>
#'   filter(!is.na(height_diff))
#'
#' result <- descriptives_answers(my_data,
#'   vars = c("year", "clark_height_in", "height_diff")
#' )
#' result$Descriptives
#' result$Sample_Size
#'
#' @export
descriptives_answers <- function(data, vars = NULL) {

  if (!is.null(vars)) {
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars) > 0) {
      stop(
        "Variable(s) not found in data: ",
        paste(missing_vars, collapse = ", ")
      )
    }
    analysis_df <- data[, vars, drop = FALSE]
  } else {
    analysis_df <- data
  }

  desc_stats <- compute_summary_stats(analysis_df)

  list(
    Descriptives = desc_stats,
    Sample_Size  = nrow(data)
  )
}

