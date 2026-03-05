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

