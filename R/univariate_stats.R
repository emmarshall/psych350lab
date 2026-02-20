# Descriptive Statistics — Summary Stats & Interactive Checker Table

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
#' @export
compute_summary_stats <- function(data, digits = 2) {

  data |>
    dplyr::summarise(dplyr::across(dplyr::everything(), list(
      mean = ~mean(., na.rm = TRUE),
      sd   = ~sd(., na.rm = TRUE),
      n    = ~sum(!is.na(.))
    ))) |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to  = c("variable", ".value"),
      names_pattern = "(.+)_(mean|sd|n)"
    ) |>
    dplyr::mutate(
      sem  = .data$sd / sqrt(.data$n),
      mean = round(.data$mean, digits),
      sd   = round(.data$sd, digits),
      sem  = round(.data$sem, digits)
    )
}

