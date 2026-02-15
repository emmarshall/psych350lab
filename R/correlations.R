#' Pearson Correlation Analysis
#'
#' Computes a Pearson correlation between two variables, including descriptive
#' statistics computed separately for each variable (matching SPSS/Jamovi behavior)
#' and inferential statistics for complete pairs.
#'
#' @param data A data frame or tibble.
#' @param var1 Character string. Name of the first variable.
#' @param var2 Character string. Name of the second variable.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{Correlation}{A list containing `r`, `p_value`, `p_value_raw`, `df`,
#'     `CI_low`, `CI_high`, and `method`.}
#'   \item{Descriptives}{A tibble with `variable`, `mean`, `sd`, `n`, and `sem`
#'     for each variable (using all available cases per variable).}
#'   \item{Sample_Size}{The number of complete pairs used in the correlation.}
#' }
#'
#' @examples
#' data(superman_data)
#' result <- corr_answers(superman_data, "clark_height_in", "rt_critics_score")
#' result$Correlation$r
#' result$Descriptives
#'
#' @export
corr_answers <- function(data, var1, var2) {

  vector1 <- as.numeric(data[[var1]])
  vector2 <- as.numeric(data[[var2]])

  # Descriptives per variable (all available cases)
  desc_stats_separate <- tibble::tibble(
    variable = c(var1, var2),
    mean = c(mean(vector1, na.rm = TRUE), mean(vector2, na.rm = TRUE)),
    sd = c(sd(vector1, na.rm = TRUE), sd(vector2, na.rm = TRUE)),
    n = c(sum(!is.na(vector1)), sum(!is.na(vector2)))
  ) |>
    dplyr::mutate(
      sem = .data$sd / sqrt(.data$n),
      dplyr::across(c("mean", "sd", "sem"), ~ round(., 2))
    )

  # Filter to complete pairs
  valid_cases <- !is.na(vector1) & !is.na(vector2)
  vector1 <- vector1[valid_cases]
  vector2 <- vector2[valid_cases]

  if (length(vector1) < 3 || length(vector2) < 3) {
    stop(paste0("Not enough valid cases remaining after removing missing values (need at least 3). ",
                "Found ", length(vector1), " valid cases."))
  }

  cor_test <- stats::cor.test(vector1, vector2, method = "pearson")

  p_value_formatted <- if (cor_test$p.value < 0.001) {
    0.001
  } else {
    round(cor_test$p.value, 3)
  }

  results_list <- list(
    Correlation = list(
      r = round(cor_test$estimate, 2),
      p_value = p_value_formatted,
      p_value_raw = cor_test$p.value,
      df = cor_test$parameter,
      CI_low = round(cor_test$conf.int[1], 2),
      CI_high = round(cor_test$conf.int[2], 2),
      method = cor_test$method
    ),
    Descriptives = desc_stats_separate,
    Sample_Size = length(vector1)
  )

  invisible(results_list)
}
