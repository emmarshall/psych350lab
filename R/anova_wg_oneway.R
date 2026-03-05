# =============================================================================
# anova_wg.R
# Within-Groups (Repeated Measures) ANOVA Functions
# =============================================================================
# All functions return unrounded values; formatting handled by display functions.
# Updated to match existing bg_anova_answers() patterns.
# =============================================================================


#' Within-Groups ANOVA (Paired/Repeated Measures) - Two Conditions
#'
#' Performs a within-groups ANOVA for two repeated measures conditions.
#' Equivalent to a paired-samples t-test but returns F statistic.
#' Uses listwise deletion (matches SPSS/JAMOVI).
#'
#' @param data A data frame or tibble.
#' @param dv1 Character string. Name of the first condition variable.
#' @param dv2 Character string. Name of the second condition variable.
#' @param dv1_label Character or NULL. Display label for dv1.
#' @param dv2_label Character or NULL. Display label for dv2.
#'
#' @return A list with elements:
#' \describe{
#'   \item{ANOVA}{A list with `F`, `p_value`, `df_effect`, `df_error`, `mse`,
#'     and `method`. Unrounded.}
#'   \item{Descriptives}{A tibble with `condition`, `condition_label`, `mean`,
#'     `sd`, `n`, and `sem`. Unrounded.}
#'   \item{Sample_Size}{Number of complete cases.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- wg_anova_answers(data, "pretest", "posttest",
#'                            dv1_label = "Pre-test", dv2_label = "Post-test")
#' result$ANOVA
#' result$Descriptives
#' }
#'
#' @export
wg_anova_answers <- function(data, dv1, dv2,
                             dv1_label = NULL, dv2_label = NULL) {

  # Extract variables and create analysis data frame with listwise deletion
  analysis_df <- data |>
    dplyr::transmute(
      dv1 = as.numeric(.data[[dv1]]),
      dv2 = as.numeric(.data[[dv2]])
    ) |>
    dplyr::filter(!is.na(dv1) & !is.na(dv2))

  n <- nrow(analysis_df)

  # Reshape to long format for ANOVA

  long_df <- analysis_df |>
    dplyr::mutate(id = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = c(dv1, dv2),
      names_to = "condition",
      values_to = "value"
    ) |>
    dplyr::mutate(
      id = factor(id),
      condition = factor(condition, levels = c("dv1", "dv2"))
    )

  # Run repeated measures ANOVA using aov with Error term
  anova_model <- stats::aov(value ~ condition + Error(id/condition), data = long_df)
  anova_summary <- summary(anova_model)

  # Extract from the condition stratum
  condition_summary <- anova_summary$`Error: id:condition`[[1]]

  f_stat <- condition_summary$`F value`[1]
  p_value <- condition_summary$`Pr(>F)`[1]
  df_effect <- condition_summary$Df[1]
  df_error <- condition_summary$Df[2]
  ss_effect <- condition_summary$`Sum Sq`[1]
  ss_error <- condition_summary$`Sum Sq`[2]
  mse <- condition_summary$`Mean Sq`[2]

  # Set labels
  if (is.null(dv1_label)) dv1_label <- dv1
  if (is.null(dv2_label)) dv2_label <- dv2

  # Calculate descriptive statistics - NO ROUNDING
  desc_stats <- tibble::tibble(
    condition = c("dv1", "dv2"),
    condition_label = c(dv1_label, dv2_label),
    mean = c(mean(analysis_df$dv1), mean(analysis_df$dv2)),
    sd = c(stats::sd(analysis_df$dv1), stats::sd(analysis_df$dv2)),
    n = c(n, n)
  ) |>
    dplyr::mutate(sem = sd / sqrt(n))

  results_list <- list(
    ANOVA = list(
      F = f_stat,
      p_value = p_value,
      df_effect = df_effect,
      df_error = df_error,
      mse = mse,
      ss_effect = ss_effect,
      ss_error = ss_error,
      method = "Within-Groups ANOVA (Repeated Measures)"
    ),
    Descriptives = desc_stats,
    Sample_Size = n
  )

  invisible(results_list)
}
