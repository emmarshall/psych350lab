#' Within-Groups (Repeated Measures) ANOVA
#'
#' Performs a repeated measures ANOVA for two conditions.
#' Uses listwise deletion (matches SPSS/JAMOVI) - subjects missing on
#' either condition are excluded from the analysis.
#'
#' @param data A data frame or tibble.
#' @param dv1 Character string. Name of the first condition's variable.
#' @param dv2 Character string. Name of the second condition's variable.
#' @param id_var Character string or `NULL`. Name of the subject ID variable.
#'   If `NULL`, row numbers are used as subject IDs.
#' @param digits Integer. Number of decimal places for rounding (default: 2).
#'
#' @return A list with elements:
#' \describe{
#'   \item{ANOVA}{A list with `F`, `p_value`, `df_effect`, `df_error`, `mse`, and `method`.}
#'   \item{Descriptives}{A tibble with `condition`, `mean`, `sd`, `n`, and `sem`.}
#'   \item{Sample_Size}{Number of unique participants with complete data.}
#' }
#'
#' @examples
#' data(superman)
#' result <- wg_anova_answers(superman,
#'   dv1 = "rt_critics_score",
#'   dv2 = "rt_audience_score"
#' )
#' result$ANOVA
#' result$Descriptives
#'
#' @export
wg_anova_answers <- function(data, dv1, dv2, id_var = NULL, digits = 2) {

  dv_vars <- c(dv1, dv2)

  # Create subject ID if not provided
  if (is.null(id_var)) {
    data$subject_id <- seq_len(nrow(data))
    id_var <- "subject_id"
  }

  # CRITICAL: Listwise deletion BEFORE pivoting
  # Remove any row where EITHER dv1 OR dv2 is missing
  # This matches SPSS/JAMOVI default behavior
  complete_data <- data |>
    dplyr::select(dplyr::all_of(c(id_var, dv_vars))) |>
    dplyr::filter(!is.na(.data[[dv1]]) & !is.na(.data[[dv2]]))

  # Now pivot to long format (no NAs should remain)
  long_data <- complete_data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(dv_vars),
      names_to = "condition",
      values_to = "score"
    ) |>
    dplyr::rename(subject = dplyr::all_of(id_var)) |>
    dplyr::mutate(
      subject = as.factor(subject),
      condition = as.factor(condition)
    ) |>
    dplyr::arrange(condition)  # Ensures consistent ordering

  # Run repeated measures ANOVA
  aov_model <- stats::aov(score ~ condition + Error(subject / condition),
                          data = long_data)
  aov_summary <- summary(aov_model)
  within_summary <- aov_summary$`Error: subject:condition`[[1]]

  # Extract ANOVA statistics safely
  f_stat <- if (length(within_summary$`F value`) > 0) within_summary$`F value`[1] else NA
  p_value <- if (length(within_summary$`Pr(>F)`) > 0) within_summary$`Pr(>F)`[1] else NA
  df_effect <- if (length(within_summary$Df) > 0) within_summary$Df[1] else NA
  df_error <- if (length(within_summary$Df) > 1) within_summary$Df[2] else NA
  mse <- if (length(within_summary$`Mean Sq`) > 1) within_summary$`Mean Sq`[2] else NA

  # Calculate descriptive statistics using modern .by syntax
  desc_stats <- long_data |>
    dplyr::summarise(
      mean = mean(score),
      sd = stats::sd(score),
      n = dplyr::n(),
      .by = condition
    ) |>
    dplyr::mutate(
      sem = sd / sqrt(n),
      dplyr::across(c(mean, sd, sem), \(x) round(x, digits))
    ) |>
    dplyr::arrange(condition)

  results_list <- list(
    ANOVA = list(
      F = if (!is.na(f_stat)) round(f_stat, digits) else NA,
      p_value = if (!is.na(p_value)) round(p_value, 3) else NA,
      df_effect = df_effect,
      df_error = df_error,
      mse = if (!is.na(mse)) round(mse, digits) else NA,
      method = "Repeated Measures ANOVA"
    ),
    Descriptives = desc_stats,
    Sample_Size = nrow(complete_data)  # Number of complete cases
  )

  invisible(results_list)
}
