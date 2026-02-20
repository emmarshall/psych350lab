#' Within-Groups (Repeated Measures) ANOVA
#'
#' Performs a one-way repeated measures ANOVA comparing two conditions
#' measured on the same participants.
#'
#' @param data A data frame or tibble in wide format.
#' @param dv1 Character string. Name of the first condition's variable.
#' @param dv2 Character string. Name of the second condition's variable.
#' @param id_var Character string or `NULL`. Name of the subject ID variable.
#'   If `NULL`, row numbers are used as subject IDs.
#'
#' @return A list with elements:
#' \describe{
#'   \item{ANOVA}{A list with `F`, `p_value`, `df_effect`, `df_error`, `mse`, and `method`.}
#'   \item{Descriptives}{A tibble with `condition`, `mean`, `sd`, `n`, and `sem`.}
#'   \item{Sample_Size}{Number of unique participants.}
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
wg_anova_answers <- function(data, dv1, dv2, id_var = NULL) {

  dv_vars <- c(dv1, dv2)

  if (is.null(id_var)) {
    data$subject_id <- seq_len(nrow(data))
    id_var <- "subject_id"
  }

  long_data <- data |>
    dplyr::select(dplyr::all_of(c(id_var, dv_vars))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(dv_vars),
      names_to = "condition",
      values_to = "score"
    ) |>
    dplyr::filter(!is.na(.data$score))

  long_data <- long_data |>
    dplyr::rename(subject = dplyr::all_of(id_var))

  long_data$subject <- as.factor(long_data$subject)
  long_data$condition <- as.factor(long_data$condition)

  aov_model <- stats::aov(score ~ condition + Error(subject / condition),
                          data = long_data)
  aov_summary <- summary(aov_model)

  within_summary <- aov_summary$`Error: subject:condition`[[1]]

  f_stat <- if (length(within_summary$`F value`) > 0) within_summary$`F value`[1] else NA

  p_value <- if (length(within_summary$`Pr(>F)`) > 0) within_summary$`Pr(>F)`[1] else NA
  df_effect <- if (length(within_summary$Df) > 0) within_summary$Df[1] else NA
  df_error <- if (length(within_summary$Df) > 1) within_summary$Df[2] else NA
  mse <- if (length(within_summary$`Mean Sq`) > 1) within_summary$`Mean Sq`[2] else NA

  desc_stats <- long_data |>
    dplyr::group_by(.data$condition) |>
    dplyr::summarise(
      mean = mean(.data$score, na.rm = TRUE),
      sd = sd(.data$score, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      sem = .data$sd / sqrt(.data$n),
      dplyr::across(c("mean", "sd", "sem"), ~ round(., 2))
    )

  results_list <- list(
    ANOVA = list(
      F = if (!is.na(f_stat)) round(f_stat, 2) else NA,
      p_value = if (!is.na(p_value)) round(p_value, 3) else NA,
      df_effect = df_effect,
      df_error = df_error,
      mse = if (!is.na(mse)) round(mse, 2) else NA,
      method = "Repeated Measures ANOVA"
    ),
    Descriptives = desc_stats,
    Sample_Size = length(unique(long_data$subject))
  )

  invisible(results_list)
}
