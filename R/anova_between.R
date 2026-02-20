#' Between-Groups One-Way ANOVA
#'
#' Performs a one-way between-groups ANOVA with descriptive statistics
#' for each group level.
#'
#' @param data A data frame or tibble.
#' @param iv Character string. Name of the independent variable (grouping factor).
#' @param dv Character string. Name of the dependent (continuous) variable.
#'
#' @return A list with elements:
#' \describe{
#'   \item{ANOVA}{A list with `F`, `p_value`, `df_between`, `df_within`, `mse`, and `method`.}
#'   \item{Descriptives}{A tibble with `iv`, `mean`, `sd`, `n`, and `sem` per group.}
#'   \item{Sample_Size}{Total number of valid cases.}
#' }
#'
#' @examples
#' data(superman)
#' result <- bg_anova_answers(superman, iv = "clark_grp", dv = "rt_critics_score")
#' result$ANOVA
#' result$Descriptives
#'
#' @export
bg_anova_answers <- function(data, iv, dv) {

  dv_vector <- as.numeric(data[[dv]])
  iv_vector <- as.factor(data[[iv]])

  analysis_df <- data.frame(dv = dv_vector, iv = iv_vector)
  analysis_df <- analysis_df |>
    dplyr::filter(!is.na(.data$dv) & !is.na(.data$iv))

  anova_model <- stats::aov(dv ~ iv, data = analysis_df)
  anova_summary <- summary(anova_model)

  f_stat <- anova_summary[[1]]$`F value`[1]
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  df_between <- anova_summary[[1]]$Df[1]
  df_within <- anova_summary[[1]]$Df[2]
  mse <- anova_summary[[1]]$`Mean Sq`[2]

  desc_stats <- analysis_df |>
    dplyr::group_by(.data$iv) |>
    dplyr::summarise(
      mean = mean(.data$dv, na.rm = TRUE),
      sd = sd(.data$dv, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      sem = .data$sd / sqrt(.data$n),
      dplyr::across(c("mean", "sd", "sem"), ~ round(., 2))
    )

  results_list <- list(
    ANOVA = list(
      F = round(f_stat, 2),
      p_value = round(p_value, 3),
      df_between = df_between,
      df_within = df_within,
      mse = round(mse, 2),
      method = "One-way ANOVA"
    ),
    Descriptives = desc_stats,
    Sample_Size = nrow(analysis_df)
  )

  invisible(results_list)
}
