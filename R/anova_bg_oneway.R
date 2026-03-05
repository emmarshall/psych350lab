#' Between-Groups One-Way ANOVA
#'
#' Performs a one-way between-groups ANOVA with descriptive statistics
#' for each group level. Uses listwise deletion (matches SPSS/JAMOVI).
#'
#' @param data A data frame or tibble.
#' @param iv Character string. Name of the independent variable (grouping factor).
#' @param dv Character string. Name of the dependent (continuous) variable.
#'
#' @return A list with elements:
#' \describe{
#'   \item{ANOVA}{A list with `F`, `p_value`, `df_between`, `df_within`, `mse`, and `method`. Unrounded.}
#'   \item{Descriptives}{A tibble with `iv`, `mean`, `sd`, `n`, and `sem` per group. Unrounded.}
#'   \item{Sample_Size}{Total number of valid cases.}
#' }
#'
#' @examples
#' data(superman, package = "psych350data")
#' result <- bg_anova_answers(superman, iv = "clark_grp", dv = "rt_critics_score")
#' result$ANOVA
#' result$Descriptives
#'
#' @export
bg_anova_answers <- function(data, iv, dv) {

  # CRITICAL: Listwise deletion - remove rows where EITHER iv OR dv is missing
  analysis_df <- data |>
    dplyr::transmute(
      dv = as.numeric(.data[[dv]]),
      iv = as.factor(.data[[iv]])
    ) |>
    dplyr::filter(!is.na(dv) & !is.na(iv))

  # Run ANOVA
  anova_model <- stats::aov(dv ~ iv, data = analysis_df)
  anova_summary <- summary(anova_model)[[1]]

  # Extract ANOVA statistics - NO ROUNDING
  f_stat <- anova_summary$`F value`[1]
  p_value <- anova_summary$`Pr(>F)`[1]
  df_between <- anova_summary$Df[1]
  df_within <- anova_summary$Df[2]
  mse <- anova_summary$`Mean Sq`[2]

  # Calculate descriptive statistics - NO ROUNDING
  desc_stats <- analysis_df |>
    dplyr::summarise(
      mean = mean(dv),
      sd = stats::sd(dv),
      n = dplyr::n(),
      .by = iv
    ) |>
    dplyr::mutate(
      sem = sd / sqrt(n)
      # NO rounding - let display functions handle it
    ) |>
    dplyr::arrange(iv)

  results_list <- list(
    ANOVA = list(
      F = f_stat,
      p_value = p_value,
      df_between = df_between,
      df_within = df_within,
      mse = mse,
      method = "One-way ANOVA"
    ),
    Descriptives = desc_stats,
    Sample_Size = nrow(analysis_df)
  )

  invisible(results_list)
}

#' Pairwise Effect Size from Means
#'
#' Computes r effect size from two group means using omnibus MSE.
#'
#' @param mean1 Mean of group 1.
#' @param mean2 Mean of group 2.
#' @param mse Mean square error from omnibus ANOVA.
#' @param n1 Sample size of group 1.
#' @param n2 Sample size of group 2.
#'
#' @return Numeric r effect size.
#'
#' @examples
#' pr_means_to_r(10.5, 8.2, 4.5, 25, 30)
#'
#' @export
pr_means_to_r <- function(mean1, mean2, mse, n1, n2) {
  se_diff <- sqrt(mse * (1 / n1 + 1 / n2))
  t_stat <- abs(mean1 - mean2) / se_diff
  df <- n1 + n2 - 2
  r <- sqrt(t_stat^2 / (t_stat^2 + df))
  r
}


#' K-Group Between-Groups ANOVA with Pairwise Comparisons
#'
#' Performs a one-way ANOVA with LSD/HSD pairwise comparisons,
#' effect sizes, and power assessments. All numeric values stored unrounded.
#'
#' @param data A data frame.
#' @param dv Character. Name of the dependent variable.
#' @param iv Character. Name of the independent variable (grouping factor).
#' @param group_labels Character vector or NULL. Display labels for groups.
#'
#' @return A list with elements (all numeric values unrounded).
#'
#' @export
anova_kgroup_answers <- function(data, dv, iv, group_labels = NULL) {

  dv_vector <- as.numeric(data[[dv]])
  iv_vector <- as.factor(data[[iv]])

  analysis_df <- data.frame(
    dv = dv_vector,
    iv = iv_vector
  )

  analysis_df <- analysis_df[!is.na(analysis_df$dv) & !is.na(analysis_df$iv), ]

  anova_model <- stats::aov(dv ~ iv, data = analysis_df)
  anova_summary <- summary(anova_model)

  # NO ROUNDING on extraction
  f_stat <- anova_summary[[1]]$`F value`[1]
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  df_between <- anova_summary[[1]]$Df[1]
  df_within <- anova_summary[[1]]$Df[2]
  mse <- anova_summary[[1]]$`Mean Sq`[2]

  # Descriptives - NO ROUNDING
  desc_list <- lapply(split(analysis_df$dv, analysis_df$iv), function(x) {
    data.frame(
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      n = length(x)
    )
  })

  desc_stats <- do.call(rbind, desc_list)
  desc_stats$iv <- rownames(desc_stats)
  rownames(desc_stats) <- NULL
  desc_stats <- tibble::as_tibble(desc_stats) |>
    dplyr::arrange(iv)

  if (!is.null(group_labels)) {
    desc_stats$group_label <- group_labels
  } else {
    desc_stats$group_label <- as.character(desc_stats$iv)
  }

  total_n <- nrow(analysis_df)
  k <- length(unique(analysis_df$iv))
  mean_n <- mean(desc_stats$n)

  lsd_hsd_results <- lsd_hsd_calculator(
    k = k,
    n_per_group = mean_n,
    mse = mse,
    df_error_input = df_within
  )

  lsd_mmd <- lsd_hsd_results$lsd
  hsd_mmd <- lsd_hsd_results$hsd

  group_levels <- levels(analysis_df$iv)
  n_groups <- length(group_levels)
  pairwise_results <- list()

  comparison_counter <- 1
  for (i in 1:(n_groups - 1)) {
    for (j in (i + 1):n_groups) {
      group1 <- group_levels[i]
      group2 <- group_levels[j]

      if (!is.null(group_labels)) {
        label1 <- group_labels[i]
        label2 <- group_labels[j]
      } else {
        label1 <- group1
        label2 <- group2
      }

      mean1 <- desc_stats$mean[desc_stats$iv == group1]
      mean2 <- desc_stats$mean[desc_stats$iv == group2]
      n1 <- desc_stats$n[desc_stats$iv == group1]
      n2 <- desc_stats$n[desc_stats$iv == group2]
      sd1 <- desc_stats$sd[desc_stats$iv == group1]
      sd2 <- desc_stats$sd[desc_stats$iv == group2]

      mean_diff <- mean1 - mean2
      is_sig <- abs(mean_diff) > lsd_mmd

      if (is_sig) {
        lsd_result <- if (mean_diff > 0) ">" else "<"
      } else {
        lsd_result <- "="
      }

      effect_size <- pr_means_to_r(mean1, mean2, mse, n1, n2)
      error_type <- if (is_sig) "Type I & III" else "Type II"

      if (is_sig) {
        power_problem <- "No \u2013 rejecting H0: means there was sufficient power"
      } else {
        if (effect_size < 0.10) {
          power_problem <- "No \u2013 effect is \"too small to be interesting,\" (r < .10)"
        } else {
          power_problem <- "Yes \u2013 The effect is \"large enough to be interesting,\" (r > .10)"
        }
      }

      # NO ROUNDING - store raw values
      pairwise_results[[comparison_counter]] <- list(
        comparison = paste(label1, "vs", label2),
        mean_diff = mean_diff,
        lsd_result = lsd_result,
        error_type = error_type,
        effect_size = effect_size,
        power_problem = power_problem
      )

      comparison_counter <- comparison_counter + 1
    }
  }

  # NO ROUNDING in final output
  results_list <- list(
    ANOVA = list(
      F = f_stat,
      p_value = p_value,
      df_between = df_between,
      df_within = df_within,
      mse = mse,
      total_n = total_n,
      k = k,
      mean_n = mean_n
    ),
    Descriptives = desc_stats,
    LSD = list(
      lsd_mmd = lsd_mmd,
      hsd_mmd = hsd_mmd,
      need_posthoc = p_value < 0.05,
      parameters_used = lsd_hsd_results$parameters_used
    ),
    Pairwise = pairwise_results,
    group_labels = desc_stats$group_label
  )

  invisible(results_list)
}
