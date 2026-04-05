
#' Pairwise Effect Size from Means
#'
#' Computes r effect size from two group means using omnibus MSE.
#'
#' @param mean1 Numeric. Mean of group 1.
#' @param mean2 Numeric. Mean of group 2.
#' @param mse Numeric. Mean square error from omnibus ANOVA.
#' @param n1 Integer. Sample size of group 1.
#' @param n2 Integer. Sample size of group 2.
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
    dplyr::mutate(sem = sd / sqrt(n)) |>
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



#' LSD and HSD Critical Value Calculator
#'
#' Calculates Fisher's Least Significant Difference (LSD) and Tukey's
#' Honestly Significant Difference (HSD) minimum mean differences
#' using a stored studentized range table.
#'
#' @param k Integer. Number of groups (must be between 3 and 6).
#' @param n_per_group Numeric. Average sample size per group.
#' @param mse Numeric. Mean square error from the ANOVA.
#' @param df_error_input Numeric. Error degrees of freedom from the ANOVA.
#'
#' @return A list with elements:
#' \describe{
#'   \item{lsd}{The LSD minimum mean difference for significance.}
#'   \item{hsd}{The HSD minimum mean difference for significance.}
#'   \item{parameters_used}{A list containing the closest df used,
#'     q-value, and t-critical value from the lookup table.}
#' }
#'
#' @examples
#' # Calculate LSD and HSD for a 4-group ANOVA
#' result <- lsd_hsd_calculator(
#'   k = 4,
#'   n_per_group = 25,
#'   mse = 10.5,
#'   df_error_input = 96
#' )
#' result$lsd
#' result$hsd
#'
#' @export
lsd_hsd_calculator <- function(k, n_per_group, mse, df_error_input) {

  studentized_range_table <- data.frame(
    df = c(5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 90, 120, 200, 999),
    tcrit_p.05 = c(2.57, 2.23, 2.13, 2.09, 2.06, 2.04, 2.03, 2.02, 2.01, 2.00, 2.01, 1.98, 1.97, 1.96),
    k3 = c(4.60, 3.88, 3.67, 3.58, 3.52, 3.49, 3.47, 3.44, 3.42, 3.40, 3.37, 3.36, 3.34, 3.31),
    k4 = c(5.22, 4.33, 4.08, 3.96, 3.89, 3.85, 3.83, 3.79, 3.76, 3.74, 3.71, 3.68, 3.67, 3.63),
    k5 = c(5.67, 4.65, 4.37, 4.23, 4.15, 4.10, 4.08, 4.04, 4.01, 3.98, 3.95, 3.92, 3.90, 3.86),
    k6 = c(6.03, 4.91, 4.59, 4.45, 4.36, 4.30, 4.28, 4.23, 4.20, 4.16, 4.13, 4.10, 4.08, 4.03)
  )

  closest_df_index <- which.min(abs(studentized_range_table$df - df_error_input))
  q_value <- NA

  if (k == 3) {
    q_value <- studentized_range_table$k3[closest_df_index]
  } else if (k == 4) {
    q_value <- studentized_range_table$k4[closest_df_index]
  } else if (k == 5) {
    q_value <- studentized_range_table$k5[closest_df_index]
  } else if (k == 6) {
    q_value <- studentized_range_table$k6[closest_df_index]
  } else {
    stop("k must be between 3 and 6 for this implementation")
  }

  t_crit <- studentized_range_table$tcrit_p.05[closest_df_index]

  lsd <- t_crit * sqrt(2 * mse / n_per_group)

  hsd <- (q_value / sqrt(2)) * sqrt(mse / n_per_group)

  return(list(
    lsd = lsd,
    hsd = hsd,
    parameters_used = list(
      closest_df = studentized_range_table$df[closest_df_index],
      q_value = q_value,
      t_crit = t_crit
    )
  ))
}

#' Research Hypothesis Support Text (ANOVA)
#'
#' Creates formatted text evaluating whether research hypotheses are
#' supported based on pairwise comparison results.
#'
#' @param anova_results_list Output from [anova_kgroup_answers()].
#' @param hypotheses_list A list of hypothesis specifications. Each element
#'   should have `group1`, `group2`, `direction` (">", "<", or "="), and `text`.
#' @param group_labels Character vector or NULL. Display labels for groups.
#' @param KEY Logical. If TRUE (default), show answers; if FALSE, show blanks.
#' @param highlight Logical. If TRUE and KEY is TRUE, wrap answers in
#'   highlight formatting.
#'
#' @return A character string with formatted RH evaluation.
#'
#' @examples
#' \dontrun{
#' hypotheses <- list(
#'   list(group1 = "High", group2 = "Low", direction = ">",
#'        text = "High group will score higher than Low group")
#' )
#' create_rh_support_text(result, hypotheses, KEY = TRUE)
#' }
#'
#' @export
create_rh_support_text <- function(anova_results_list,
                                   hypotheses_list,
                                   group_labels = NULL,
                                   KEY = TRUE,
                                   highlight = FALSE) {

  if (is.null(group_labels)) {
    group_labels <- anova_results_list$Descriptives$group_label
  }

  pairwise <- anova_results_list$Pairwise

  hl <- function(text) {
    if (highlight && KEY) {
      paste0("[", text, "]{custom-style=\"highlight-yellow\"}")
    } else {
      as.character(text)
    }
  }

  check_hypothesis <- function(hyp) {
    group1 <- hyp$group1
    group2 <- hyp$group2
    expected_direction <- hyp$direction

    comp_name1 <- paste(group1, "vs", group2)
    comp_name2 <- paste(group2, "vs", group1)

    comp_result <- NULL
    is_reversed <- FALSE
    for (comp in pairwise) {
      if (comp$comparison == comp_name1) {
        comp_result <- comp
        is_reversed <- FALSE
        break
      } else if (comp$comparison == comp_name2) {
        comp_result <- comp
        is_reversed <- TRUE
        break
      }
    }

    if (is.null(comp_result)) {
      return(list(supported = "Unknown", text = "Comparison not found"))
    }

    actual_result <- comp_result$lsd_result

    supported <- FALSE
    result_text <- ""

    if (!is_reversed) {
      if (expected_direction == ">" && actual_result == ">") {
        supported <- TRUE
        result_text <- paste0(hl("Fully supported"), " -- ", group1, " significantly greater than ", group2, " by LSD")
      } else if (expected_direction == "<" && actual_result == "<") {
        supported <- TRUE
        result_text <- paste0(hl("Fully supported"), " -- ", group1, " significantly less than ", group2, " by LSD")
      } else if (expected_direction == "=" && actual_result == "=") {
        supported <- TRUE
        result_text <- paste0(hl("Fully supported"), " -- ", group1, " not significantly different than ", group2, " by LSD")
      } else if (expected_direction == ">" && actual_result == "<") {
        supported <- FALSE
        result_text <- paste0(hl("Not supported"), " -- ", group1, " significantly less than ", group2, " by LSD")
      } else if (expected_direction == "<" && actual_result == ">") {
        supported <- FALSE
        result_text <- paste0(hl("Not supported"), " -- ", group1, " significantly greater than ", group2, " by LSD")
      } else if (expected_direction != "=" && actual_result == "=") {
        supported <- FALSE
        result_text <- paste0(hl("Not supported"), " -- ", group1, " not significantly different than ", group2, " by LSD")
      } else {
        supported <- FALSE
        result_text <- paste0(hl("Not supported"), " -- ", group1, " not significantly different than ", group2, " by LSD")
      }
    } else {
      if (expected_direction == "<" && actual_result == ">") {
        supported <- TRUE
        result_text <- paste0(hl("Fully supported"), " -- ", group1, " significantly less than ", group2, " by LSD")
      } else if (expected_direction == ">" && actual_result == "<") {
        supported <- TRUE
        result_text <- paste0(hl("Fully supported"), " -- ", group1, " significantly greater than ", group2, " by LSD")
      } else if (expected_direction == "=" && actual_result == "=") {
        supported <- TRUE
        result_text <- paste0(hl("Fully supported"), " -- ", group1, " not significantly different than ", group2, " by LSD")
      } else if (expected_direction == ">" && actual_result == ">") {
        supported <- FALSE
        result_text <- paste0(hl("Not supported"), " -- ", group1, " significantly less than ", group2, " by LSD")
      } else if (expected_direction == "<" && actual_result == "<") {
        supported <- FALSE
        result_text <- paste0(hl("Not supported"), " -- ", group1, " significantly greater than ", group2, " by LSD")
      } else if (expected_direction != "=" && actual_result == "=") {
        supported <- FALSE
        result_text <- paste0(hl("Not supported"), " -- ", group1, " not significantly different than ", group2, " by LSD")
      } else {
        supported <- FALSE
        result_text <- paste0(hl("Not supported"), " -- ", group1, " not significantly different than ", group2, " by LSD")
      }
    }

    return(list(supported = supported, text = result_text))
  }

  output <- "## RH: Testing\n\n"
  n_supported <- 0
  n_total <- length(hypotheses_list)

  for (i in 1:length(hypotheses_list)) {
    hyp <- hypotheses_list[[i]]
    rh_num <- paste0("RH", i)

    output <- paste0(output, rh_num, ": ", hyp$text, "\n\n")
    output <- paste0(output, "     Is this RH: fully, partially or not supported? Explain your answer.\n   ")

    if (KEY) {
      result <- check_hypothesis(hyp)
      output <- paste0(output, result$text, "\n\n")

      if (result$supported == TRUE) {
        n_supported <- n_supported + 1
      }
    } else {
      output <- paste0(output, "____________\n\n")
    }
  }

  output <- paste0(output, "Overall, is this set of RH: fully, partially or not supported? ")

  if (KEY) {
    if (n_supported == n_total) {
      overall <- paste0(hl("Fully supported"), " -- all ", n_total, " pairwise comparisons met RH.")
    } else if (n_supported > 0) {
      overall <- paste0(hl("Partially supported"), " -- ", n_supported, " of ", n_total, " pairwise comparisons met RH.")
    } else {
      overall <- paste0(hl("Not supported"), " -- 0 of ", n_total, " pairwise comparisons met RH.")
    }
    output <- paste0(output, overall, "\n")
  } else {
    output <- paste0(output, "____________\n")
  }

  return(output)
}


#' ANOVA Statistics Text Output (Answer KEY)
#'
#' Creates formatted text output for ANOVA statistics, either filled
#' (answer KEY) or blank (student worksheet).
#'
#' @param anova_results_list Output from [anova_kgroup_answers()].
#' @param KEY Logical. If TRUE (default), show answers; if FALSE, show blanks.
#' @param highlight Logical. If TRUE and KEY is TRUE, wrap answers in
#'   highlight formatting for Quarto/Word output.
#'
#' @return A character string with formatted ANOVA results.
#'
#' @examples
#' \dontrun{
#' result <- anova_kgroup_answers(data, "dv", "iv")
#' cat(anova_statistics_KEY(result, KEY = TRUE))
#' }
#'
#' @export
anova_statistics_KEY <- function(anova_results_list,
                                 KEY = TRUE,
                                 highlight = FALSE) {
  f_stat <- anova_results_list$ANOVA$F
  p_value <- anova_results_list$ANOVA$p_value
  df_between <- anova_results_list$ANOVA$df_between
  df_within <- anova_results_list$ANOVA$df_within
  mse <- anova_results_list$ANOVA$mse
  total_n <- anova_results_list$ANOVA$total_n
  k <- anova_results_list$ANOVA$k
  mean_n <- anova_results_list$ANOVA$mean_n

  has_lsd <- !is.null(anova_results_list$LSD$lsd_mmd) && !is.na(anova_results_list$LSD$lsd_mmd)
  if (has_lsd) {
    lsd_mmd <- anova_results_list$LSD$lsd_mmd
  }

  hl <- function(text) {
    if (highlight && KEY) {
      paste0("[", text, "]{custom-style=\"highlight-yellow\"}")
    } else {
      as.character(text)
    }
  }

  if(p_value < 0.05) {
    posthoc_answer <- "Yes \u2013 we know there's a mean difference, but we don't know which groups are different from which others."
  } else {
    posthoc_answer <- "No \u2013 a nonsignificant Omnibus F-test"
  }

  if (KEY) {
    p_fmt <- format_p_value(p_value, include_p = TRUE)

    anova_text <- paste0(
      "F = ", hl(format_F(f_stat)), "    df = ", hl(format_df(df_between)), " , ", hl(format_df(df_within)),
      "    MSE = ", hl(format_mse(mse)), "    ", hl(p_fmt),
      "    N = ", hl(format_n(total_n)), "    k = ", hl(format_int(k)), "    n = ", hl(format_stat(mean_n)), "\n\n",
      "Do we need to perform LSD pairwise comparisons to test the RH? Why or why not? ",
      hl(posthoc_answer)
    )

    if (has_lsd) {
      anova_text <- paste0(anova_text, "\n\nLSDmmd = ", hl(format_stat(lsd_mmd)),
                           "  Based on the LSDmmd, use <, > & = signs to show the results of each pairwise comparison.")
    }
  } else {
    anova_text <- paste0(
      "F = ____           df = ____ , ____            MSE = ____           p = ____            N = ____        k = ____         n = ____\n\n",
      "## LSD, Pairwise Comparisons & RH: Testing\n\n",
      "\n\nDo we need to perform LSD pairwise comparisons to test the RH? Why or why not? ____________"
    )

    if (has_lsd) {
      anova_text <- paste0(anova_text, "\n\nLSDmmd = ____\n\n Based on the LSDmmd, use <, > & = signs to show the results of each pairwise comparison.")
    }
  }

  return(anova_text)
}

#' ANOVA Descriptives Table (Answer KEY)
#'
#' Creates a flextable showing group means and SDs with groups as columns.
#'
#' @param anova_results_list Output from [anova_kgroup_answers()].
#' @param group_labels Character vector or NULL. Display labels for groups.
#' @param KEY Logical. If TRUE (default), fill with values; if FALSE, blank.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' \dontrun{
#' result <- anova_kgroup_answers(data, "dv", "iv")
#' anova_descriptives_KEY(result, KEY = TRUE)
#' }
#'
#' @export
anova_descriptives_KEY <- function(anova_results_list,
                                   group_labels = NULL,
                                   KEY = TRUE) {
  desc_stats <- anova_results_list$Descriptives
  n_groups <- nrow(desc_stats)

  if (is.null(group_labels)) {
    group_labels <- desc_stats$group_label
  }

  if (KEY) {
    desc_data <- data.frame(
      ` ` = c("Mean", "Std"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    for (i in 1:n_groups) {
      desc_data[[group_labels[i]]] <- c(
        format_mean(desc_stats$mean[i]),
        format_sd(desc_stats$sd[i])
      )
    }
  } else {
    desc_data <- data.frame(
      ` ` = c("Mean", "Std"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    for (i in 1:n_groups) {
      desc_data[[group_labels[i]]] <- c("", "")
    }
  }

  desc_data <- tibble::as_tibble(desc_data)

  ft <- flextable::flextable(desc_data) |>
    flextable::set_header_labels(` ` = "") |>
    flextable::theme_box() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::autofit()

  if (KEY) {
    ft <- ft |> flextable::color(j = 2:(n_groups + 1), color = "#d00000", part = "body")
  }

  return(ft)
}
