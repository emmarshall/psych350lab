# =============================================================================
# apa_anova_writeups.R
# APA Write-Up Functions for ANOVA Results
# =============================================================================

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Null coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Format Effect Size for APA Reporting
#'
#' Formats eta-squared or partial eta-squared with interpretation.
#'
#' @param es Numeric effect size value.
#' @param type Character. Type of effect size: "eta_sq", "partial_eta_sq", "omega_sq".
#' @param include_interpretation Logical. Include verbal interpretation.
#'
#' @return Character string with formatted effect size.
#'
#' @examples
#' # Format partial eta-squared effect size
#' format_effect_size_anova(0.125)
#'
#' # Include interpretation of effect size
#' format_effect_size_anova(0.03, include_interpretation = TRUE)
#'
#' # Use eta-squared instead
#' format_effect_size_anova(0.23, type = "eta_sq", include_interpretation = TRUE)
#'
#' @export
format_effect_size_anova <- function(es, type = "partial_eta_sq",
                                     include_interpretation = FALSE) {

  es_formatted <- format_effect(es)


  symbol <- switch(type,
                   "eta_sq" = .eta_sq_symbol(),
                   "partial_eta_sq" = paste0(.eta_sq_symbol(), "p"),
                   "omega_sq" = paste0(intToUtf8(0x03C9), intToUtf8(0x00B2)),
                   .eta_sq_symbol()
  )


  result <- paste0("*", symbol, "* = ", es_formatted)


  if (include_interpretation) {
    interp <- interpret_eta_sq(es)
    result <- paste0(result, " (", interp, " effect)")
  }


  result
}


# =============================================================================
# WITHIN-GROUPS ANOVA (Two Conditions)
# =============================================================================

#' APA Write-Up for Within-Groups ANOVA (Two Conditions)
#'
#' Generates an APA-style paragraph reporting a within-groups ANOVA result,
#' with hypothesis-driven conclusions.
#'
#' @param anova_results_list Output from wg_anova_answers().
#' @param condition_labels Character vector of length 2. Labels for the two conditions
#'   (e.g., c("critics score", "audience score")).
#' @param dv_name Character. Descriptive name for the DV construct
#'   (e.g., "favorable ratings").
#' @param hypothesis List with hypothesis details:
#'   \describe{
#'     \item{direction}{Expected direction: "condition1_higher", "condition2_higher", or NULL}
#'     \item{rh_text}{Optional. Full RH statement for custom phrasing.}
#'   }
#'   If NULL, generates a generic non-directional APA writeup.
#' @param alpha Significance level. Default 0.05.
#' @param subject Character or NULL. Subject description (e.g., "participants", "media").
#' @param include_effect_size Logical. Include effect size in output. Default FALSE.
#' @param effect_size_type Character. Type: "partial_eta_sq" or "eta_sq". Default "partial_eta_sq".
#'
#' @return A character string with the APA write-up.
#'
#' @examples
#' \dontrun{
#' result <- wg_anova_answers(data, "rt_critics_score", "rt_audience_score")
#'
#' writeup <- apa_wg_anova_writeup(
#'   result,
#'   condition_labels = c("critics", "audience members"),
#'   dv_name = "liking the movie",
#'   hypothesis = list(
#'     direction = "condition2_higher",
#'     rh_text = paste0("a higher percentage of audience members ",
#'       "will give the media a favorable review than critics")
#'   ),
#'   subject = "raters"
#' )
#' cat(writeup)
#' }
#'
#' @export
apa_wg_anova_writeup <- function(anova_results_list,
                                 condition_labels,
                                 dv_name,
                                 hypothesis = NULL,
                                 alpha = 0.05,
                                 subject = NULL,
                                 include_effect_size = FALSE,
                                 effect_size_type = "partial_eta_sq") {


  # Extract values
  f_val <- anova_results_list$ANOVA$F
  p_val <- anova_results_list$ANOVA$p_value
  mse <- anova_results_list$ANOVA$mse


  df_effect <- anova_results_list$ANOVA$df_effect %||%
    anova_results_list$ANOVA$df_between %||% 1
  df_error <- anova_results_list$ANOVA$df_error %||%
    anova_results_list$ANOVA$df_within


  desc_stats <- anova_results_list$Descriptives


  # Extract condition stats
  cond1_mean <- desc_stats$mean[1]
  cond1_sd <- desc_stats$sd[1]
  cond2_mean <- desc_stats$mean[2]
  cond2_sd <- desc_stats$sd[2]


  is_significant <- p_val < alpha
  actual_direction <- if (cond1_mean > cond2_mean) "condition1_higher" else "condition2_higher"


  # Format statistics
  f_text <- format_F(f_val)
  df_eff_text <- format_df(df_effect)
  df_err_text <- format_df(df_error)
  mse_text <- format_mse(mse)
  p_text <- format_p_value(p_val)


  m1 <- format_mean(cond1_mean)
  sd1 <- format_sd(cond1_sd)
  m2 <- format_mean(cond2_mean)
  sd2 <- format_sd(cond2_sd)


  subject_word <- subject %||% "participants"


  # Build effect size string if requested
  es_string <- ""
  if (include_effect_size) {
    es_value <- if (effect_size_type == "eta_sq") {
      anova_results_list$ANOVA$eta_sq
    } else {
      anova_results_list$ANOVA$partial_eta_sq
    }
    if (!is.null(es_value)) {
      es_string <- paste0(", ", format_effect_size_anova(es_value, effect_size_type))
    }
  }


  # Build main statistical sentence
  if (is_significant) {
    stat_sentence <- glue::glue(
      "There was a significant difference between the {condition_labels[1]} and the {condition_labels[2]}, ",
      "*F*({df_eff_text}, {df_err_text}) = {f_text}, *MSe* = {mse_text}, *p* = {p_text}{es_string}."
    )
  } else {
    stat_sentence <- glue::glue(
      "There was no significant difference between the {condition_labels[1]} and the {condition_labels[2]}, ",
      "*F*({df_eff_text}, {df_err_text}) = {f_text}, *MSe* = {mse_text}, *p* = {p_text}{es_string}."
    )
  }


  # Build conclusion
  if (!is.null(hypothesis)) {
    expected_direction <- hypothesis$direction


    if (is_significant) {
      if (!is.null(expected_direction) && actual_direction == expected_direction) {
        conclusion_start <- "Consistent with the research hypothesis"
      } else if (!is.null(expected_direction)) {
        conclusion_start <- "Contrary to the research hypothesis"
      } else {
        conclusion_start <- "Consistent with the research hypothesis"
      }
    } else {
      conclusion_start <- "Contrary to the research hypothesis"
    }


    if (is_significant) {
      if (cond1_mean > cond2_mean) {
        higher_label <- condition_labels[1]
        lower_label <- condition_labels[2]
        higher_m <- m1
        higher_sd <- sd1
        lower_m <- m2
        lower_sd <- sd2
      } else {
        higher_label <- condition_labels[2]
        lower_label <- condition_labels[1]
        higher_m <- m2
        higher_sd <- sd2
        lower_m <- m1
        lower_sd <- sd1
      }


      conclusion <- glue::glue(
        " {conclusion_start}, more {higher_label} (*M* = {higher_m}, *SD* = {higher_sd}) ",
        "reported {dv_name} than {lower_label} (*M* = {lower_m}, *SD* = {lower_sd})."
      )
    } else {
      conclusion <- glue::glue(
        " {conclusion_start}, {condition_labels[1]} (*M* = {m1}, *SD* = {sd1}) ",
        "and {condition_labels[2]} (*M* = {m2}, *SD* = {sd2}) did not significantly differ in {dv_name}."
      )
    }


    writeup <- paste0(stat_sentence, conclusion)


  } else {
    # No hypothesis provided - generic conclusion
    if (is_significant) {
      if (cond1_mean > cond2_mean) {
        conclusion <- glue::glue(
          " {condition_labels[1]} (*M* = {m1}, *SD* = {sd1}) scored significantly higher ",
          "than {condition_labels[2]} (*M* = {m2}, *SD* = {sd2})."
        )
      } else {
        conclusion <- glue::glue(
          " {condition_labels[2]} (*M* = {m2}, *SD* = {sd2}) scored significantly higher ",
          "than {condition_labels[1]} (*M* = {m1}, *SD* = {sd1})."
        )
      }
    } else {
      conclusion <- glue::glue(
        " {condition_labels[1]} (*M* = {m1}, *SD* = {sd1}) and ",
        "{condition_labels[2]} (*M* = {m2}, *SD* = {sd2}) did not significantly differ."
      )
    }


    writeup <- paste0(stat_sentence, conclusion)
  }


  as.character(writeup)
}


# =============================================================================
# BETWEEN-GROUPS ANOVA (Two Groups)
# =============================================================================

#' APA Write-Up for Between-Groups ANOVA (Two Groups)
#'
#' Generates an APA-style paragraph reporting a between-groups ANOVA result,
#' with hypothesis-driven conclusions that compare expected vs. actual results.
#'
#' @param anova_results_list Output from bg_anova_answers().
#' @param iv_name Character. Descriptive name for the IV (e.g., "condition").
#' @param dv_name Character. Descriptive name for the DV (e.g., "recall accuracy").
#' @param group_labels Character vector of length 2. Labels for IV levels
#'   (e.g., c("control", "experimental")).
#' @param hypothesis List with hypothesis details:
#'   \describe{
#'     \item{direction}{Expected direction: "group1_higher", "group2_higher", or NULL}
#'     \item{rh_text}{Optional. Full RH statement for custom phrasing.}
#'   }
#'   If NULL, generates a generic non-directional APA writeup.
#' @param alpha Significance level. Default 0.05.
#' @param include_effect_size Logical. Include effect size in output. Default FALSE.
#' @param effect_size_type Character. Type: "partial_eta_sq", "eta_sq", or "omega_sq".
#'   Default "partial_eta_sq".
#'
#' @return A character string with the APA write-up.
#'
#' @examples
#' \dontrun{
#' result <- bg_anova_answers(data, "condition", "score")
#'
#' writeup <- apa_bg_anova_writeup(
#'   result,
#'   iv_name = "training condition",
#'   dv_name = "test performance",
#'   group_labels = c("control", "trained"),
#'   hypothesis = list(
#'     direction = "group2_higher",
#'     rh_text = "trained participants would perform better than control participants"
#'   )
#' )
#' cat(writeup)
#' }
#'
#' @export
apa_bg_anova_writeup <- function(anova_results_list,
                                 iv_name,
                                 dv_name,
                                 group_labels = NULL,
                                 hypothesis = NULL,
                                 alpha = 0.05,
                                 include_effect_size = FALSE,
                                 effect_size_type = "partial_eta_sq") {


  # Extract values
  f_val <- anova_results_list$ANOVA$F
  p_val <- anova_results_list$ANOVA$p_value
  df_between <- anova_results_list$ANOVA$df_between
  df_within <- anova_results_list$ANOVA$df_within
  mse <- anova_results_list$ANOVA$mse


  desc_stats <- anova_results_list$Descriptives


  if (is.null(group_labels)) {
    group_labels <- as.character(desc_stats$group_label %||% desc_stats$group %||% desc_stats$iv)
  }


  group1_mean <- desc_stats$mean[1]
  group1_sd <- desc_stats$sd[1]
  group2_mean <- desc_stats$mean[2]
  group2_sd <- desc_stats$sd[2]


  is_significant <- p_val < alpha
  actual_direction <- if (group1_mean > group2_mean) "group1_higher" else "group2_higher"


  # Format statistics
  f_text <- format_F(f_val)
  df_b_text <- format_df(df_between)
  df_w_text <- format_df(df_within)
  mse_text <- format_mse(mse)
  p_text <- format_p_value(p_val)


  m1 <- format_mean(group1_mean)
  sd1 <- format_sd(group1_sd)
  m2 <- format_mean(group2_mean)
  sd2 <- format_sd(group2_sd)


  # Effect size string
  es_string <- ""
  if (include_effect_size) {
    es_value <- if (effect_size_type == "eta_sq") {
      anova_results_list$ANOVA$eta_sq
    } else if (effect_size_type == "omega_sq") {
      anova_results_list$ANOVA$omega_sq
    } else {
      anova_results_list$ANOVA$partial_eta_sq
    }
    if (!is.null(es_value)) {
      es_string <- paste0(", ", format_effect_size_anova(es_value, effect_size_type))
    }
  }


  # Build main sentence
  if (is_significant) {
    stat_sentence <- glue::glue(
      "There was a significant difference in {dv_name} between {group_labels[1]} and {group_labels[2]}, ",
      "*F*({df_b_text}, {df_w_text}) = {f_text}, *MSe* = {mse_text}, *p* = {p_text}{es_string}."
    )
  } else {
    stat_sentence <- glue::glue(
      "There was no significant difference in {dv_name} between {group_labels[1]} and {group_labels[2]}, ",
      "*F*({df_b_text}, {df_w_text}) = {f_text}, *MSe* = {mse_text}, *p* = {p_text}{es_string}."
    )
  }


  # Build conclusion
  if (!is.null(hypothesis)) {
    expected_direction <- hypothesis$direction


    if (is_significant) {
      if (!is.null(expected_direction) && actual_direction == expected_direction) {
        conclusion_start <- "Consistent with the research hypothesis"
      } else if (!is.null(expected_direction)) {
        conclusion_start <- "Contrary to the research hypothesis"
      } else {
        conclusion_start <- "As expected"
      }
    } else {
      conclusion_start <- "Contrary to the research hypothesis"
    }


    if (is_significant) {
      if (group1_mean > group2_mean) {
        higher_label <- group_labels[1]
        lower_label <- group_labels[2]
        higher_m <- m1
        higher_sd <- sd1
        lower_m <- m2
        lower_sd <- sd2
      } else {
        higher_label <- group_labels[2]
        lower_label <- group_labels[1]
        higher_m <- m2
        higher_sd <- sd2
        lower_m <- m1
        lower_sd <- sd1
      }


      conclusion <- glue::glue(
        " {conclusion_start}, {higher_label} (*M* = {higher_m}, *SD* = {higher_sd}) ",
        "had significantly higher {dv_name} than {lower_label} (*M* = {lower_m}, *SD* = {lower_sd})."
      )
    } else {
      conclusion <- glue::glue(
        " {conclusion_start}, {group_labels[1]} (*M* = {m1}, *SD* = {sd1}) ",
        "and {group_labels[2]} (*M* = {m2}, *SD* = {sd2}) did not significantly differ."
      )
    }


    writeup <- paste0(stat_sentence, conclusion)


  } else {
    # No hypothesis - generic conclusion
    if (is_significant) {
      if (group1_mean > group2_mean) {
        conclusion <- glue::glue(
          " {group_labels[1]} (*M* = {m1}, *SD* = {sd1}) scored significantly higher ",
          "than {group_labels[2]} (*M* = {m2}, *SD* = {sd2})."
        )
      } else {
        conclusion <- glue::glue(
          " {group_labels[2]} (*M* = {m2}, *SD* = {sd2}) scored significantly higher ",
          "than {group_labels[1]} (*M* = {m1}, *SD* = {sd1})."
        )
      }
    } else {
      conclusion <- glue::glue(
        " {group_labels[1]} (*M* = {m1}, *SD* = {sd1}) and ",
        "{group_labels[2]} (*M* = {m2}, *SD* = {sd2}) did not significantly differ."
      )
    }


    writeup <- paste0(stat_sentence, conclusion)
  }


  as.character(writeup)
}


# =============================================================================
# K-GROUP BETWEEN-GROUPS ANOVA WITH POST-HOC
# =============================================================================

#' APA Write-Up for K-Group ANOVA with Post-Hoc Tests
#'
#' Generates an APA-style paragraph for a one-way ANOVA with more than 2 groups,
#' including post-hoc pairwise comparison results and hypothesis evaluation.
#'
#' @param kgroup_results_list Output from anova_kgroup_answers().
#' @param dv_name Character. Descriptive name for DV (e.g., "ratings of relevance").
#' @param iv_name Character. Descriptive name for IV (e.g., "type of testimony").
#' @param group_labels Character vector. Labels for groups in order matching data.
#' @param hypothesis List with hypothesis details:
#'   \describe{
#'     \item{rh_text}{Optional. Full RH statement for intro.}
#'     \item{ranking}{Character vector specifying expected mean ordering from highest
#'       to lowest (e.g., c("BWS", "CSA", "EWT")). Use group_labels values.}
#'     \item{pairwise}{Optional. List of expected pairwise comparisons with format
#'       list(list(group1="A", group2="B", expected="sig"), ...). Use "sig" for
#'       expected significant difference, "ns" for expected non-significant.}
#'   }
#' @param alpha Significance level. Default 0.05.
#' @param posthoc_tests Character vector. Names of post-hoc tests used.
#'   Default c("LSD", "Tukey's HSD").
#' @param include_effect_size Logical. Include effect size. Default FALSE.
#' @param effect_size_type Character. Type of effect size. Default "partial_eta_sq".
#'
#' @return A character string with APA write-up.
#'
#' @examples
#' \dontrun{
#' result <- anova_kgroup_answers(data, "testimony_type", "relevance_rating")
#'
#' writeup <- apa_kgroup_anova_writeup(
#'   result,
#'   dv_name = "ratings of the importance of relevance",
#'   iv_name = "types of psychological testimony",
#'   group_labels = c("BWS", "CSA", "EWT"),
#'   hypothesis = list(
#'     rh_text = paste0("the relevance of battered woman syndrome ",
#'       "evidence will be seen as most important"),
#'     ranking = c("BWS", "CSA", "EWT"),
#'     pairwise = list(
#'       list(group1 = "BWS", group2 = "CSA", expected = "sig"),
#'       list(group1 = "BWS", group2 = "EWT", expected = "sig"),
#'       list(group1 = "CSA", group2 = "EWT", expected = "sig")
#'     )
#'   ),
#'   posthoc_tests = c("LSD", "Tukey's HSD")
#' )
#' cat(writeup)
#' }
#'
#' @export
apa_kgroup_anova_writeup <- function(kgroup_results_list,
                                     dv_name,
                                     iv_name,
                                     group_labels = NULL,
                                     hypothesis = NULL,
                                     alpha = 0.05,
                                     posthoc_tests = c("LSD", "Tukey's HSD"),
                                     include_effect_size = FALSE,
                                     effect_size_type = "partial_eta_sq") {


  # Extract ANOVA values
  f_val <- kgroup_results_list$ANOVA$F
  p_val <- kgroup_results_list$ANOVA$p_value
  df_between <- kgroup_results_list$ANOVA$df_between
  df_within <- kgroup_results_list$ANOVA$df_within
  mse <- kgroup_results_list$ANOVA$mse


  desc_stats <- kgroup_results_list$Descriptives
  pairwise <- kgroup_results_list$Pairwise


  if (is.null(group_labels)) {
    group_labels <- as.character(desc_stats$group_label %||% desc_stats$group)
  }


  is_significant <- p_val < alpha


  # Format statistics
  f_text <- format_F(f_val)
  df_b_text <- format_df(df_between)
  df_w_text <- format_df(df_within)
  mse_text <- format_mse(mse)
  p_text <- format_p_value(p_val)


  # Effect size
  es_string <- ""
  if (include_effect_size) {
    es_value <- if (effect_size_type == "eta_sq") {
      kgroup_results_list$ANOVA$eta_sq
    } else if (effect_size_type == "omega_sq") {
      kgroup_results_list$ANOVA$omega_sq
    } else {
      kgroup_results_list$ANOVA$partial_eta_sq
    }
    if (!is.null(es_value)) {
      es_string <- paste0(", ", format_effect_size_anova(es_value, effect_size_type))
    }
  }


  # Post-hoc test string
  posthoc_string <- if (length(posthoc_tests) == 1) {
    posthoc_tests
  } else if (length(posthoc_tests) == 2) {
    paste(posthoc_tests, collapse = " and ")
  } else {
    paste(c(paste(posthoc_tests[-length(posthoc_tests)], collapse = ", "),
            posthoc_tests[length(posthoc_tests)]), collapse = ", and ")
  }


  # Omnibus result
  if (is_significant) {
    omnibus <- glue::glue(
      "There was a significant difference in {dv_name} across {iv_name}, ",
      "*F*({df_b_text}, {df_w_text}) = {f_text}, *MSe* = {mse_text}, *p* = {p_text}{es_string}."
    )
  } else {
    omnibus <- glue::glue(
      "There was no significant difference in {dv_name} across {iv_name}, ",
      "*F*({df_b_text}, {df_w_text}) = {f_text}, *MSe* = {mse_text}, *p* = {p_text}{es_string}."
    )
  }


  writeup <- as.character(omnibus)


  # Post-hoc results if significant
  if (is_significant && !is.null(pairwise) && length(pairwise) > 0) {
    writeup <- paste0(writeup, " ", posthoc_string, " were used as follow up tests.")


    # Create lookup for group means and SDs
    group_means <- stats::setNames(desc_stats$mean, group_labels)
    group_sds <- stats::setNames(desc_stats$sd, group_labels)


    # Categorize comparisons
    sig_comps <- list()
    nonsig_comps <- list()


    for (comp in pairwise) {
      groups <- trimws(strsplit(comp$comparison, " vs ")[[1]])
      g1 <- groups[1]
      g2 <- groups[2]


      is_sig <- if (!is.null(comp$lsd_result)) {
        comp$lsd_result != "="
      } else if (!is.null(comp$tukey_result)) {
        comp$tukey_result != "="
      } else if (!is.null(comp$p_value)) {
        comp$p_value < alpha
      } else {
        FALSE
      }


      comp_info <- list(
        group1 = g1,
        group2 = g2,
        mean1 = group_means[g1],
        sd1 = group_sds[g1],
        mean2 = group_means[g2],
        sd2 = group_sds[g2]
      )


      if (is_sig) {
        sig_comps <- c(sig_comps, list(comp_info))
      } else {
        nonsig_comps <- c(nonsig_comps, list(comp_info))
      }
    }


    # Build hypothesis-driven narrative
    if (!is.null(hypothesis) && !is.null(hypothesis$pairwise)) {
      n_supported <- 0
      n_total <- length(hypothesis$pairwise)


      for (ph in hypothesis$pairwise) {
        actual_sig <- FALSE
        for (comp in pairwise) {
          groups <- trimws(strsplit(comp$comparison, " vs ")[[1]])
          if ((ph$group1 %in% groups) && (ph$group2 %in% groups)) {
            actual_sig <- if (!is.null(comp$lsd_result)) {
              comp$lsd_result != "="
            } else if (!is.null(comp$tukey_result)) {
              comp$tukey_result != "="
            } else {
              comp$p_value < alpha
            }
            break
          }
        }


        if ((ph$expected == "sig" && actual_sig) ||
            (ph$expected == "ns" && !actual_sig)) {
          n_supported <- n_supported + 1
        }
      }


      # Report supported significant comparisons
      if (length(sig_comps) > 0) {
        all_groups <- unique(unlist(lapply(sig_comps, function(x) c(x$group1, x$group2))))
        highest_group <- all_groups[which.max(group_means[all_groups])]
        m_highest <- format_mean(group_means[highest_group])
        sd_highest <- format_sd(group_sds[highest_group])


        lower_groups <- setdiff(all_groups, highest_group)
        lower_parts <- sapply(lower_groups, function(g) {
          m <- format_mean(group_means[g])
          sd <- format_sd(group_sds[g])
          glue::glue("{g} (*M* = {m}, *SD* = {sd})")
        })
        lower_string <- paste(lower_parts, collapse = " and ")


        if (n_supported == n_total) {
          supported_text <- glue::glue(
            " Consistent with the research hypothesis, {highest_group} was rated as significantly ",
            "higher (*M* = {m_highest}, *SD* = {sd_highest}) than {lower_string}."
          )
        } else {
          supported_text <- glue::glue(
            " {highest_group} (*M* = {m_highest}, *SD* = {sd_highest}) was significantly ",
            "higher than {lower_string}."
          )
        }
        writeup <- paste0(writeup, supported_text)
      }


      # Report unsupported predictions
      for (ph in hypothesis$pairwise) {
        if (ph$expected == "sig") {
          actual_sig <- FALSE
          for (comp in pairwise) {
            groups <- trimws(strsplit(comp$comparison, " vs ")[[1]])
            if ((ph$group1 %in% groups) && (ph$group2 %in% groups)) {
              actual_sig <- if (!is.null(comp$lsd_result)) {
                comp$lsd_result != "="
              } else if (!is.null(comp$tukey_result)) {
                comp$tukey_result != "="
              } else {
                comp$p_value < alpha
              }
              break
            }
          }
          if (!actual_sig) {
            contrary_text <- glue::glue(
              " Contrary to hypothesis, {ph$group1} and {ph$group2} were not significantly different."
            )
            writeup <- paste0(writeup, contrary_text)
          }
        }
      }


    } else {
      # No hypothesis - report descriptively
      if (length(sig_comps) > 0) {
        sig_parts <- sapply(sig_comps, function(comp) {
          m1 <- format_mean(comp$mean1)
          sd1 <- format_sd(comp$sd1)
          m2 <- format_mean(comp$mean2)
          sd2 <- format_sd(comp$sd2)


          if (comp$mean1 > comp$mean2) {
            glue::glue("{comp$group1} (*M* = {m1}, *SD* = {sd1}) was significantly higher than {comp$group2} (*M* = {m2}, *SD* = {sd2})")
          } else {
            glue::glue("{comp$group2} (*M* = {m2}, *SD* = {sd2}) was significantly higher than {comp$group1} (*M* = {m1}, *SD* = {sd1})")
          }
        })
        writeup <- paste0(writeup, " ", paste(sig_parts, collapse = ". "), ".")
      }


      if (length(nonsig_comps) > 0) {
        nonsig_parts <- sapply(nonsig_comps, function(comp) {
          paste(comp$group1, "and", comp$group2)
        })
        writeup <- paste0(writeup, " ", paste(nonsig_parts, collapse = ", "),
                          " did not significantly differ.")
      }
    }


  } else if (!is_significant) {
    # Report group means for non-significant
    desc_parts <- sapply(seq_along(group_labels), function(i) {
      m <- format_mean(desc_stats$mean[i])
      sd <- format_sd(desc_stats$sd[i])
      glue::glue("{group_labels[i]} (*M* = {m}, *SD* = {sd})")
    })


    desc_string <- if (length(desc_parts) == 2) {
      paste(desc_parts, collapse = " and ")
    } else {
      paste(c(paste(desc_parts[-length(desc_parts)], collapse = ", "),
              desc_parts[length(desc_parts)]), collapse = ", and ")
    }


    writeup <- paste0(writeup, " Group means were: ", desc_string, ".")


    if (!is.null(hypothesis)) {
      writeup <- paste0(writeup, " This result does not support the research hypothesis.")
    }
  }


  as.character(writeup)
}


# =============================================================================
# K-GROUP WITHIN-GROUPS ANOVA WITH POST-HOC
# =============================================================================

#' APA Write-Up for K-Level Within-Groups ANOVA with Post-Hoc
#'
#' Generates an APA-style paragraph for a repeated measures ANOVA with more than
#' 2 conditions, including post-hoc pairwise comparison results.
#'
#' @param wg_kgroup_results_list Output from wg_anova_kgroup_answers().
#' @param dv_name Character. Descriptive name for DV.
#' @param iv_name Character. Descriptive name for IV (the within-subjects factor).
#' @param condition_labels Character vector. Labels for conditions in order.
#' @param hypothesis List with hypothesis details (same structure as apa_kgroup_anova_writeup()).
#' @param alpha Significance level. Default 0.05.
#' @param posthoc_tests Character vector. Names of post-hoc tests used.
#' @param include_effect_size Logical. Include effect size. Default FALSE.
#' @param effect_size_type Character. Type of effect size. Default "partial_eta_sq".
#'
#' @return A character string with APA write-up.
#'
#' @export
apa_wg_kgroup_anova_writeup <- function(wg_kgroup_results_list,
                                        dv_name,
                                        iv_name,
                                        condition_labels = NULL,
                                        hypothesis = NULL,
                                        alpha = 0.05,
                                        posthoc_tests = c("LSD"),
                                        include_effect_size = FALSE,
                                        effect_size_type = "partial_eta_sq") {


  # Extract ANOVA values
  f_val <- wg_kgroup_results_list$ANOVA$F
  p_val <- wg_kgroup_results_list$ANOVA$p_value
  mse <- wg_kgroup_results_list$ANOVA$mse


  df_effect <- wg_kgroup_results_list$ANOVA$df_effect %||%
    wg_kgroup_results_list$ANOVA$df_between
  df_error <- wg_kgroup_results_list$ANOVA$df_error %||%
    wg_kgroup_results_list$ANOVA$df_within


  desc_stats <- wg_kgroup_results_list$Descriptives
  pairwise <- wg_kgroup_results_list$Pairwise


  if (is.null(condition_labels)) {
    condition_labels <- as.character(desc_stats$condition_label %||% desc_stats$condition)
  }


  is_significant <- p_val < alpha


  # Format statistics
  f_text <- format_F(f_val)
  df_eff_text <- format_df(df_effect)
  df_err_text <- format_df(df_error)
  mse_text <- format_mse(mse)
  p_text <- format_p_value(p_val)


  # Effect size
  es_string <- ""
  if (include_effect_size) {
    es_value <- if (effect_size_type == "eta_sq") {
      wg_kgroup_results_list$ANOVA$eta_sq
    } else {
      wg_kgroup_results_list$ANOVA$partial_eta_sq
    }
    if (!is.null(es_value)) {
      es_string <- paste0(", ", format_effect_size_anova(es_value, effect_size_type))
    }
  }


  posthoc_string <- paste(posthoc_tests, collapse = " and ")


  # Omnibus result
  if (is_significant) {
    omnibus <- glue::glue(
      "There was a significant difference in {dv_name} across {iv_name}, ",
      "*F*({df_eff_text}, {df_err_text}) = {f_text}, *MSe* = {mse_text}, *p* = {p_text}{es_string}."
    )
  } else {
    omnibus <- glue::glue(
      "There was no significant difference in {dv_name} across {iv_name}, ",
      "*F*({df_eff_text}, {df_err_text}) = {f_text}, *MSe* = {mse_text}, *p* = {p_text}{es_string}."
    )
  }


  writeup <- as.character(omnibus)


  if (is_significant && !is.null(pairwise)) {
    writeup <- paste0(writeup, " ", posthoc_string, " was used as a follow up test.")


    # Create lookup
    cond_means <- stats::setNames(desc_stats$mean, condition_labels)
    cond_sds <- stats::setNames(desc_stats$sd, condition_labels)


    sig_comps <- list()
    nonsig_comps <- list()


    for (comp in pairwise) {
      groups <- trimws(strsplit(comp$comparison, " vs ")[[1]])
      g1 <- groups[1]
      g2 <- groups[2]


      is_sig <- if (!is.null(comp$lsd_result)) {
        comp$lsd_result != "="
      } else {
        comp$p_value < alpha
      }


      comp_info <- list(
        cond1 = g1, cond2 = g2,
        mean1 = cond_means[g1], sd1 = cond_sds[g1],
        mean2 = cond_means[g2], sd2 = cond_sds[g2]
      )


      if (is_sig) {
        sig_comps <- c(sig_comps, list(comp_info))
      } else {
        nonsig_comps <- c(nonsig_comps, list(comp_info))
      }
    }


    if (length(sig_comps) > 0) {
      sig_parts <- sapply(sig_comps, function(comp) {
        m1 <- format_mean(comp$mean1)
        sd1 <- format_sd(comp$sd1)
        m2 <- format_mean(comp$mean2)
        sd2 <- format_sd(comp$sd2)


        if (comp$mean1 > comp$mean2) {
          glue::glue("{comp$cond1} (*M* = {m1}, *SD* = {sd1}) was significantly higher than {comp$cond2} (*M* = {m2}, *SD* = {sd2})")
        } else {
          glue::glue("{comp$cond2} (*M* = {m2}, *SD* = {sd2}) was significantly higher than {comp$cond1} (*M* = {m1}, *SD* = {sd1})")
        }
      })
      writeup <- paste0(writeup, " ", paste(sig_parts, collapse = ". "), ".")
    }


    if (length(nonsig_comps) > 0) {
      nonsig_pairs <- sapply(nonsig_comps, function(comp) {
        paste(comp$cond1, "and", comp$cond2)
      })
      writeup <- paste0(writeup, " ", paste(nonsig_pairs, collapse = ", "),
                        " did not significantly differ.")
    }
  }


  as.character(writeup)
}


# =============================================================================
# PLANNED COMPARISONS
# =============================================================================

#' APA Write-Up for Planned Comparisons (Between-Groups)
#'
#' Generates an APA-style paragraph for planned t-test comparisons against a
#' reference group, without omnibus F-test.
#'
#' @param planned_results_list Output from planned comparison analysis containing:
#'   \describe{
#'     \item{Descriptives}{Data frame with group_label, mean, sd, n}
#'     \item{Contrasts}{List of contrast results, each with comparison, t, df, p_value}
#'   }
#' @param dv_name Character. Descriptive name for DV.
#' @param reference_group Character. Label of the reference group being compared against.
#' @param comparison_groups Character vector. Labels of groups compared to reference.
#' @param hypothesis List with hypothesis details:
#'   \describe{
#'     \item{direction}{Expected direction: "reference_higher" or "reference_lower"}
#'     \item{rh_text}{Optional. Full RH statement.}
#'   }
#' @param alpha Significance level. Default 0.05.
#' @param include_effect_size Logical. Include effect sizes. Default FALSE.
#'
#' @return A character string with APA write-up.
#'
#' @examples
#' \dontrun{
#' writeup <- apa_planned_comparisons_writeup(
#'   results,
#'   dv_name = "ratings of the importance of relevance",
#'   reference_group = "BWS",
#'   comparison_groups = c("CSA", "EWT"),
#'   hypothesis = list(
#'     direction = "reference_higher",
#'     rh_text = paste0("BWS evidence will be seen as more ",
#'       "important than CSA and EWT evidence")
#'   )
#' )
#' }
#'
#' @export
apa_planned_comparisons_writeup <- function(planned_results_list,
                                            dv_name,
                                            reference_group,
                                            comparison_groups,
                                            hypothesis = NULL,
                                            alpha = 0.05,
                                            include_effect_size = FALSE) {


  desc_stats <- planned_results_list$Descriptives
  contrasts <- planned_results_list$Contrasts


  # Create lookup
  group_means <- stats::setNames(desc_stats$mean, desc_stats$group_label)
  group_sds <- stats::setNames(desc_stats$sd, desc_stats$group_label)


  ref_m <- format_mean(group_means[reference_group])
  ref_sd <- format_sd(group_sds[reference_group])


  # Track hypothesis support
  all_supported <- TRUE
  any_supported <- FALSE
  contrast_texts <- c()


  for (comp_group in comparison_groups) {
    # Find contrast result
    contrast <- NULL
    for (c in contrasts) {
      if (!is.null(c$comparison_group) && c$comparison_group == comp_group) {
        contrast <- c
        break
      }
      if (!is.null(c$comparison) && grepl(comp_group, c$comparison)) {
        contrast <- c
        break
      }
    }


    if (is.null(contrast)) next


    is_sig <- contrast$p_value < alpha
    comp_m <- format_mean(group_means[comp_group])
    comp_sd <- format_sd(group_sds[comp_group])
    t_text <- format_t(contrast$t)
    df_text <- format_df(contrast$df)
    p_text <- format_p_value(contrast$p_value)


    # Check direction
    ref_higher <- group_means[reference_group] > group_means[comp_group]


    # Check hypothesis support
    if (!is.null(hypothesis$direction)) {
      if (hypothesis$direction == "reference_higher") {
        supported <- is_sig && ref_higher
      } else {
        supported <- is_sig && !ref_higher
      }
    } else {
      supported <- is_sig
    }


    if (supported) any_supported <- TRUE else all_supported <- FALSE


    # Build contrast text
    es_text <- ""
    if (include_effect_size && !is.null(contrast$effect_size_r)) {
      es_text <- paste0(", *r* = ", format_r(contrast$effect_size_r))
    }


    contrast_texts <- c(contrast_texts, glue::glue(
      "{comp_group} (*M* = {comp_m}, *SD* = {comp_sd}), *t*({df_text}) = {t_text}, *p* = {p_text}{es_text}"
    ))
  }


  # Build intro
  if (!is.null(hypothesis)) {
    if (all_supported && any_supported) {
      intro <- "Consistent with the research hypothesis"
    } else if (any_supported) {
      intro <- "Partially consistent with the research hypothesis"
    } else {
      intro <- "Contrary to the research hypothesis"
    }
  } else {
    intro <- "Results indicated that"
  }


  # Combine
  comp_string <- if (length(contrast_texts) == 1) {
    contrast_texts
  } else if (length(contrast_texts) == 2) {
    paste(contrast_texts, collapse = ", and ")
  } else {
    paste(c(paste(contrast_texts[-length(contrast_texts)], collapse = ", "),
            contrast_texts[length(contrast_texts)]), collapse = ", and ")
  }


  writeup <- glue::glue(
    "{intro}, {dv_name} for {reference_group} (*M* = {ref_m}, *SD* = {ref_sd}) ",
    "was rated as significantly more important than {comp_string}."
  )


  as.character(writeup)
}


#' APA Write-Up for Within-Groups Simple Contrasts
#'
#' Generates an APA-style paragraph for repeated measures ANOVA with simple
#' (planned) contrasts comparing conditions to a reference.
#'
#' @param wg_contrast_results_list Output from within-groups ANOVA with contrasts containing:
#'   \describe{
#'     \item{ANOVA}{List with F, p_value, df_effect, df_error, mse}
#'     \item{Descriptives}{Data frame with condition_label, mean, sd}
#'     \item{Contrasts}{Optional. List of contrast results if computed separately}
#'   }
#' @param dv_name Character. Descriptive name for DV.
#' @param iv_name Character. Descriptive name for IV (conditions).
#' @param condition_labels Character vector. Labels for conditions.
#' @param reference_condition Character. Label of reference condition.
#' @param hypothesis List with hypothesis details:
#'   \describe{
#'     \item{direction}{Expected direction: "reference_higher" or "reference_lower"}
#'     \item{rh_text}{Optional. Full RH statement.}
#'   }
#' @param alpha Significance level. Default 0.05.
#'
#' @return A character string with APA write-up.
#'
#' @examples
#' \dontrun{
#' writeup <- apa_wg_simple_contrasts_writeup(
#'   results,
#'   dv_name = "judges' ratings of importance",
#'   iv_name = "criterion type",
#'   condition_labels = c("relevance", "assist_trier", "qualifications"),
#'   reference_condition = "assist_trier",
#'   hypothesis = list(
#'     direction = "reference_higher",
#'     rh_text = paste0("assisting the trier of fact will be ",
#'       "more important than relevance and qualifications")
#'   )
#' )
#' }
#'
#' @export
apa_wg_simple_contrasts_writeup <- function(wg_contrast_results_list,
                                            dv_name,
                                            iv_name,
                                            condition_labels,
                                            reference_condition,
                                            hypothesis = NULL,
                                            alpha = 0.05) {


  # Extract ANOVA values
  f_val <- wg_contrast_results_list$ANOVA$F
  p_val <- wg_contrast_results_list$ANOVA$p_value
  mse <- wg_contrast_results_list$ANOVA$mse


  df_effect <- wg_contrast_results_list$ANOVA$df_effect %||%
    wg_contrast_results_list$ANOVA$df_between
  df_error <- wg_contrast_results_list$ANOVA$df_error %||%
    wg_contrast_results_list$ANOVA$df_within


  desc_stats <- wg_contrast_results_list$Descriptives


  is_significant <- p_val < alpha


  # Format statistics
  f_text <- format_F(f_val)
  df_eff_text <- format_df(df_effect)
  df_err_text <- format_df(df_error)
  mse_text <- format_mse(mse)
  p_text <- format_p_value(p_val)


  # Create condition labels string
  cond_string <- if (length(condition_labels) == 2) {
    paste(condition_labels, collapse = " and ")
  } else {
    paste(c(paste(condition_labels[-length(condition_labels)], collapse = ", "),
            condition_labels[length(condition_labels)]), collapse = ", and ")
  }


  # Omnibus result
  if (is_significant) {
    omnibus <- glue::glue(
      "There was a significant difference in {dv_name} for {cond_string}, ",
      "*F*({df_eff_text}, {df_err_text}) = {f_text}, *MSe* = {mse_text}, *p* = {p_text}."
    )
  } else {
    omnibus <- glue::glue(
      "There was no significant difference in {dv_name} for {cond_string}, ",
      "*F*({df_eff_text}, {df_err_text}) = {f_text}, *MSe* = {mse_text}, *p* = {p_text}."
    )
  }


  writeup <- paste0(omnibus, " Simple contrasts were used as follow up tests.")


  # Build contrast results
  if (is_significant) {
    # Create lookup
    cond_means <- stats::setNames(desc_stats$mean,
                                  desc_stats$condition_label %||% condition_labels)
    cond_sds <- stats::setNames(desc_stats$sd,
                                desc_stats$condition_label %||% condition_labels)


    ref_m <- format_mean(cond_means[reference_condition])
    ref_sd <- format_sd(cond_sds[reference_condition])


    # Other conditions
    other_conds <- setdiff(condition_labels, reference_condition)


    other_parts <- sapply(other_conds, function(cond) {
      m <- format_mean(cond_means[cond])
      sd <- format_sd(cond_sds[cond])
      glue::glue("{cond} (*M* = {m}, *SD* = {sd})")
    })


    other_string <- if (length(other_parts) == 1) {
      other_parts
    } else {
      paste(other_parts, collapse = " and ")
    }


    # Determine hypothesis support
    ref_is_highest <- all(cond_means[reference_condition] > cond_means[other_conds])


    if (!is.null(hypothesis)) {
      if (hypothesis$direction == "reference_higher" && ref_is_highest) {
        conclusion_start <- "Consistent with the research hypothesis"
      } else if (hypothesis$direction == "reference_lower" && !ref_is_highest) {
        conclusion_start <- "Consistent with the research hypothesis"
      } else {
        conclusion_start <- "Contrary to the research hypothesis"
      }
    } else {
      conclusion_start <- "Results indicated that"
    }


    conclusion <- glue::glue(
      " {conclusion_start}, {reference_condition} (*M* = {ref_m}, *SD* = {ref_sd}) ",
      "was rated higher than {other_string}."
    )


    writeup <- paste0(writeup, conclusion)
  }


  as.character(writeup)
}


# =============================================================================
# 2x2 FACTORIAL ANOVAs
# =============================================================================

#' APA Write-Up for 2x2 Between-Groups Factorial ANOVA
#'
#' Generates an APA-style paragraph for a two-way between-groups ANOVA,
#' reporting interaction and main effects with hypothesis evaluation.
#'
#' @param factorial_results_list Output from factorial ANOVA containing:
#'   \describe{
#'     \item{Interaction}{List with F, p_value, df_effect, df_error, mse}
#'     \item{MainEffect_A}{List for factor A}
#'     \item{MainEffect_B}{List for factor B}
#'     \item{CellDescriptives}{Data frame with factor_a, factor_b, mean, sd, n}
#'     \item{MarginalMeans_A}{Data frame with level, mean, se or sd}
#'     \item{MarginalMeans_B}{Data frame with level, mean, se or sd}
#'     \item{SimpleEffects}{Optional. List of simple effect results}
#'   }
#' @param dv_name Character. Descriptive name for DV.
#' @param factor_a_name Character. Descriptive name for factor A.
#' @param factor_b_name Character. Descriptive name for factor B.
#' @param factor_a_labels Character vector of length 2. Labels for factor A levels.
#' @param factor_b_labels Character vector of length 2. Labels for factor B levels.
#' @param hypothesis List with hypothesis details:
#'   \describe{
#'     \item{interaction_rh}{Full RH statement for interaction.}
#'     \item{simple_effects}{List of expected simple effects}
#'     \item{main_a_direction}{Expected main effect direction for A}
#'     \item{main_b_direction}{Expected main effect direction for B}
#'   }
#' @param alpha Significance level. Default 0.05.
#' @param report_main_effects Logical. Report main effects. Default TRUE.
#'
#' @return A character string with APA write-up.
#'
#' @examples
#' \dontrun{
#' writeup <- apa_2x2_bg_factorial_writeup(
#'   results,
#'   dv_name = "importance ratings",
#'   factor_a_name = "gender",
#'   factor_b_name = "party",
#'   factor_a_labels = c("female", "male"),
#'   factor_b_labels = c("prosecution", "defense"),
#'   hypothesis = list(
#'     interaction_rh = "the effect of party will depend on gender",
#'     simple_effects = list(
#'       list(at_level = "female", direction = "b1_higher"),
#'       list(at_level = "male", direction = "b2_higher")
#'     )
#'   )
#' )
#' }
#'
#' @export
apa_2x2_bg_factorial_writeup <- function(factorial_results_list,
                                         dv_name,
                                         factor_a_name,
                                         factor_b_name,
                                         factor_a_labels,
                                         factor_b_labels,
                                         hypothesis = NULL,
                                         alpha = 0.05,
                                         report_main_effects = TRUE) {

  # Extract interaction results
  int <- factorial_results_list$Interaction
  int_f <- int$F
  int_p <- int$p_value
  int_df1 <- int$df_effect %||% int$df_between %||% 1
  int_df2 <- int$df_error %||% int$df_within
  int_mse <- int$mse

  int_sig <- int_p < alpha

  # Format interaction statistics
  int_f_text <- format_F(int_f)
  int_df1_text <- format_df(int_df1)
  int_df2_text <- format_df(int_df2)
  int_mse_text <- format_mse(int_mse)
  int_p_text <- format_p_value(int_p)

  # Get cell descriptives
  cell_desc <- factorial_results_list$CellDescriptives

  # Create cell mean lookup (factor_a x factor_b)
  # Assumes cell_desc has columns: factor_a (or similar), factor_b, mean, sd
  cell_lookup <- list()
  for (i in seq_len(nrow(cell_desc))) {
    a_level <- as.character(cell_desc[[1]][i])  # First column is factor A
    b_level <- as.character(cell_desc[[2]][i])  # Second column is factor B
    key <- paste(a_level, b_level, sep = "_")
    cell_lookup[[key]] <- list(
      mean = cell_desc$mean[i],
      sd = cell_desc$sd[i],
      n = cell_desc$n[i]
    )
  }

  # Build interaction sentence
  if (int_sig) {
    int_sentence <- glue::glue(
      "There was a significant interaction between {factor_a_name} and {factor_b_name} ",
      "as they relate to {dv_name}, ",
      "*F*({int_df1_text}, {int_df2_text}) = {int_f_text}, *MSe* = {int_mse_text}, *p* = {int_p_text}."
    )
  } else {
    int_sentence <- glue::glue(
      "There was not a significant interaction between {factor_a_name} and {factor_b_name} ",
      "as they relate to {dv_name}, ",
      "*F*({int_df1_text}, {int_df2_text}) = {int_f_text}, *MSe* = {int_mse_text}, *p* = {int_p_text}."
    )
  }

  writeup <- as.character(int_sentence)

  # Report simple effects for interaction
  if (!is.null(hypothesis$simple_effects)) {
    simple_effects <- hypothesis$simple_effects
    simple_results <- factorial_results_list$SimpleEffects

    for (se in simple_effects) {
      at_level <- se$at_level
      expected_direction <- se$direction  # "b1_higher" or "b2_higher"

      # Get cell means for this level of factor A
      key1 <- paste(at_level, factor_b_labels[1], sep = "_")
      key2 <- paste(at_level, factor_b_labels[2], sep = "_")

      # Try alternative key formats if needed
      if (is.null(cell_lookup[[key1]])) {
        # Try finding by matching
        for (k in names(cell_lookup)) {
          if (grepl(at_level, k, ignore.case = TRUE) &&
              grepl(factor_b_labels[1], k, ignore.case = TRUE)) {
            key1 <- k
            break
          }
        }
      }
      if (is.null(cell_lookup[[key2]])) {
        for (k in names(cell_lookup)) {
          if (grepl(at_level, k, ignore.case = TRUE) &&
              grepl(factor_b_labels[2], k, ignore.case = TRUE)) {
            key2 <- k
            break
          }
        }
      }

      m1 <- format_mean(cell_lookup[[key1]]$mean)
      sd1 <- format_sd(cell_lookup[[key1]]$sd)
      m2 <- format_mean(cell_lookup[[key2]]$mean)
      sd2 <- format_sd(cell_lookup[[key2]]$sd)

      # Determine actual direction
      actual_b1_higher <- cell_lookup[[key1]]$mean > cell_lookup[[key2]]$mean

      # Find simple effect test result if available
      se_sig <- NULL
      if (!is.null(simple_results)) {
        for (sr in simple_results) {
          if (grepl(at_level, sr$at_level, ignore.case = TRUE)) {
            se_sig <- sr$p_value < alpha
            break
          }
        }
      }

      # Determine hypothesis support
      if (expected_direction == "b1_higher") {
        supported <- (is.null(se_sig) || se_sig) && actual_b1_higher
      } else {
        supported <- (is.null(se_sig) || se_sig) && !actual_b1_higher
      }

      # Build simple effect sentence
      if (supported && (is.null(se_sig) || se_sig)) {
        se_intro <- "As hypothesized"
        if (actual_b1_higher) {
          se_text <- glue::glue(
            " {se_intro}, {at_level} judges rated {dv_name} as significantly more important ",
            "when proffered by the {factor_b_labels[1]} (*M* = {m1}, *SD* = {sd1}) ",
            "than when proffered by the {factor_b_labels[2]} (*M* = {m2}, *SD* = {sd2})."
          )
        } else {
          se_text <- glue::glue(
            " {se_intro}, {at_level} judges rated {dv_name} as significantly more important ",
            "when proffered by the {factor_b_labels[2]} (*M* = {m2}, *SD* = {sd2}) ",
            "than when proffered by the {factor_b_labels[1]} (*M* = {m1}, *SD* = {sd1})."
          )
        }
      } else {
        se_intro <- "Contrary to hypothesis"
        se_text <- glue::glue(
          " {se_intro}, there was no significant difference in {at_level} judges' ratings of {dv_name} ",
          "between experts proffered by the {factor_b_labels[2]} (*M* = {m2}, *SD* = {sd2}), ",
          "and by the {factor_b_labels[1]} (*M* = {m1}, *SD* = {sd1})."
        )
      }

      writeup <- paste0(writeup, se_text)
    }
  }

  # Report main effects
  if (report_main_effects) {
    # Main effect A
    me_a <- factorial_results_list$MainEffect_A
    me_a_f <- me_a$F
    me_a_p <- me_a$p_value
    me_a_df1 <- me_a$df_effect %||% me_a$df_between %||% 1
    me_a_df2 <- me_a$df_error %||% me_a$df_within
    me_a_mse <- me_a$mse
    me_a_sig <- me_a_p < alpha

    me_a_f_text <- format_F(me_a_f)
    me_a_df1_text <- format_df(me_a_df1)
    me_a_df2_text <- format_df(me_a_df2)
    me_a_mse_text <- format_mse(me_a_mse)
    me_a_p_text <- format_p_value(me_a_p)

    # Get marginal means for A
    marg_a <- factorial_results_list$MarginalMeans_A
    if (!is.null(marg_a)) {
      a1_m <- format_mean(marg_a$mean[1])
      a1_se_or_sd <- if (!is.null(marg_a$se)) format_stat(marg_a$se[1]) else format_sd(marg_a$sd[1])
      a2_m <- format_mean(marg_a$mean[2])
      a2_se_or_sd <- if (!is.null(marg_a$se)) format_stat(marg_a$se[2]) else format_sd(marg_a$sd[2])
      se_label <- if (!is.null(marg_a$se)) "SE" else "SD"
    }

    if (me_a_sig) {
      higher_a <- if (marg_a$mean[1] > marg_a$mean[2]) factor_a_labels[1] else factor_a_labels[2]
      lower_a <- if (marg_a$mean[1] > marg_a$mean[2]) factor_a_labels[2] else factor_a_labels[1]
      higher_a_m <- if (marg_a$mean[1] > marg_a$mean[2]) a1_m else a2_m
      higher_a_se <- if (marg_a$mean[1] > marg_a$mean[2]) a1_se_or_sd else a2_se_or_sd
      lower_a_m <- if (marg_a$mean[1] > marg_a$mean[2]) a2_m else a1_m
      lower_a_se <- if (marg_a$mean[1] > marg_a$mean[2]) a2_se_or_sd else a1_se_or_sd

      me_a_text <- glue::glue(
        " There was a significant main effect for {factor_a_name}, ",
        "*F*({me_a_df1_text}, {me_a_df2_text}) = {me_a_f_text}, *MSe* = {me_a_mse_text}, *p* = {me_a_p_text}, ",
        "such that {dv_name} was higher for {higher_a} (*M* = {higher_a_m}, *{se_label}* = {higher_a_se}) ",
        "than for {lower_a} (*M* = {lower_a_m}, *{se_label}* = {lower_a_se})."
      )
    } else {
      me_a_text <- glue::glue(
        " There was no significant main effect for {factor_a_name}, ",
        "*F*({me_a_df1_text}, {me_a_df2_text}) = {me_a_f_text}, *MSe* = {me_a_mse_text}, *p* = {me_a_p_text}."
      )
    }

    writeup <- paste0(writeup, me_a_text)

    # Main effect B
    me_b <- factorial_results_list$MainEffect_B
    me_b_f <- me_b$F
    me_b_p <- me_b$p_value
    me_b_df1 <- me_b$df_effect %||% me_b$df_between %||% 1
    me_b_df2 <- me_b$df_error %||% me_b$df_within
    me_b_mse <- me_b$mse
    me_b_sig <- me_b_p < alpha

    me_b_f_text <- format_F(me_b_f)
    me_b_df1_text <- format_df(me_b_df1)
    me_b_df2_text <- format_df(me_b_df2)
    me_b_mse_text <- format_mse(me_b_mse)
    me_b_p_text <- format_p_value(me_b_p)

    # Get marginal means for B
    marg_b <- factorial_results_list$MarginalMeans_B
    if (!is.null(marg_b)) {
      b1_m <- format_mean(marg_b$mean[1])
      b1_se_or_sd <- if (!is.null(marg_b$se)) format_stat(marg_b$se[1]) else format_sd(marg_b$sd[1])
      b2_m <- format_mean(marg_b$mean[2])
      b2_se_or_sd <- if (!is.null(marg_b$se)) format_stat(marg_b$se[2]) else format_sd(marg_b$sd[2])
      se_label <- if (!is.null(marg_b$se)) "SE" else "SD"
    }

    if (me_b_sig) {
      higher_b <- if (marg_b$mean[1] > marg_b$mean[2]) factor_b_labels[1] else factor_b_labels[2]
      lower_b <- if (marg_b$mean[1] > marg_b$mean[2]) factor_b_labels[2] else factor_b_labels[1]
      higher_b_m <- if (marg_b$mean[1] > marg_b$mean[2]) b1_m else b2_m
      higher_b_se <- if (marg_b$mean[1] > marg_b$mean[2]) b1_se_or_sd else b2_se_or_sd
      lower_b_m <- if (marg_b$mean[1] > marg_b$mean[2]) b2_m else b1_m
      lower_b_se <- if (marg_b$mean[1] > marg_b$mean[2]) b2_se_or_sd else b1_se_or_sd

      me_b_text <- glue::glue(
        " There was a significant main effect of {factor_b_name}, ",
        "*F*({me_b_df1_text}, {me_b_df2_text}) = {me_b_f_text}, *MSe* = {me_b_mse_text}, *p* = {me_b_p_text}, ",
        "such that {dv_name} was higher for {higher_b} (*M* = {higher_b_m}, *{se_label}* = {higher_b_se}) ",
        "than for {lower_b} (*M* = {lower_b_m}, *{se_label}* = {lower_b_se})."
      )
    } else {
      me_b_text <- glue::glue(
        " There was no significant main effect for {factor_b_name}, ",
        "*F*({me_b_df1_text}, {me_b_df2_text}) = {me_b_f_text}, *MSe* = {me_b_mse_text}, *p* = {me_b_p_text}."
      )
    }

    writeup <- paste0(writeup, me_b_text)
  }

  as.character(writeup)
}


#' APA Write-Up for 2x2 Mixed Factorial ANOVA
#'
#' Generates an APA-style paragraph for a mixed design ANOVA with one
#' between-groups factor and one within-groups factor.
#'
#' @param mixed_results_list Output from mixed factorial ANOVA containing:
#'   \describe{
#'     \item{Interaction}{List with F, p_value, df_effect, df_error, mse}
#'     \item{MainEffect_Between}{List for between-groups factor}
#'     \item{MainEffect_Within}{List for within-groups factor}
#'     \item{CellDescriptives}{Data frame with between_factor, within_factor, mean, sd}
#'     \item{MarginalMeans_Between}{Data frame with level, mean, se}
#'     \item{MarginalMeans_Within}{Data frame with level, mean, sd}
#'   }
#' @param dv_name Character. Descriptive name for DV.
#' @param between_name Character. Name of between-groups factor (e.g., "gender").
#' @param within_name Character. Name of within-groups factor (e.g., "time").
#' @param between_labels Character vector of length 2. Labels for between factor.
#' @param within_labels Character vector of length 2. Labels for within factor.
#' @param hypothesis List with hypothesis details:
#'   \describe{
#'     \item{interaction_rh}{Full RH statement for interaction.}
#'     \item{interaction_pattern}{Description of expected pattern.}
#'     \item{main_between_direction}{Expected: "level1_higher" or "level2_higher"}
#'     \item{main_between_rh}{RH text for between main effect.}
#'     \item{main_within_direction}{Expected direction for within factor.}
#'   }
#' @param alpha Significance level. Default 0.05.
#'
#' @return A character string with APA write-up.
#'
#' @export
apa_2x2_mixed_factorial_writeup <- function(mixed_results_list,
                                            dv_name,
                                            between_name,
                                            within_name,
                                            between_labels,
                                            within_labels,
                                            hypothesis = NULL,
                                            alpha = 0.05) {

  # Extract interaction
  int <- mixed_results_list$Interaction
  int_f <- int$F
  int_p <- int$p_value
  int_df1 <- int$df_effect %||% 1
  int_df2 <- int$df_error
  int_mse <- int$mse
  int_sig <- int_p < alpha

  # Format interaction stats
  int_f_text <- format_F(int_f)
  int_df1_text <- format_df(int_df1)
  int_df2_text <- format_df(int_df2)
  int_mse_text <- format_mse(int_mse)
  int_p_text <- format_p_value(int_p)

  # Get cell descriptives
  cell_desc <- mixed_results_list$CellDescriptives

  # Build interaction sentence
  if (int_sig) {
    int_sentence <- glue::glue(
      "There was a significant interaction between {between_name} and {within_name}, ",
      "*F*({int_df1_text}, {int_df2_text}) = {int_f_text}, *MSe* = {int_mse_text}, *p* = {int_p_text}."
    )
  } else {
    int_sentence <- glue::glue(
      "There was not a significant interaction between {between_name} and {within_name}, ",
      "*F*({int_df1_text}, {int_df2_text}) = {int_f_text}, *MSe* = {int_mse_text}, *p* = {int_p_text}."
    )
  }

  writeup <- as.character(int_sentence)

  # Add interaction interpretation based on hypothesis
  if (!is.null(hypothesis$interaction_pattern)) {
    if (int_sig) {
      writeup <- paste0(writeup, " As hypothesized, ", hypothesis$interaction_pattern)
    } else {
      writeup <- paste0(writeup, " Contrary to hypothesis, ",
                        gsub("will be", "was not", hypothesis$interaction_pattern))
    }
  } else if (!int_sig && !is.null(hypothesis$interaction_rh)) {
    # Build generic contrary statement from cell means
    # Get cell means for description
    # This is simplified - full implementation would describe the pattern
    writeup <- paste0(writeup, " Contrary to hypothesis, the interaction pattern was not significant.")
  }

  # Main effect - Between groups
  me_b <- mixed_results_list$MainEffect_Between
  me_b_f <- me_b$F
  me_b_p <- me_b$p_value
  me_b_df1 <- me_b$df_effect %||% 1
  me_b_df2 <- me_b$df_error
  me_b_mse <- me_b$mse
  me_b_sig <- me_b_p < alpha

  me_b_f_text <- format_F(me_b_f)
  me_b_df1_text <- format_df(me_b_df1)
  me_b_df2_text <- format_df(me_b_df2)
  me_b_mse_text <- format_mse(me_b_mse)
  me_b_p_text <- format_p_value(me_b_p)

  marg_b <- mixed_results_list$MarginalMeans_Between

  if (me_b_sig) {
    b1_m <- format_mean(marg_b$mean[1])
    b1_se <- format_stat(marg_b$se[1])
    b2_m <- format_mean(marg_b$mean[2])
    b2_se <- format_stat(marg_b$se[2])

    higher_b <- if (marg_b$mean[1] > marg_b$mean[2]) between_labels[1] else between_labels[2]
    lower_b <- if (marg_b$mean[1] > marg_b$mean[2]) between_labels[2] else between_labels[1]
    higher_b_m <- if (marg_b$mean[1] > marg_b$mean[2]) b1_m else b2_m
    higher_b_se <- if (marg_b$mean[1] > marg_b$mean[2]) b1_se else b2_se
    lower_b_m <- if (marg_b$mean[1] > marg_b$mean[2]) b2_m else b1_m
    lower_b_se <- if (marg_b$mean[1] > marg_b$mean[2]) b2_se else b1_se

    # Check hypothesis support
    if (!is.null(hypothesis$main_between_direction)) {
      actual_higher <- if (marg_b$mean[1] > marg_b$mean[2]) "level1_higher" else "level2_higher"
      if (actual_higher == hypothesis$main_between_direction) {
        me_b_intro <- "Consistent with hypothesis"
      } else {
        me_b_intro <- "Contrary to hypothesis"
      }
    } else {
      me_b_intro <- "However, there was"
    }

    if (!is.null(hypothesis$main_between_rh)) {
      me_b_text <- glue::glue(
        " {me_b_intro}, there was a significant main effect for {between_name}, ",
        "*F*({me_b_df1_text}, {me_b_df2_text}) = {me_b_f_text}, *MSe* = {me_b_mse_text}, *p* = {me_b_p_text}. ",
        "{hypothesis$main_between_rh}, {higher_b} (*M* = {higher_b_m}, *SE* = {higher_b_se}) ",
        "compared to {lower_b} (*M* = {lower_b_m}, *SE* = {lower_b_se})."
      )
    } else {
      me_b_text <- glue::glue(
        " {me_b_intro} a significant main effect for {between_name}, ",
        "{higher_b} (*M* = {higher_b_m}, *SE* = {higher_b_se}) scored higher overall ",
        "than {lower_b} (*M* = {lower_b_m}, *SE* = {lower_b_se}), ",
        "*F*({me_b_df1_text}, {me_b_df2_text}) = {me_b_f_text}, *MSe* = {me_b_mse_text}, *p* = {me_b_p_text}."
      )
    }
  } else {
    me_b_text <- glue::glue(
      " There was no significant main effect for {between_name}, ",
      "*F*({me_b_df1_text}, {me_b_df2_text}) = {me_b_f_text}, *MSe* = {me_b_mse_text}, *p* = {me_b_p_text}."
    )
  }

  writeup <- paste0(writeup, me_b_text)

  # Main effect - Within groups
  me_w <- mixed_results_list$MainEffect_Within
  me_w_f <- me_w$F
  me_w_p <- me_w$p_value
  me_w_df1 <- me_w$df_effect %||% 1
  me_w_df2 <- me_w$df_error
  me_w_mse <- me_w$mse
  me_w_sig <- me_w_p < alpha

  me_w_f_text <- format_F(me_w_f)
  me_w_df1_text <- format_df(me_w_df1)
  me_w_df2_text <- format_df(me_w_df2)
  me_w_mse_text <- format_mse(me_w_mse)
  me_w_p_text <- format_p_value(me_w_p)

  marg_w <- mixed_results_list$MarginalMeans_Within

  if (me_w_sig) {
    w1_m <- format_mean(marg_w$mean[1])
    w1_sd <- format_sd(marg_w$sd[1])
    w2_m <- format_mean(marg_w$mean[2])
    w2_sd <- format_sd(marg_w$sd[2])

    higher_w <- if (marg_w$mean[1] > marg_w$mean[2]) within_labels[1] else within_labels[2]
    lower_w <- if (marg_w$mean[1] > marg_w$mean[2]) within_labels[2] else within_labels[1]
    higher_w_m <- if (marg_w$mean[1] > marg_w$mean[2]) w1_m else w2_m
    higher_w_sd <- if (marg_w$mean[1] > marg_w$mean[2]) w1_sd else w2_sd
    lower_w_m <- if (marg_w$mean[1] > marg_w$mean[2]) w2_m else w1_m
    lower_w_sd <- if (marg_w$mean[1] > marg_w$mean[2]) w2_sd else w1_sd

    me_w_text <- glue::glue(
      " There was also a significant main effect for {within_name}, such that ",
      "{dv_name} was significantly higher for {higher_w} (*M* = {higher_w_m}, *SD* = {higher_w_sd}) ",
      "than {lower_w} (*M* = {lower_w_m}, *SD* = {lower_w_sd}), ",
      "*F*({me_w_df1_text}, {me_w_df2_text}) = {me_w_f_text}, *MSe* = {me_w_mse_text}, *p* = {me_w_p_text}."
    )
  } else {
    me_w_text <- glue::glue(
      " There was no significant main effect for {within_name}, ",
      "*F*({me_w_df1_text}, {me_w_df2_text}) = {me_w_f_text}, *MSe* = {me_w_mse_text}, *p* = {me_w_p_text}."
    )
  }

  writeup <- paste0(writeup, me_w_text)

  as.character(writeup)
}


#' APA Write-Up for 2x2 Within-Groups Factorial ANOVA
#'
#' Generates an APA-style paragraph for a two-way within-groups (repeated measures)
#' ANOVA where both factors are measured within subjects.
#'
#' @param wg_factorial_results_list Output from within-groups factorial ANOVA containing:
#'   \describe{
#'     \item{Interaction}{List with F, p_value, df_effect, df_error, mse}
#'     \item{MainEffect_A}{List for factor A}
#'     \item{MainEffect_B}{List for factor B}
#'     \item{CellDescriptives}{Data frame with factor_a, factor_b, mean, sd}
#'     \item{MarginalMeans_A}{Data frame with level, mean, se}
#'     \item{MarginalMeans_B}{Data frame with level, mean, se}
#'   }
#' @param dv_name Character. Descriptive name for DV (e.g., "performance").
#' @param factor_a_name Character. Name of factor A (e.g., "task type").
#' @param factor_b_name Character. Name of factor B (e.g., "difficulty").
#' @param factor_a_labels Character vector of length 2. Labels for factor A.
#' @param factor_b_labels Character vector of length 2. Labels for factor B.
#' @param hypothesis List with hypothesis details:
#'   \describe{
#'     \item{interaction_rh}{Full RH statement for interaction.}
#'     \item{simple_effects}{List describing expected pattern at each level}
#'   }
#' @param alpha Significance level. Default 0.05.
#' @param posthoc_test Character. Name of follow-up test. Default "LSD".
#'
#' @return A character string with APA write-up.
#'
#' @examples
#' \dontrun{
#' writeup <- apa_2x2_wg_factorial_writeup(
#'   results,
#'   dv_name = "performance",
#'   factor_a_name = "task presentation",
#'   factor_b_name = "task difficulty",
#'   factor_a_labels = c("computer", "paper"),
#'   factor_b_labels = c("easy", "hard"),
#'   hypothesis = list(
#'     interaction_rh = paste0("students will perform better on ",
#'       "hard tasks on computer but better on easy tasks on paper"),
#'     simple_effects = list(
#'       list(at_a_level = "computer", direction = "b2_higher"),  # hard > easy on computer
#'       list(at_a_level = "paper", direction = "b1_higher")      # easy > hard on paper
#'     )
#'   )
#' )
#' }
#'
#' @export
apa_2x2_wg_factorial_writeup <- function(wg_factorial_results_list,
                                         dv_name,
                                         factor_a_name,
                                         factor_b_name,
                                         factor_a_labels,
                                         factor_b_labels,
                                         hypothesis = NULL,
                                         alpha = 0.05,
                                         posthoc_test = "LSD") {

  # Intro sentence
  intro <- glue::glue(
    "A 2-way within-groups ANOVA using an {posthoc_test} (*p* = .05) follow up procedure ",
    "to interpret the interaction was conducted."
  )

  # Extract interaction
  int <- wg_factorial_results_list$Interaction
  int_f <- int$F
  int_p <- int$p_value
  int_df1 <- int$df_effect %||% 1
  int_df2 <- int$df_error
  int_mse <- int$mse
  int_sig <- int_p < alpha

  int_f_text <- format_F(int_f)
  int_df1_text <- format_df(int_df1)
  int_df2_text <- format_df(int_df2)
  int_mse_text <- format_mse(int_mse)
  int_p_text <- format_p_value(int_p)

  # Build interaction sentence
  if (int_sig) {
    int_sentence <- glue::glue(
      " There was a significant interaction between {factor_a_name} and {factor_b_name} ",
      "as they relate to {dv_name}, ",
      "*F*({int_df1_text}, {int_df2_text}) = {int_f_text}, *MSe* = {int_mse_text}, *p* = {int_p_text}."
    )
  } else {
    int_sentence <- glue::glue(
      " There was not a significant interaction between {factor_a_name} and {factor_b_name} ",
      "as they relate to {dv_name}, ",
      "*F*({int_df1_text}, {int_df2_text}) = {int_f_text}, *MSe* = {int_mse_text}, *p* = {int_p_text}."
    )
  }

  writeup <- paste0(intro, int_sentence)

  # Get cell descriptives
  cell_desc <- wg_factorial_results_list$CellDescriptives

  # Create cell lookup
  cell_lookup <- list()
  for (i in seq_len(nrow(cell_desc))) {
    a_level <- as.character(cell_desc[[1]][i])
    b_level <- as.character(cell_desc[[2]][i])
    key <- paste(a_level, b_level, sep = "_")
    cell_lookup[[key]] <- list(mean = cell_desc$mean[i], sd = cell_desc$sd[i])
  }

  # Report simple effects for interaction
  if (int_sig && !is.null(hypothesis$simple_effects)) {
    se_parts <- c()

    for (se in hypothesis$simple_effects) {
      at_level <- se$at_a_level
      expected <- se$direction  # "b1_higher" or "b2_higher"

      # Get cell means
      key1 <- paste(at_level, factor_b_labels[1], sep = "_")
      key2 <- paste(at_level, factor_b_labels[2], sep = "_")

      # Try flexible matching
      if (is.null(cell_lookup[[key1]])) {
        for (k in names(cell_lookup)) {
          if (grepl(at_level, k, ignore.case = TRUE) &&
              grepl(factor_b_labels[1], k, ignore.case = TRUE)) {
            key1 <- k
            break
          }
        }
      }
      if (is.null(cell_lookup[[key2]])) {
        for (k in names(cell_lookup)) {
          if (grepl(at_level, k, ignore.case = TRUE) &&
              grepl(factor_b_labels[2], k, ignore.case = TRUE)) {
            key2 <- k
            break
          }
        }
      }

      m1 <- format_mean(cell_lookup[[key1]]$mean)
      sd1 <- format_sd(cell_lookup[[key1]]$sd)
      m2 <- format_mean(cell_lookup[[key2]]$mean)
      sd2 <- format_sd(cell_lookup[[key2]]$sd)

      # Determine actual pattern
      if (cell_lookup[[key1]]$mean > cell_lookup[[key2]]$mean) {
        higher_b <- factor_b_labels[1]
        lower_b <- factor_b_labels[2]
        higher_m <- m1
        higher_sd <- sd1
        lower_m <- m2
        lower_sd <- sd2
      } else {
        higher_b <- factor_b_labels[2]
        lower_b <- factor_b_labels[1]
        higher_m <- m2
        higher_sd <- sd2
        lower_m <- m1
        lower_sd <- sd1
      }

      se_parts <- c(se_parts, glue::glue(
        "on {at_level} they performed better on {higher_b} tasks (*M* = {higher_m}, *SD* = {higher_sd}) ",
        "than on {lower_b} tasks (*M* = {lower_m}, *SD* = {lower_sd})"
      ))
    }

    se_text <- paste(" As hypothesized, students", paste(se_parts, collapse = ", and "), ".")
    writeup <- paste0(writeup, se_text)
  }

  # Main effect A
  me_a <- wg_factorial_results_list$MainEffect_A
  me_a_f <- me_a$F
  me_a_p <- me_a$p_value
  me_a_df1 <- me_a$df_effect %||% 1
  me_a_df2 <- me_a$df_error
  me_a_mse <- me_a$mse
  me_a_sig <- me_a_p < alpha

  me_a_f_text <- format_F(me_a_f)
  me_a_df1_text <- format_df(me_a_df1)
  me_a_df2_text <- format_df(me_a_df2)
  me_a_mse_text <- format_mse(me_a_mse)
  me_a_p_text <- format_p_value(me_a_p)

  marg_a <- wg_factorial_results_list$MarginalMeans_A

  if (me_a_sig && !is.null(marg_a)) {
    a1_m <- format_mean(marg_a$mean[1])
    a1_se <- format_stat(marg_a$se[1])
    a2_m <- format_mean(marg_a$mean[2])
    a2_se <- format_stat(marg_a$se[2])

    higher_a <- if (marg_a$mean[1] > marg_a$mean[2]) factor_a_labels[1] else factor_a_labels[2]
    lower_a <- if (marg_a$mean[1] > marg_a$mean[2]) factor_a_labels[2] else factor_a_labels[1]
    higher_a_m <- if (marg_a$mean[1] > marg_a$mean[2]) a1_m else a2_m
    higher_a_se <- if (marg_a$mean[1] > marg_a$mean[2]) a1_se else a2_se
    lower_a_m <- if (marg_a$mean[1] > marg_a$mean[2]) a2_m else a1_m
    lower_a_se <- if (marg_a$mean[1] > marg_a$mean[2]) a2_se else a1_se

    me_a_text <- glue::glue(
      " There was a significant main effect for {factor_a_name}, ",
      "*F*({me_a_df1_text}, {me_a_df2_text}) = {me_a_f_text}, *MSe* = {me_a_mse_text}, *p* = {me_a_p_text}, ",
      "such that {higher_a} tasks (*M* = {higher_a_m}, *SE* = {higher_a_se}) were performed better ",
      "than {lower_a} tasks overall (*M* = {lower_a_m}, *SE* = {lower_a_se})."
    )
  } else {
    me_a_text <- glue::glue(
      " There was no significant main effect for {factor_a_name}, ",
      "*F*({me_a_df1_text}, {me_a_df2_text}) = {me_a_f_text}, *MSe* = {me_a_mse_text}, *p* = {me_a_p_text}."
    )
  }

  writeup <- paste0(writeup, me_a_text)

  # Main effect B
  me_b <- wg_factorial_results_list$MainEffect_B
  me_b_f <- me_b$F
  me_b_p <- me_b$p_value
  me_b_df1 <- me_b$df_effect %||% 1
  me_b_df2 <- me_b$df_error
  me_b_mse <- me_b$mse
  me_b_sig <- me_b_p < alpha

  me_b_f_text <- format_F(me_b_f)
  me_b_df1_text <- format_df(me_b_df1)
  me_b_df2_text <- format_df(me_b_df2)
  me_b_mse_text <- format_mse(me_b_mse)
  me_b_p_text <- format_p_value(me_b_p)

  marg_b <- wg_factorial_results_list$MarginalMeans_B

  if (me_b_sig && !is.null(marg_b)) {
    b1_m <- format_mean(marg_b$mean[1])
    b1_se <- format_stat(marg_b$se[1])
    b2_m <- format_mean(marg_b$mean[2])
    b2_se <- format_stat(marg_b$se[2])

    higher_b <- if (marg_b$mean[1] > marg_b$mean[2]) factor_b_labels[1] else factor_b_labels[2]
    lower_b <- if (marg_b$mean[1] > marg_b$mean[2]) factor_b_labels[2] else factor_b_labels[1]
    higher_b_m <- if (marg_b$mean[1] > marg_b$mean[2]) b1_m else b2_m
    higher_b_se <- if (marg_b$mean[1] > marg_b$mean[2]) b1_se else b2_se
    lower_b_m <- if (marg_b$mean[1] > marg_b$mean[2]) b2_m else b1_m
    lower_b_se <- if (marg_b$mean[1] > marg_b$mean[2]) b2_se else b1_se

    me_b_text <- glue::glue(
      " There was also a significant main effect for {factor_b_name}, ",
      "*F*({me_b_df1_text}, {me_b_df2_text}) = {me_b_f_text}, *MSe* = {me_b_mse_text}, *p* = {me_b_p_text}, ",
      "such that {higher_b} tasks (*M* = {higher_b_m}, *SE* = {higher_b_se}) were performed better ",
      "than {lower_b} tasks overall (*M* = {lower_b_m}, *SE* = {lower_b_se})."
    )
  } else {
    me_b_text <- glue::glue(
      " There was no significant main effect for {factor_b_name}, ",
      "*F*({me_b_df1_text}, {me_b_df2_text}) = {me_b_f_text}, *MSe* = {me_b_mse_text}, *p* = {me_b_p_text}."
    )
  }

  writeup <- paste0(writeup, me_b_text)

  as.character(writeup)
}


# =============================================================================
# NULL COALESCING OPERATOR
# =============================================================================

#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
