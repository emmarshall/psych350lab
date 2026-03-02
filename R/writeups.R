#' APA Write-Up for Pearson Correlation
#'
#' Generates an APA-style paragraph reporting a Pearson correlation result.
#'
#' @param corr_results_list Output from [corr_answers()].
#' @param var1_name Character string. Display name for variable 1.
#' @param var2_name Character string. Display name for variable 2.
#' @param include_descriptives Logical. Include M and SD in text. Default `TRUE`.
#' @param table_number Integer or `NULL`. Table number to reference if
#'   `include_descriptives = FALSE`.
#'
#' @return A character string with the APA write-up (uses markdown italics).
#'
#' @examples
#' data(superman)
#' result <- corr_answers(superman, "clark_height_in", "rt_critics_score")
#' writeup <- apa_corr_writeup(result, "Clark Height", "Critics Score")
#' cat(writeup)
#'
#' @export
apa_corr_writeup <- function(corr_results_list,
                             var1_name,
                             var2_name,
                             include_descriptives = TRUE,
                             table_number = NULL) {

  r <- corr_results_list$Correlation$r
  p <- corr_results_list$Correlation$p_value
  df <- corr_results_list$Correlation$df

  desc_stats <- corr_results_list$Descriptives
  var1_stats <- desc_stats[desc_stats$variable == var1_name, ]
  var2_stats <- desc_stats[desc_stats$variable == var2_name, ]

  # If labels don't match variable names, try matching by position

  if (nrow(var1_stats) == 0) var1_stats <- desc_stats[1, ]
  if (nrow(var2_stats) == 0) var2_stats <- desc_stats[2, ]

  # Format p-value for APA
  if (p < 0.001) {
    p_text <- "< .001"
  } else {
    p_text <- paste0("= ", sub("^0", "", sprintf("%.3f", p)))
  }

  # Format r-value (remove leading zero)
  r_text <- sub("^0", "", sprintf("%.2f", r))
  if (r < 0) {
    r_text <- sub("^-0", "-", sprintf("%.2f", r))
  }

  significance <- if (p < 0.05) "significant" else "non-significant"

  if (include_descriptives) {
    writeup <- glue::glue(
      "A Pearson correlation was conducted to examine the relationship between ",
      "{var1_name} (*M* = {var1_stats$mean}, *SD* = {var1_stats$sd}) and ",
      "{var2_name} (*M* = {var2_stats$mean}, *SD* = {var2_stats$sd}). ",
      "The results indicated a {significance} correlation, ",
      "*r*({df}) = {r_text}, *p* {p_text}."
    )
  } else {
    if (is.null(table_number)) {
      table_ref <- "see Table for descriptive statistics"
    } else {
      table_ref <- glue::glue("see Table {table_number} for descriptive statistics")
    }

    writeup <- glue::glue(
      "A Pearson correlation was conducted to examine the relationship between ",
      "{var1_name} and {var2_name} ({table_ref}). ",
      "The results indicated a {significance} correlation, ",
      "*r*({df}) = {r_text}, *p* {p_text}."
    )
  }

  return(as.character(writeup))
}


#' APA Write-Up for Chi-Square Test
#'
#' Generates an APA-style paragraph reporting a chi-square test of independence.
#'
#' @param chi_results_list Output from [chi_square_answers()].
#' @param var1_name Character string. Name of variable 1.
#' @param var2_name Character string. Name of variable 2.
#' @param var1_labels Character vector of length 2. Labels for var1 levels.
#' @param var2_labels Character vector of length 2. Labels for var2 levels.
#' @param hypothesis_text Character string or `NULL`. Custom hypothesis text.
#' @param include_descriptives Logical. Include counts/percentages. Default `TRUE`.
#' @param table_number Integer or `NULL`. Table reference number.
#'
#' @return A character string with APA write-up.
#'
#' @examples
#' data(superman)
#' result <- chi_square_answers(superman, "clark_grp", "tomatometer")
#' writeup <- apa_chi_writeup(result,
#'   var1_name = "Height Group", var2_name = "Tomatometer",
#'   var1_labels = c("Under 6ft", "6ft+"),
#'   var2_labels = c("Rotten", "Fresh")
#' )
#' cat(writeup)
#'
#' @export
apa_chi_writeup <- function(chi_results_list,
                            var1_name,
                            var2_name,
                            var1_labels = c("Level 1", "Level 2"),
                            var2_labels = c("Level A", "Level B"),
                            hypothesis_text = NULL,
                            include_descriptives = TRUE,
                            table_number = NULL) {

  chi_sq <- chi_results_list$ChiSquare$chi_sq
  p <- chi_results_list$ChiSquare$p_value
  df <- chi_results_list$ChiSquare$df

  cont_table <- chi_results_list$ContingencyTable
  var1_desc <- chi_results_list$Var1_Descriptives
  var2_desc <- chi_results_list$Var2_Descriptives

  var1_level1_total <- var1_desc$n[1]
  var1_level2_total <- var1_desc$n[2]

  var1_lev1_var2_lev1 <- cont_table[1, 1]
  var1_lev1_var2_lev2 <- cont_table[1, 2]
  var1_lev2_var2_lev1 <- cont_table[2, 1]
  var1_lev2_var2_lev2 <- cont_table[2, 2]

  pct_var1_lev1_var2_lev1 <- round((var1_lev1_var2_lev1 / var1_level1_total) * 100, 2)
  pct_var1_lev2_var2_lev1 <- round((var1_lev2_var2_lev1 / var1_level2_total) * 100, 2)

  # Format p-value
  if (p < 0.001) {
    p_text <- "< .001"
  } else {
    p_text <- paste0("= ", sub("^0", "", sprintf("%.3f", p)))
  }

  # Default hypothesis text
  if (is.null(hypothesis_text)) {
    hypothesis_text <- glue::glue(
      "We hypothesized that {var1_labels[1]} would be equally likely to be ",
      "{var2_labels[1]} or {var2_labels[2]}, whereas {var1_labels[2]} would show ",
      "a different pattern."
    )
  }

  if (include_descriptives) {
    desc_text_sig <- glue::glue(
      "Consistent with our hypothesis, participants from {var1_labels[1]} ",
      "(*n* = {var1_level1_total}; {pct_var1_lev1_var2_lev1}%) and {var1_labels[2]} ",
      "(*n* = {var1_level2_total}; {pct_var1_lev2_var2_lev1}%) showed different patterns ",
      "of {var2_name}."
    )

    desc_text_nonsig <- glue::glue(
      "Contrary to our hypothesis, participants from both {var1_labels[1]} ",
      "(*n* = {var1_level1_total}; {pct_var1_lev1_var2_lev1}%) and {var1_labels[2]} ",
      "(*n* = {var1_level2_total}; {pct_var1_lev2_var2_lev1}%) were equally likely to be ",
      "{var2_labels[1]}."
    )
  } else {
    if (is.null(table_number)) {
      table_ref <- "see Table"
    } else {
      table_ref <- glue::glue("see Table {table_number}")
    }

    desc_text_sig <- glue::glue(
      "Consistent with our hypothesis, participants from {var1_labels[1]} and {var1_labels[2]} ",
      "showed different patterns of {var2_name} ({table_ref})."
    )

    desc_text_nonsig <- glue::glue(
      "Contrary to our hypothesis, participants from both {var1_labels[1]} and {var1_labels[2]} ",
      "were equally likely to be {var2_labels[1]} ({table_ref})."
    )
  }

  if (p < 0.05) {
    writeup <- glue::glue(
      "{hypothesis_text} A chi-square test found a significant relationship between ",
      "{var1_name} and {var2_name}, \u03C7\u00B2({df}) = {chi_sq}, *p* {p_text}. ",
      "{desc_text_sig}"
    )
  } else {
    writeup <- glue::glue(
      "{hypothesis_text} A chi-square test found no significant relationship between ",
      "{var1_name} and {var2_name}, \u03C7\u00B2({df}) = {chi_sq}, *p* {p_text}. ",
      "{desc_text_nonsig}"
    )
  }

  return(as.character(writeup))
}


#' APA Write-Up for Between-Groups ANOVA
#'
#' Generates an APA-style paragraph for a between-groups one-way ANOVA.
#'
#' @param anova_results_list Output from [bg_anova_answers()].
#' @param iv_name Character string. Display name for the IV.
#' @param dv_name Character string. Display name for the DV.
#' @param iv_labels Character vector or `NULL`. Labels for IV levels.
#' @param hypothesis_text Character string or `NULL`. Custom hypothesis text.
#' @param include_descriptives Logical. Include M and SD. Default `TRUE`.
#'
#' @return A character string with APA write-up.
#'
#' @examples
#' data(superman)
#' result <- bg_anova_answers(superman, iv = "clark_grp", dv = "rt_critics_score")
#' writeup <- apa_bg_anova_writeup(result,
#'   iv_name = "Height Group", dv_name = "Critics Score",
#'   iv_labels = c("Under 6ft", "6ft+")
#' )
#' cat(writeup)
#'
#' @export
apa_bg_anova_writeup <- function(anova_results_list,
                                 iv_name,
                                 dv_name,
                                 iv_labels = NULL,
                                 hypothesis_text = NULL,
                                 include_descriptives = TRUE) {

  f_val <- anova_results_list$ANOVA$F
  p_val <- anova_results_list$ANOVA$p_value
  df_between <- anova_results_list$ANOVA$df_between
  df_within <- anova_results_list$ANOVA$df_within
  mse <- anova_results_list$ANOVA$mse

  desc_stats <- anova_results_list$Descriptives

  if (is.null(iv_labels)) {
    iv_labels <- as.character(desc_stats$iv)
  }

  group1_mean <- desc_stats$mean[1]
  group1_sd <- desc_stats$sd[1]
  group1_n <- desc_stats$n[1]
  group2_mean <- desc_stats$mean[2]
  group2_sd <- desc_stats$sd[2]
  group2_n <- desc_stats$n[2]

  # Format p-value
  if (p_val < 0.001) {
    p_text <- "< .001"
  } else {
    p_text <- sprintf("= %.3f", p_val)
  }

  # Build writeup
  if (include_descriptives) {
    desc_part <- glue::glue(
      "Those with {iv_labels[1]} displayed a {dv_name} of {group1_mean} (*SD* = {group1_sd}), ",
      "whereas those with {iv_labels[2]} displayed a {dv_name} of {group2_mean} (*SD* = {group2_sd}). "
    )
  } else {
    desc_part <- ""
  }

  if (p_val < 0.05) {
    if (is.null(hypothesis_text)) {
      hypothesis_text <- "As hypothesized"
    }

    if (group1_mean > group2_mean) {
      direction_text <- glue::glue("{iv_labels[1]} had significantly higher {dv_name} than {iv_labels[2]}")
    } else {
      direction_text <- glue::glue("{iv_labels[2]} had significantly higher {dv_name} than {iv_labels[1]}")
    }

    writeup <- glue::glue(
      "{desc_part}{hypothesis_text}, {direction_text}, ",
      "*F*({df_between}, {df_within}) = {f_val}, *p* {p_text}, *MSE* = {mse}."
    )
  } else {
    if (is.null(hypothesis_text)) {
      hypothesis_text <- "Contrary to the hypothesis"
    }

    writeup <- glue::glue(
      "{desc_part}{hypothesis_text}, there was no significant difference in {dv_name} ",
      "between {iv_labels[1]} and {iv_labels[2]}, ",
      "*F*({df_between}, {df_within}) = {f_val}, *p* {p_text}, *MSE* = {mse}."
    )
  }

  return(as.character(writeup))
}


#' APA Write-Up for Within-Groups ANOVA
#'
#' Generates an APA-style paragraph for a repeated measures ANOVA.
#'
#' @param anova_results_list Output from [wg_anova_answers()].
#' @param dv_name Character string. Display name for the DV.
#' @param condition_labels Character vector or `NULL`. Labels for conditions.
#' @param hypothesis_text Character string or `NULL`. Custom hypothesis text.
#' @param include_descriptives Logical. Default `TRUE`.
#'
#' @return A character string with APA write-up.
#'
#' @examples
#' data(superman)
#' result <- wg_anova_answers(superman,
#'   dv1 = "rt_critics_score",
#'   dv2 = "rt_audience_score"
#' )
#' writeup <- apa_wg_anova_writeup(result,
#'   dv_name = "Rating",
#'   condition_labels = c("Critics Score", "Audience Score")
#' )
#' cat(writeup)
#'
#' @export
apa_wg_anova_writeup <- function(anova_results_list,
                                 dv_name,
                                 condition_labels = NULL,
                                 hypothesis_text = NULL,
                                 include_descriptives = TRUE) {

  f_val <- anova_results_list$ANOVA$F
  p_val <- anova_results_list$ANOVA$p_value
  df_effect <- anova_results_list$ANOVA$df_effect
  df_error <- anova_results_list$ANOVA$df_error
  mse <- anova_results_list$ANOVA$mse

  desc_stats <- anova_results_list$Descriptives
  n <- anova_results_list$Sample_Size

  if (is.null(condition_labels)) {
    condition_labels <- as.character(desc_stats$condition)
  }

  cond1_mean <- desc_stats$mean[1]
  cond1_sd <- desc_stats$sd[1]
  cond2_mean <- desc_stats$mean[2]
  cond2_sd <- desc_stats$sd[2]

  # Format p-value
  if (p_val < 0.001) {
    p_text <- "< .001"
  } else {
    p_text <- sprintf("= %.3f", p_val)
  }

  if (p_val < 0.05) {
    if (is.null(hypothesis_text)) {
      hypothesis_text <- "As hypothesized"
    }

    if (cond1_mean > cond2_mean) {
      comparison <- glue::glue(
        "{dv_name} were significantly higher in {condition_labels[1]} ",
        "(*M* = {cond1_mean}, *SD* = {cond1_sd}) than {condition_labels[2]} ",
        "(*M* = {cond2_mean}, *SD* = {cond2_sd})"
      )
    } else {
      comparison <- glue::glue(
        "{dv_name} were significantly lower in {condition_labels[1]} ",
        "(*M* = {cond1_mean}, *SD* = {cond1_sd}) than {condition_labels[2]} ",
        "(*M* = {cond2_mean}, *SD* = {cond2_sd})"
      )
    }

    writeup <- glue::glue(
      "{hypothesis_text}, {comparison}, ",
      "*F*({df_effect}, {df_error}) = {f_val}, *p* {p_text}, *MSE* = {mse}."
    )
  } else {
    if (is.null(hypothesis_text)) {
      hypothesis_text <- "Contrary to the hypothesis"
    }

    writeup <- glue::glue(
      "{hypothesis_text}, there was no significant difference in {dv_name} ",
      "between {condition_labels[1]} (*M* = {cond1_mean}, *SD* = {cond1_sd}) and ",
      "{condition_labels[2]} (*M* = {cond2_mean}, *SD* = {cond2_sd}), ",
      "*F*({df_effect}, {df_error}) = {f_val}, *p* {p_text}, *MSE* = {mse}."
    )
  }

  return(as.character(writeup))
}


#' APA Write-Up for Factorial ANOVA
#'
#' Generates a complete APA-style results section for a factorial ANOVA,
#' including interaction and main effects.
#'
#' @param anova_results_list Output from [anova_factorial_answers()].
#' @param dv_name Character string. Display name for DV.
#' @param iv1_name Character string. Display name for IV1.
#' @param iv2_name Character string. Display name for IV2.
#' @param include_interaction Logical. Report interaction. Default `TRUE`.
#' @param include_main_effect_iv1 Logical. Report main effect of IV1. Default `TRUE`.
#' @param include_main_effect_iv2 Logical. Report main effect of IV2. Default `TRUE`.
#' @param hypothesis_interaction Character or `NULL`. Custom hypothesis for interaction.
#' @param hypothesis_iv1 Character or `NULL`. Custom hypothesis for IV1.
#' @param hypothesis_iv2 Character or `NULL`. Custom hypothesis for IV2.
#' @param describe_pattern Logical. Describe pattern of results. Default `TRUE`.
#' @param report_mse Logical. Include MSE. Default `TRUE`.
#'
#' @return A character string with APA write-up.
#'
#' @examples
#' \dontrun{
#' data(superman)
#' sm <- superman
#' sm$era <- ifelse(sm$year >= 2000, 2, 1)
#' result <- anova_factorial_answers(sm, dv = "clark_height_in",
#'   iv1 = "clark_grp", iv2 = "era",
#'   iv1_labels = c("Under 6ft", "6ft+"),
#'   iv2_labels = c("Pre-2000", "Post-2000"))
#' writeup <- apa_factorial_writeup(result,
#'   dv_name = "Height", iv1_name = "Height Group", iv2_name = "Era")
#' cat(writeup)
#' }
#'
#' @export
apa_factorial_writeup <- function(anova_results_list,
                                  dv_name,
                                  iv1_name,
                                  iv2_name,
                                  include_interaction = TRUE,
                                  include_main_effect_iv1 = TRUE,
                                  include_main_effect_iv2 = TRUE,
                                  hypothesis_interaction = NULL,
                                  hypothesis_iv1 = NULL,
                                  hypothesis_iv2 = NULL,
                                  describe_pattern = TRUE,
                                  report_mse = TRUE) {

  writeup <- ""

  # INTERACTION
  if (include_interaction) {
    interaction_text <- .apa_interaction_detailed(
      anova_results_list = anova_results_list,
      dv_name = dv_name,
      iv1_name = iv1_name,
      iv2_name = iv2_name,
      hypothesis_text = hypothesis_interaction,
      describe_pattern = describe_pattern,
      report_mse = report_mse
    )
    writeup <- paste0(writeup, interaction_text)
  }

  # MAIN EFFECT IV1
  if (include_main_effect_iv1) {
    if (writeup != "") writeup <- paste0(writeup, " ")

    me_iv1_text <- .apa_main_effect_detailed(
      anova_results_list = anova_results_list,
      dv_name = dv_name,
      iv_name = iv1_name,
      which_iv = "IV1",
      hypothesis_text = hypothesis_iv1,
      describe_pattern = describe_pattern,
      report_mse = report_mse,
      check_interaction = include_interaction
    )
    writeup <- paste0(writeup, me_iv1_text)
  }

  # MAIN EFFECT IV2
  if (include_main_effect_iv2) {
    if (writeup != "") writeup <- paste0(writeup, " ")

    me_iv2_text <- .apa_main_effect_detailed(
      anova_results_list = anova_results_list,
      dv_name = dv_name,
      iv_name = iv2_name,
      which_iv = "IV2",
      hypothesis_text = hypothesis_iv2,
      describe_pattern = describe_pattern,
      report_mse = report_mse,
      check_interaction = include_interaction
    )
    writeup <- paste0(writeup, me_iv2_text)
  }

  return(writeup)
}


# =========================================================================
# Internal helper: Detailed interaction write-up
# =========================================================================
#' @noRd
.apa_interaction_detailed <- function(anova_results_list,
                                      dv_name,
                                      iv1_name,
                                      iv2_name,
                                      hypothesis_text = NULL,
                                      describe_pattern = TRUE,
                                      report_mse = TRUE) {

  f_int <- anova_results_list$ANOVA$Interaction$F
  p_int <- anova_results_list$ANOVA$Interaction$p_value
  df_int <- anova_results_list$ANOVA$Interaction$df
  df_within <- anova_results_list$ANOVA$df_within
  mse <- anova_results_list$ANOVA$mse
  lsd_mmd <- anova_results_list$LSD$lsd_mmd

  desc_stats <- anova_results_list$Descriptives

  p_text <- if (p_int < 0.001) "< .001" else paste0("= ", sub("^0", "", sprintf("%.3f", p_int)))

  writeup <- ""

  if (!is.null(hypothesis_text)) {
    writeup <- paste0(hypothesis_text, " ")
  }

  if (p_int < 0.05) {
    writeup <- paste0(writeup, "There was a significant interaction between ",
                      iv1_name, " and ", iv2_name, " as they relate to ", dv_name, ", ")
  } else {
    writeup <- paste0(writeup, "There was no significant interaction between ",
                      iv1_name, " and ", iv2_name, " as they relate to ", dv_name, ", ")
  }

  writeup <- paste0(writeup, "*F*(", df_int, ", ", df_within, ") = ",
                    sprintf("%.3f", f_int))

  if (report_mse) {
    writeup <- paste0(writeup, ", *MSe* = ", sprintf("%.3f", mse))
  }

  writeup <- paste0(writeup, ", *p* ", p_text, ".")

  if (describe_pattern && p_int < 0.05) {
    pattern_text <- .describe_interaction_pattern(
      desc_stats = desc_stats,
      lsd_mmd = lsd_mmd,
      iv1_name = iv1_name,
      iv2_name = iv2_name,
      dv_name = dv_name
    )
    writeup <- paste0(writeup, " ", pattern_text)
  }

  return(writeup)
}


# =========================================================================
# Internal helper: Detailed main effect write-up
# =========================================================================
#' @noRd
.apa_main_effect_detailed <- function(anova_results_list,
                                      dv_name,
                                      iv_name,
                                      which_iv = "IV1",
                                      hypothesis_text = NULL,
                                      describe_pattern = TRUE,
                                      report_mse = TRUE,
                                      check_interaction = TRUE) {

  if (which_iv == "IV1") {
    f_stat <- anova_results_list$ANOVA$MainEffect_IV1$F
    p_val <- anova_results_list$ANOVA$MainEffect_IV1$p_value
    df_between <- anova_results_list$ANOVA$MainEffect_IV1$df
    emm_data <- anova_results_list$EMMs$IV1
  } else {
    f_stat <- anova_results_list$ANOVA$MainEffect_IV2$F
    p_val <- anova_results_list$ANOVA$MainEffect_IV2$p_value
    df_between <- anova_results_list$ANOVA$MainEffect_IV2$df
    emm_data <- anova_results_list$EMMs$IV2
  }

  df_within <- anova_results_list$ANOVA$df_within
  mse <- anova_results_list$ANOVA$mse
  p_int <- anova_results_list$ANOVA$Interaction$p_value

  p_text <- if (p_val < 0.001) "< .001" else paste0("= ", sub("^0", "", sprintf("%.3f", p_val)))

  writeup <- ""

  if (!is.null(hypothesis_text)) {
    writeup <- paste0(hypothesis_text, " ")
  }

  if (p_val < 0.05) {
    writeup <- paste0(writeup, "There was a significant main effect of ", iv_name, ", ")
  } else {
    writeup <- paste0(writeup, "There was no significant main effect of ", iv_name, ", ")
  }

  writeup <- paste0(writeup, "*F*(", df_between, ", ", df_within, ") = ",
                    sprintf("%.3f", f_stat))

  if (report_mse) {
    writeup <- paste0(writeup, ", *MSe* = ", sprintf("%.3f", mse))
  }

  writeup <- paste0(writeup, ", *p* ", p_text)

  if (describe_pattern && p_val < 0.05) {
    pattern_text <- .describe_main_effect_pattern(
      emm_data = emm_data,
      iv_name = iv_name,
      dv_name = dv_name,
      which_iv = which_iv,
      anova_results_list = anova_results_list
    )
    writeup <- paste0(writeup, pattern_text)
  }

  writeup <- paste0(writeup, ".")

  if (check_interaction && p_int < 0.05 && p_val < 0.05) {
    writeup <- paste0(writeup,
                      " However, this main effect should be interpreted with caution due to the significant interaction.")
  }

  return(writeup)
}


# =========================================================================
# Internal helper: Describe interaction pattern
# =========================================================================
#' @noRd
.describe_interaction_pattern <- function(desc_stats, lsd_mmd,
                                          iv1_name, iv2_name, dv_name) {

  iv1_levels <- unique(desc_stats$iv1_label)
  iv2_levels <- unique(desc_stats$iv2_label)

  pattern_text <- ""

  for (i in seq_along(iv1_levels)) {
    iv1_level <- iv1_levels[i]

    cell1 <- desc_stats[desc_stats$iv1_label == iv1_level &
                          desc_stats$iv2_label == iv2_levels[1], ]
    cell2 <- desc_stats[desc_stats$iv1_label == iv1_level &
                          desc_stats$iv2_label == iv2_levels[2], ]

    if (nrow(cell1) > 0 && nrow(cell2) > 0) {
      mean1 <- cell1$mean[1]
      sd1 <- cell1$sd[1]
      mean2 <- cell2$mean[1]
      sd2 <- cell2$sd[1]

      mean_diff <- abs(mean1 - mean2)
      is_significant <- mean_diff > lsd_mmd

      if (is_significant) {
        if (mean1 > mean2) {
          pattern_text <- paste0(pattern_text,
                                 "For ", iv1_level, " ", tolower(iv1_name), ", ",
                                 dv_name, " was significantly higher for ",
                                 iv2_levels[1], " (*M* = ", sprintf("%.2f", mean1),
                                 ", *SD* = ", sprintf("%.2f", sd1), ") than for ",
                                 iv2_levels[2], " (*M* = ", sprintf("%.2f", mean2),
                                 ", *SD* = ", sprintf("%.2f", sd2), "). ")
        } else {
          pattern_text <- paste0(pattern_text,
                                 "For ", iv1_level, " ", tolower(iv1_name), ", ",
                                 dv_name, " was significantly higher for ",
                                 iv2_levels[2], " (*M* = ", sprintf("%.2f", mean2),
                                 ", *SD* = ", sprintf("%.2f", sd2), ") than for ",
                                 iv2_levels[1], " (*M* = ", sprintf("%.2f", mean1),
                                 ", *SD* = ", sprintf("%.2f", sd1), "). ")
        }
      } else {
        pattern_text <- paste0(pattern_text,
                               "For ", iv1_level, " ", tolower(iv1_name),
                               ", there was no significant difference in ",
                               dv_name, " between ", iv2_levels[1],
                               " (*M* = ", sprintf("%.2f", mean1),
                               ", *SD* = ", sprintf("%.2f", sd1), ") and ",
                               iv2_levels[2], " (*M* = ", sprintf("%.2f", mean2),
                               ", *SD* = ", sprintf("%.2f", sd2), "). ")
      }
    }
  }

  return(pattern_text)
}


# =========================================================================
# Internal helper: Describe main effect pattern
# =========================================================================
#' @noRd
.describe_main_effect_pattern <- function(emm_data, iv_name, dv_name,
                                          which_iv, anova_results_list) {

  desc_stats <- anova_results_list$Descriptives

  if (which_iv == "IV1") {
    label1 <- emm_data$iv1_label[1]
    label2 <- emm_data$iv1_label[2]
    level1_cells <- desc_stats[desc_stats$iv1_label == label1, ]
    level2_cells <- desc_stats[desc_stats$iv1_label == label2, ]
  } else {
    label1 <- emm_data$iv2_label[1]
    label2 <- emm_data$iv2_label[2]
    level1_cells <- desc_stats[desc_stats$iv2_label == label1, ]
    level2_cells <- desc_stats[desc_stats$iv2_label == label2, ]
  }

  sd1 <- sqrt(sum(level1_cells$sd^2 * level1_cells$n) / sum(level1_cells$n))
  sd2 <- sqrt(sum(level2_cells$sd^2 * level2_cells$n) / sum(level2_cells$n))

  mean1 <- emm_data$mean[1]
  mean2 <- emm_data$mean[2]

  if (mean1 > mean2) {
    pattern_text <- paste0(", such that ", dv_name, " was higher for ",
                           label1, " (*M* = ", sprintf("%.2f", mean1),
                           ", *SD* = ", sprintf("%.2f", sd1), ") than for ",
                           label2, " (*M* = ", sprintf("%.2f", mean2),
                           ", *SD* = ", sprintf("%.2f", sd2), ")")
  } else {
    pattern_text <- paste0(", such that ", dv_name, " was higher for ",
                           label2, " (*M* = ", sprintf("%.2f", mean2),
                           ", *SD* = ", sprintf("%.2f", sd2), ") than for ",
                           label1, " (*M* = ", sprintf("%.2f", mean1),
                           ", *SD* = ", sprintf("%.2f", sd1), ")")
  }

  return(pattern_text)
}


#' APA Write-Up for Multiple Regression
#'
#' Generates an APA-style paragraph for a multiple regression analysis.
#'
#' @param reg_results_list Output from \code{linear_reg_answers()}.
#' @param include_correlations Logical. Include bivariate info. Default `TRUE`.
#' @param include_coefficients Logical. Include individual predictors. Default `TRUE`.
#'
#' @return A character string with APA write-up.
#'
#' @examples
#' data(superman)
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- linear_reg_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Clark Height (in)", "Audience Score"),
#'   criterion_label = "Critics Score"
#' )
#' writeup <- apa_regression_writeup(result)
#' cat(writeup)
#'
#' @export
apa_regression_writeup <- function(reg_results_list,
                                   include_correlations = TRUE,
                                   include_coefficients = TRUE) {

  r <- reg_results_list$Model$R
  r_sq <- reg_results_list$Model$R_squared
  adj_r_sq <- reg_results_list$Model$Adj_R_squared
  f_stat <- reg_results_list$Model$F
  df1 <- reg_results_list$Model$df1
  df2 <- reg_results_list$Model$df2
  p_val <- reg_results_list$Model$p_value
  n <- reg_results_list$Model$n
  criterion_label <- reg_results_list$Labels$criterion_label
  predictor_labels <- reg_results_list$Labels$predictor_labels
  predictors <- reg_results_list$Labels$predictors

  format_p <- function(p) {
    if (p < 0.001) "< .001"
    else paste0("= ", sub("^0", "", sprintf("%.3f", p)))
  }

  writeup <- paste0(
    "A multiple regression analysis was conducted to predict ", criterion_label,
    " from ", paste(predictor_labels, collapse = ", "), " (*N* = ", n, "). "
  )

  if (p_val < 0.05) {
    writeup <- paste0(writeup,
                      "The overall model was statistically significant, ",
                      "*R*\u00B2 = ", sprintf("%.3f", r_sq),
                      ", *F*(", df1, ", ", df2, ") = ", sprintf("%.2f", f_stat),
                      ", *p* ", format_p(p_val),
                      ", explaining ", round(r_sq * 100, 1), "% of the variance in ",
                      criterion_label, ". ")
  } else {
    writeup <- paste0(writeup,
                      "The overall model was not statistically significant, ",
                      "*R*\u00B2 = ", sprintf("%.3f", r_sq),
                      ", *F*(", df1, ", ", df2, ") = ", sprintf("%.2f", f_stat),
                      ", *p* ", format_p(p_val), ". ")
  }

  if (include_coefficients) {
    sig_predictors <- c()
    nonsig_predictors <- c()

    for (i in seq_along(predictors)) {
      p <- predictors[i]
      regwt <- reg_results_list$Regression_Weights[[p]]

      if (regwt$significant) {
        sig_predictors <- c(sig_predictors,
                            paste0(predictor_labels[i],
                                   " (*b* = ", sprintf("%.3f", regwt$b),
                                   ", *p* ", format_p(regwt$p_value), ")"))
      } else {
        nonsig_predictors <- c(nonsig_predictors, predictor_labels[i])
      }
    }

    if (length(sig_predictors) > 0) {
      writeup <- paste0(writeup,
                        "Significant predictors included: ",
                        paste(sig_predictors, collapse = "; "), ". ")
    }

    if (length(nonsig_predictors) > 0) {
      writeup <- paste0(writeup,
                        paste(nonsig_predictors, collapse = ", "),
                        ifelse(length(nonsig_predictors) == 1,
                               " was not a significant predictor.",
                               " were not significant predictors."))
    }
  }

  return(writeup)
}


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
        as.character(desc_stats$mean[i]),
        as.character(desc_stats$sd[i])
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
    posthoc_answer <- paste0("Yes ", .en_dash(), " we know there's a mean difference, but we don't know which groups are different from which others.")
  } else {
    posthoc_answer <- paste0("No ", .en_dash(), " a nonsignificant Omnibus F-test")
  }

  if (KEY) {
    anova_text <- paste0(
      "F = ", hl(f_stat), "    df = ", hl(df_between), " , ", hl(df_within),
      "    MSE = ", hl(mse), "    p = ", hl(p_value),
      "    N = ", hl(total_n), "    k = ", hl(k), "    n = ", hl(mean_n), "\n\n",
      "Do we need to perform LSD pairwise comparisons to test the RH? Why or why not? ",
      hl(posthoc_answer)
    )

    if (has_lsd) {
      anova_text <- paste0(anova_text, "\n\nLSDmmd = ", hl(lsd_mmd),
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




#' LSD Pairwise Comparisons Table (Answer KEY)
#'
#' Creates a flextable showing LSD pairwise comparison results with
#' comparisons as columns.
#'
#' @param anova_results_list Output from [anova_kgroup_answers()].
#' @param KEY Logical. If TRUE (default), fill with values; if FALSE, blank.
#' @param group_labels Character vector or NULL. Display labels for groups.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' \dontrun{
#' result <- anova_kgroup_answers(data, "dv", "iv")
#' lsd_pairwise_KEY(result, KEY = TRUE)
#' }
#'
#' @export
lsd_pairwise_KEY <- function(anova_results_list, KEY = TRUE, group_labels = NULL) {
  pairwise <- anova_results_list$Pairwise
  n_pairwise <- length(pairwise)

  if (KEY) {
    pairwise_data <- data.frame(
      ` ` = c(
        "Pairwise comparison \u2192",
        "Mean difference \u2192",
        "LSD result \u2192",
        "Type of Stat Error risked \u2192",
        "Pairwise effect size (r) \u2192",
        "Power Problem? \u2192"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    for (i in 1:n_pairwise) {
      comparison_name <- pairwise[[i]]$comparison
      if (!is.null(group_labels)) {
        original_labels <- anova_results_list$group_labels
        if (!is.null(original_labels)) {
          for (j in seq_along(original_labels)) {
            comparison_name <- gsub(original_labels[j], group_labels[j],
                                    comparison_name, fixed = TRUE)
          }
        }
      }

      power_text <- pairwise[[i]]$power_problem
      if (grepl("rejecting H0", power_text)) {
        power_code <- "*"
      } else if (grepl("too small", power_text)) {
        power_code <- "**"
      } else {
        power_code <- "***"
      }

      col_name <- paste0("Comp", i)
      pairwise_data[[col_name]] <- c(
        comparison_name,
        as.character(pairwise[[i]]$mean_diff),
        pairwise[[i]]$lsd_result,
        pairwise[[i]]$error_type,
        as.character(pairwise[[i]]$effect_size),
        power_code
      )
    }
  } else {
    pairwise_data <- data.frame(
      ` ` = c(
        "Pairwise comparison \u2192",
        "Mean difference \u2192",
        "LSD result \u2192",
        "Type of Stat Error risked \u2192",
        "Pairwise effect size (r) \u2192",
        "Power Problem? \u2192"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    for (i in 1:n_pairwise) {
      comparison_name <- pairwise[[i]]$comparison
      if (!is.null(group_labels)) {
        original_labels <- anova_results_list$group_labels
        if (!is.null(original_labels)) {
          for (j in seq_along(original_labels)) {
            comparison_name <- gsub(original_labels[j], group_labels[j],
                                    comparison_name, fixed = TRUE)
          }
        }
      }

      col_name <- paste0("Comp", i)
      pairwise_data[[col_name]] <- c(
        comparison_name,
        "",
        "",
        "",
        "",
        ""
      )
    }
  }

  pairwise_data <- tibble::as_tibble(pairwise_data)

  ft <- flextable::flextable(pairwise_data) |>
    flextable::theme_box() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::delete_part(part = "header") |>
    flextable::hline_top(border = officer::fp_border(width = 1), part = "body") |>
    flextable::autofit()

  if (KEY) {
    ft <- ft |> flextable::color(i = 2:6, j = 2:(n_pairwise + 1), color = "#d00000", part = "body")
  }

  caption_text <- paste0("*   No ", .en_dash(), " rejecting H0: means there was sufficient power\n",
    "**  No ", .en_dash(), " effect is \"too small to be interesting,\" (r < .10) so being nonsignificant doesn't indicate a power problem\n",
    "*** Yes ", .en_dash(), " The effect is \"large enough to be interesting,\" (r > .10) so being nonsignificant indicates there likely is a power problem"
  )

  ft <- flextable::add_footer_lines(ft, values = caption_text) |>
    flextable::align(align = "left", part = "footer") |>
    flextable::fontsize(size = 9, part = "footer") |>
    flextable::merge_at(part = "footer", i = 1) |>
    flextable::hline(part = "footer", border = officer::fp_border(width = 0))

  return(ft)
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





#' APA Write-Up for K-Group Between-Groups ANOVA
#'
#' Generates an APA-style paragraph reporting a one-way ANOVA with
#' post-hoc comparisons.
#'
#' @param anova_results_list Output from [anova_kgroup_answers()].
#' @param dv_name Character. Display name for the dependent variable.
#' @param iv_name Character. Display name for the independent variable.
#' @param group_labels Character vector or NULL. Display labels for groups.
#' @param hypothesis_text Character or NULL. Custom hypothesis statement.
#' @param posthoc_tests Character. Name of post-hoc test used. Default "LSD".
#' @param report_mse Logical. Include MSE in output. Default TRUE.
#' @param report_eta_sq Logical. Include eta-squared effect size. Default FALSE.
#'
#' @return A character string with APA-formatted results.
#'
#' @examples
#' \dontrun{
#' result <- anova_kgroup_answers(data, "score", "condition")
#' writeup <- apa_kgroup_bg_writeup(result, "Score", "Condition")
#' cat(writeup)
#' }
#'
#' @export
apa_kgroup_bg_writeup <- function(anova_results_list,
                                  dv_name,
                                  iv_name,
                                  group_labels = NULL,
                                  hypothesis_text = NULL,
                                  posthoc_tests = "LSD",
                                  report_mse = TRUE,
                                  report_eta_sq = FALSE) {

  f_val <- anova_results_list$ANOVA$F
  p_val <- anova_results_list$ANOVA$p_value
  df_between <- anova_results_list$ANOVA$df_between
  df_within <- anova_results_list$ANOVA$df_within
  mse <- anova_results_list$ANOVA$mse

  desc_stats <- anova_results_list$Descriptives

  if (is.null(group_labels)) {
    group_labels <- desc_stats$group_label
  }

  group_means <- desc_stats$mean
  group_sds <- desc_stats$sd
  group_ns <- desc_stats$n

  pairwise <- anova_results_list$Pairwise

  if (report_eta_sq) {
    ss_between <- f_val * mse * df_between
    ss_total <- ss_between + (mse * df_within)
    eta_sq <- ss_between / ss_total
  }

  if (p_val < 0.001) {
    p_text <- "< .001"
  } else {
    p_text <- paste0("= ", sub("^0", "", sprintf("%.3f", p_val)))
  }

  if (p_val < 0.05) {
    f_sentence <- glue::glue(
      "There was a significant difference in {dv_name} across {iv_name}, ",
      "*F*({df_between},{df_within}) = {f_val}"
    )
  } else {
    f_sentence <- glue::glue(
      "There was no significant difference in {dv_name} across {iv_name}, ",
      "*F*({df_between},{df_within}) = {f_val}"
    )
  }

  if (report_mse) {
    f_sentence <- paste0(f_sentence, glue::glue(", MS~e~ = {mse}"))
  }

  if (report_eta_sq) {
    f_sentence <- paste0(f_sentence, paste0(", ", .eta_sq_symbol(), " = ", round(eta_sq, 2)))
  }

  f_sentence <- paste0(f_sentence, glue::glue(", *p* {p_text}. "))

  if (p_val < 0.05) {
    f_sentence <- paste0(f_sentence, glue::glue("{posthoc_tests} was used as a follow-up test. "))
  }

  writeup <- f_sentence

  if (p_val < 0.05 && !is.null(pairwise) && length(pairwise) > 0) {

    sig_comps <- list()
    nonsig_comps <- list()

    for (i in 1:length(pairwise)) {
      comp <- pairwise[[i]]
      if (comp$lsd_result != "=") {
        sig_comps[[length(sig_comps) + 1]] <- comp
      } else {
        nonsig_comps[[length(nonsig_comps) + 1]] <- comp
      }
    }

    if (length(sig_comps) > 0) {

      if (!is.null(hypothesis_text)) {
        writeup <- paste0(writeup, hypothesis_text, " ")
      }

      group_comparisons <- list()

      for (comp in sig_comps) {
        groups <- trimws(strsplit(comp$comparison, " vs ")[[1]])
        group1 <- groups[1]
        group2 <- groups[2]

        idx1 <- which(group_labels == group1)
        idx2 <- which(group_labels == group2)

        if (length(idx1) == 0 || length(idx2) == 0) {
          next
        }

        mean1 <- group_means[idx1]
        mean2 <- group_means[idx2]
        sd1 <- group_sds[idx1]
        sd2 <- group_sds[idx2]

        if (mean1 > mean2) {
          higher_group <- group1
          higher_idx <- idx1
          lower_group <- group2
          lower_idx <- idx2
        } else {
          higher_group <- group2
          higher_idx <- idx2
          lower_group <- group1
          lower_idx <- idx1
        }

        if (is.null(group_comparisons[[higher_group]])) {
          group_comparisons[[higher_group]] <- list(
            mean = group_means[higher_idx],
            sd = group_sds[higher_idx],
            lower_than = list()
          )
        }

        group_comparisons[[higher_group]]$lower_than[[length(group_comparisons[[higher_group]]$lower_than) + 1]] <- list(
          name = lower_group,
          mean = group_means[lower_idx],
          sd = group_sds[lower_idx]
        )
      }

      comp_texts <- c()
      for (group_name in names(group_comparisons)) {
        group_info <- group_comparisons[[group_name]]

        lower_parts <- c()
        for (lower in group_info$lower_than) {
          lower_parts <- c(lower_parts,
                           glue::glue("{lower$name} (*M* = {lower$mean}, *SD* = {lower$sd})"))
        }

        if (length(lower_parts) == 1) {
          lower_text <- lower_parts[1]
        } else if (length(lower_parts) == 2) {
          lower_text <- paste(lower_parts[1], "and", lower_parts[2])
        } else {
          lower_text <- paste(paste(lower_parts[-length(lower_parts)], collapse = ", "),
                              "and", lower_parts[length(lower_parts)])
        }

        comp_texts <- c(comp_texts,
                        glue::glue("{group_name} (*M* = {group_info$mean}, *SD* = {group_info$sd}) ",
                             "was rated as significantly higher than {lower_text}"))
      }

      if (length(comp_texts) == 1) {
        writeup <- paste0(writeup, comp_texts[1], ". ")
      } else {
        writeup <- paste0(writeup, paste(comp_texts, collapse = " and "), ". ")
      }
    }

    if (length(nonsig_comps) > 0) {
      nonsig_pairs <- c()

      for (comp in nonsig_comps) {
        groups <- trimws(strsplit(comp$comparison, " vs ")[[1]])
        nonsig_pairs <- c(nonsig_pairs, paste(groups[1], "and", groups[2]))
      }

      writeup <- paste0(writeup, "Contrary to hypothesis, ")

      if (length(nonsig_pairs) == 1) {
        writeup <- paste0(writeup, nonsig_pairs[1], " were not significantly different.")
      } else if (length(nonsig_pairs) == 2) {
        writeup <- paste0(writeup, nonsig_pairs[1], " and ", nonsig_pairs[2],
                          " were not significantly different.")
      } else {
        writeup <- paste0(writeup, paste(nonsig_pairs[-length(nonsig_pairs)], collapse = ", "),
                          ", and ", nonsig_pairs[length(nonsig_pairs)],
                          " were not significantly different.")
      }
    }

  } else if (p_val >= 0.05) {
    desc_parts <- c()
    for (i in 1:length(group_labels)) {
      desc_parts <- c(desc_parts,
                      glue::glue("{group_labels[i]} (*M* = {group_means[i]}, *SD* = {group_sds[i]})"))
    }
    desc_text <- paste(desc_parts, collapse = ", ")
    writeup <- paste0(writeup, "Group means were: ", desc_text, ".")
  }

  return(writeup)
}

# RH support text









#' Chi-Square Pairwise Results Table (Answer KEY)
#'
#' Creates a flextable showing pairwise chi-square comparison results.
#'
#' @param chi_results_list Output from [chi_square_kgroup_answers()].
#' @param KEY Logical. If TRUE (default), fill with values; if FALSE, blank.
#' @param comparison_var_label Character or NULL. Label for the percentage
#'   comparison variable.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' \dontrun{
#' result <- chi_square_kgroup_answers(data, "group", "outcome")
#' chisq_pairwise_KEY(result, KEY = TRUE)
#' }
#'
#' @export
chisq_pairwise_KEY <- function(chi_results_list,
                               KEY = TRUE,
                               comparison_var_label = NULL) {

  pairwise <- chi_results_list$Pairwise
  n_pairwise <- length(pairwise)

  if (is.null(comparison_var_label)) {
    comparison_var_label <- chi_results_list$pct_var2_label
  }

  if (!is.null(comparison_var_label)) {
    pct_row_label <- paste0("% ", comparison_var_label, " \u2192")
  } else {
    pct_row_label <- "% comparison \u2192"
  }

  if (KEY) {
    pairwise_data <- data.frame(
      ` ` = c(
        "Pairwise comparison \u2192",
        pct_row_label,
        paste0(.chi_sq_symbol(), " result \u2192"),
        "Type of Stat Error risked \u2192",
        "Pairwise effect size (r) \u2192",
        "Power Problem? \u2192"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    for (i in 1:n_pairwise) {
      power_text <- pairwise[[i]]$power_problem
      if (grepl("rejecting H0", power_text)) {
        power_code <- "*"
      } else if (grepl("too small", power_text)) {
        power_code <- "**"
      } else {
        power_code <- "***"
      }

      col_name <- paste0("Comp", i)
      pairwise_data[[col_name]] <- c(
        pairwise[[i]]$comparison,
        paste0(pairwise[[i]]$pct1, "% vs ", pairwise[[i]]$pct2, "%"),
        paste0(pairwise[[i]]$chi_sq, " ", pairwise[[i]]$chi_result),  # <-- Updated line
        pairwise[[i]]$error_type,
        as.character(pairwise[[i]]$effect_size),
        power_code
      )
    }
  } else {
    pairwise_data <- data.frame(
      ` ` = c(
        "Pairwise comparison \u2192",
        pct_row_label,
        paste0(.chi_sq_symbol(), " result \u2192"),
        "Type of Stat Error risked \u2192",
        "Pairwise effect size (r) \u2192",
        "Power Problem? \u2192"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    for (i in 1:n_pairwise) {
      col_name <- paste0("Comp", i)
      pairwise_data[[col_name]] <- c(
        pairwise[[i]]$comparison,
        "",
        "",
        "",
        "",
        ""
      )
    }
  }

  pairwise_data <- tibble::as_tibble(pairwise_data)

  ft <- flextable::flextable(pairwise_data) |>
    flextable::theme_box() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::delete_part(part = "header") |>
    flextable::hline_top(border = officer::fp_border(width = 1), part = "body") |>
    flextable::autofit()

  if (KEY) {
    ft <- ft |> flextable::color(i = 2:6, j = 2:(n_pairwise + 1), color = "#d00000", part = "body")
  }

  caption_text <- paste0(
    "*   No \u2013 rejecting H0: means there was sufficient power\n",
    "**  No \u2013 effect is \"too small to be interesting,\" (r < .10) so being nonsignificant doesn't indicate a power problem\n",
    "*** Yes \u2013 The effect is \"large enough to be interesting,\" (r > .10) so being nonsignificant indicates there likely is a power problem"
  )

  ft <- flextable::add_footer_lines(ft, values = caption_text) |>
    flextable::align(align = "left", part = "footer") |>
    flextable::fontsize(size = 9, part = "footer") |>
    flextable::merge_at(part = "footer", i = 1) |>
    flextable::hline(part = "footer", border = officer::fp_border(width = 0))

  return(ft)
}









#' Research Hypothesis Support Text (Chi-Square)
#'
#' Creates formatted text evaluating whether research hypotheses are
#' supported based on pairwise chi-square comparison results.
#'
#' @param chi_results_list Output from [chi_square_kgroup_answers()].
#' @param hypotheses_list A list of hypothesis specifications. Each element
#'   should have `group1`, `group2`, `direction` (">", "<", or "="), and `text`.
#' @param var1_labels Character vector or NULL. Labels for var1 levels.
#' @param var2_labels Character vector or NULL. Labels for var2 levels.
#' @param KEY Logical. If TRUE (default), show answers; if FALSE, show blanks.
#' @param highlight Logical. If TRUE and KEY is TRUE, wrap answers in
#'   highlight formatting.
#'
#' @return A character string with formatted RH evaluation.
#'
#' @examples
#' \dontrun{
#' hypotheses <- list(
#'   list(group1 = "Young", group2 = "Old", direction = ">",
#'        text = "Young will have higher percentage than Old")
#' )
#' create_chi_rh_support_text(result, hypotheses, KEY = TRUE)
#' }
#'
#' @export
create_chi_rh_support_text <- function(chi_results_list,
                                       hypotheses_list,
                                       var1_labels = NULL,
                                       var2_labels = NULL,
                                       KEY = TRUE,
                                       highlight = FALSE) {

  if (is.null(var1_labels)) {
    var1_labels <- chi_results_list$Var1_Descriptives$level_label
  }
  if (is.null(var2_labels)) {
    var2_labels <- chi_results_list$Var2_Descriptives$level_label
  }

  pairwise <- chi_results_list$Pairwise

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

    if (is_reversed) {
      pct1 <- comp_result$pct2
      pct2 <- comp_result$pct1
      actual_result <- comp_result$chi_result
      if (actual_result == ">") {
        actual_result <- "<"
      } else if (actual_result == "<") {
        actual_result <- ">"
      }
    } else {
      pct1 <- comp_result$pct1
      pct2 <- comp_result$pct2
      actual_result <- comp_result$chi_result
    }

    supported <- FALSE
    result_text <- ""

    if (expected_direction == ">") {
      if (actual_result == ">" && pct1 > pct2) {
        supported <- TRUE
        result_text <- paste0(hl("Fully supported"), " -- ", pct1, "% significantly greater than ", pct2, "%")
      } else if (actual_result == "<" || (actual_result == ">" && pct1 < pct2)) {
        supported <- FALSE
        result_text <- paste0(hl("Not supported"), " -- ", pct1, "% significantly less than ", pct2, "%")
      } else {
        supported <- FALSE
        result_text <- paste0(hl("Not supported"), " -- ", pct1, "% not significantly different than ", pct2, "%")
      }
    } else if (expected_direction == "<") {
      if (actual_result == "<" && pct1 < pct2) {
        supported <- TRUE
        result_text <- paste0(hl("Fully supported"), " -- ", pct1, "% significantly less than ", pct2, "%")
      } else if (actual_result == ">" || (actual_result == "<" && pct1 > pct2)) {
        supported <- FALSE
        result_text <- paste0(hl("Not supported"), " -- ", pct1, "% significantly greater than ", pct2, "%")
      } else {
        supported <- FALSE
        result_text <- paste0(hl("Not supported"), " -- ", pct1, "% not significantly different than ", pct2, "%")
      }
    } else if (expected_direction == "=") {
      if (actual_result == "=") {
        supported <- TRUE
        result_text <- paste0(hl("Fully supported"), " -- ", pct1, "% not significantly different than ", pct2, "%")
      } else {
        supported <- FALSE
        if (pct1 > pct2) {
          result_text <- paste0(hl("Not supported"), " -- ", pct1, "% significantly greater than ", pct2, "%")
        } else {
          result_text <- paste0(hl("Not supported"), " -- ", pct1, "% significantly less than ", pct2, "%")
        }
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









#' APA Write-Up for K-Group Chi-Square Test
#'
#' Generates an APA-style paragraph reporting a chi-square test of
#' independence with multiple groups.
#'
#' @param chi_results_list Output from [chi_square_kgroup_answers()].
#' @param var1_name Character. Display name for variable 1.
#' @param var2_name Character. Display name for variable 2.
#' @param var1_labels Character vector or NULL. Labels for var1 levels.
#' @param var2_labels Character vector of length 2. Labels for var2 levels.
#' @param hypothesis_text Character or NULL. Custom hypothesis statement.
#' @param conclusion_intro Character or NULL. Custom conclusion introduction
#'   (e.g., "As hypothesized" or "Contrary to our hypothesis").
#' @param subject Character or NULL. Subject description for the sentence
#'   (e.g., "Participants").
#'
#' @return A character string with APA-formatted results.
#'
#' @examples
#' \dontrun{
#' result <- chi_square_kgroup_answers(data, "age_group", "outcome")
#' writeup <- apa_kgroup_chi_writeup(result, "Age Group", "Outcome")
#' cat(writeup)
#' }
#'
#' @export
apa_kgroup_chi_writeup <- function(chi_results_list,
                                   var1_name,
                                   var2_name,
                                   var1_labels = NULL,
                                   var2_labels = c("Level A", "Level B"),
                                   hypothesis_text = NULL,
                                   conclusion_intro = NULL,
                                   subject = NULL) {

  chi_sq <- chi_results_list$ChiSquare$chi_sq
  p <- chi_results_list$ChiSquare$p_value
  df <- chi_results_list$ChiSquare$df

  cont_table <- chi_results_list$ContingencyTable
  var1_desc <- chi_results_list$Var1_Descriptives

  if (is.null(var1_labels)) {
    var1_labels <- var1_desc$level_label
  }

  pct_var2_level <- chi_results_list$pct_var2_level
  if (is.null(pct_var2_level)) {
    pct_var2_level <- 2
  }

  p_text <- sub("^0", "", sprintf("%.3f", p))

  desc_parts <- c()
  for (i in 1:nrow(var1_desc)) {
    group_total <- var1_desc$n[i]
    group_count <- cont_table[i, pct_var2_level]
    group_pct <- round((group_count / group_total) * 100, 2)

    desc_parts <- c(desc_parts,
                    glue::glue("{var1_labels[i]} (*n* = {group_total}; {group_pct}%)"))
  }

  if (length(desc_parts) == 2) {
    desc_text <- paste(desc_parts, collapse = " and ")
  } else {
    desc_text <- paste(desc_parts, collapse = ", ")
  }

  if (p < 0.05) {
    sig_phrase <- "a significant relationship"
    equality_phrase <- "differed in their"
  } else {
    sig_phrase <- "no significant relationship"
    equality_phrase <- "were equally"
  }

  if (is.null(hypothesis_text)) {
    hypothesis_text <- glue::glue("We hypothesized a relationship between {var1_name} and {var2_name}.")
  }

  if (is.null(conclusion_intro)) {
    conclusion_intro <- ifelse(p < 0.05,
                               "As hypothesized",
                               "Contrary to our hypothesis")
  }

  if (is.null(subject)) {
    subject_phrase <- ""
    from_phrase <- "both"
  } else {
    subject_phrase <- paste(subject, "from")
    from_phrase <- "both"
  }

  writeup <- glue::glue(
    "{hypothesis_text} A two-way chi-square test found {sig_phrase} between ",
    "{var1_name} and {var2_name}, {.chi_sq_symbol()}({df}) = {chi_sq}, *p* = {p_text}. ",
    "{conclusion_intro}, {subject_phrase} {from_phrase} {desc_text} {equality_phrase} likely to be {var2_labels[pct_var2_level]}."
  )

  return(writeup)
}



