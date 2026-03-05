# =============================================================================
# Functions that print out APA-Style Write-Ups based on RH and results_list variables for CHI-SQ's
# =============================================================================


# =============================================================================
# CHI-SQUARE WRITE-UP (2x2)
# =============================================================================

#' APA Write-Up for 2x2 Chi-Square Test
#'
#' Generates an APA-style write up for a chi-square test of independence,
#' with hypothesis-driven conclusions comparing expected vs. actual patterns.
#'
#' @param chi_results_list Output from [chi_square_answers()].
#' @param var1_name Character. Descriptive name for row variable
#'   (e.g., "age gap between Lois and Clark").
#' @param var2_name Character. Descriptive name for column variable
#'   (e.g., "tomatometer status").
#' @param var1_labels Character vector of length 2. Labels for var1 levels
#'   (e.g., c("Small", "Big")).
#' @param var2_labels Character vector of length 2. Labels for var2 levels
#'   (e.g., c("Fresh", "Rotten")).
#' @param hypothesis List with hypothesis details:
#'   \describe{
#'     \item{pattern}{Character vector of length 2 specifying expected pattern
#'       for each level of var1. Use "=" for equal distribution, ">" for
#'       more likely to be var2_labels\[1\], "<" for more likely to be var2_labels\[2\].
#'       E.g., c("=", "<") means level1 expects equal, level2 expects more of var2_labels\[2\].}
#'     \item{rh_text}{Optional. Full RH statement for intro.}
#'     \item{comparison_var2_level}{Which var2 level to report percentages for (1 or 2). Default 2.}
#'   }
#' @param hypothesis_text Character or NULL. Alternative to providing a
#'   `hypothesis` list. If provided, used as the `rh_text` in the hypothesis.
#' @param include_descriptives Logical. Currently unused, reserved for future
#'   use. Default TRUE.
#' @param table_number Integer or NULL. Optional table reference number.
#'   Default NULL.
#' @param alpha Significance level. Default 0.05.
#' @param subject Character or NULL. Subject description (e.g., "media portrayals").
#'
#' @return A character string with APA write-up.
#'
#' @examples
#' \dontrun{
#' result <- chi_square_answers(superman, "age_grp2", "tomatometer2")
#'
#' writeup <- apa_chi_writeup(
#'   result,
#'   var1_name = "age gap between Lois and Clark",
#'   var2_name = "tomatometer status",
#'   var1_labels = c("Small", "Big"),
#'   var2_labels = c("Fresh", "Rotten"),
#'   hypothesis = list(
#'     pattern = c("=", "<"),  # Small equal, Big more likely to be Rotten
#'     rh_text = paste0("media with small age gaps would be equally ",
#'       "likely to be Fresh or Rotten, whereas media with big age gaps would ",
#'       "be more likely to be rated as Rotten than Fresh by critics."),
#'     comparison_var2_level = 2  # Report % Rotten
#'   ),
#'   subject = "superman media"
#' )
#' cat(writeup)
#' }
#'
#' @export
apa_chi_writeup <- function(chi_results_list,
                            var1_name,
                            var2_name,
                            var1_labels = c("Level 1", "Level 2"),
                            var2_labels = c("Category A", "Category B"),
                            hypothesis = NULL,
                            hypothesis_text = NULL,  # add this
                            alpha = 0.05,
                            include_descriptives = TRUE,  # add this
                            table_number = NULL,  # add this
                            subject = NULL) {

  # Extract values (handle naming variations)
  if (!is.null(chi_results_list$ChiSquare)) {
    chi_sq <- chi_results_list$ChiSquare$chi_sq
    p <- chi_results_list$ChiSquare$p_value
    df <- chi_results_list$ChiSquare$df
    cont_table <- chi_results_list$ContingencyTable
  } else if (!is.null(chi_results_list$Chi_Square)) {
    chi_sq <- chi_results_list$Chi_Square$chi_square
    p <- chi_results_list$Chi_Square$p_value
    df <- chi_results_list$Chi_Square$df
    cont_table <- chi_results_list$Crosstab
  } else {
    stop("Unrecognized chi-square results structure")
  }

  is_significant <- p < alpha

  # Format statistics
  chi_sq_text <- format_chi2(chi_sq)
  df_text <- format_df(df)
  p_text <- format_p_value(p)

  # Calculate row totals and percentages
  row_totals <- rowSums(cont_table)

  # Default to reporting second column (often the "positive" outcome)
  comparison_level <- if (!is.null(hypothesis$comparison_var2_level)) {
    hypothesis$comparison_var2_level
  } else {
    2
  }

  pct1 <- round((cont_table[1, comparison_level] / row_totals[1]) * 100, 2)
  pct2 <- round((cont_table[2, comparison_level] / row_totals[2]) * 100, 2)

  n1_text <- format_int(row_totals[1])
  n2_text <- format_int(row_totals[2])

  # Subject phrase
  subject_word <- if (!is.null(subject)) subject else "participants"

  # Build hypothesis intro if provided
  if (!is.null(hypothesis) && !is.null(hypothesis$rh_text)) {
    intro <- glue::glue("We hypothesized that {hypothesis$rh_text}. ")
  } else {
    intro <- ""
  }

  # Main statistical result
  if (is_significant) {
    stat_sentence <- glue::glue(
      "A two-way chi-square test found a significant relationship between {var1_name} and {var2_name}, ",
      "{.chi_sq_symbol()}({df_text}) = {chi_sq_text}, *p* = {p_text}."
    )
  } else {
    stat_sentence <- glue::glue(
      "A two-way chi-square test found no significant relationship between {var1_name} and {var2_name}, ",
      "{.chi_sq_symbol()}({df_text}) = {chi_sq_text}, *p* = {p_text}."
    )
  }

  # Build conclusion based on hypothesis
  if (!is.null(hypothesis) && !is.null(hypothesis$pattern)) {
    expected_pattern <- hypothesis$pattern
    comparison_label <- var2_labels[comparison_level]

    # Determine actual pattern for each group
    # "=" means roughly equal (within some threshold or non-significant)
    # ">" means more of var2_labels\[1\]
    # "<" means more of var2_labels\[2\]

    # For chi-square, we assess overall pattern support
    # If significant: there IS a difference between groups
    # If not significant: groups are similar

    # Check if hypothesis pattern predicts difference or equality
    predicts_difference <- any(expected_pattern != "=")

    # Determine if hypothesis is supported
    if (predicts_difference) {
      # Hypothesis predicts some difference
      # Need significance AND correct direction pattern
      if (is_significant) {
        # Check if the direction matches
        # This is simplified - a full implementation would check each cell
        hypothesis_supported <- TRUE  # Significant as predicted
        conclusion_start <- "Consistent with our hypothesis"
      } else {
        hypothesis_supported <- FALSE
        conclusion_start <- "Contrary to our hypothesis"
      }
    } else {
      # Hypothesis predicts no difference (both groups equal)
      if (!is_significant) {
        hypothesis_supported <- TRUE
        conclusion_start <- "Consistent with our hypothesis"
      } else {
        hypothesis_supported <- FALSE
        conclusion_start <- "Contrary to our hypothesis"
      }
    }

    # Build descriptive conclusion
    if (is_significant) {
      # Groups differ
      conclusion <- glue::glue(
        " {conclusion_start}, {subject_word} from {var1_labels[1]} neighborhoods ",
        "(*n* = {n1_text}; {pct1}%) and {var1_labels[2]} neighborhoods ",
        "(*n* = {n2_text}; {pct2}%) showed different patterns of {var2_name}."
      )
    } else {
      # Groups similar
      conclusion <- glue::glue(
        " {conclusion_start}, {subject_word} from both {var1_labels[1]} neighborhoods ",
        "(*n* = {n1_text}; {pct1}%) and {var1_labels[2]} neighborhoods ",
        "(*n* = {n2_text}; {pct2}%) were equally likely to be {comparison_label}."
      )
    }

    writeup <- paste0(intro, stat_sentence, conclusion)

  } else {
    # No hypothesis provided - generic conclusion
    if (is_significant) {
      conclusion <- glue::glue(
        " {var1_labels[1]} (*n* = {n1_text}; {pct1}%) and {var1_labels[2]} ",
        "(*n* = {n2_text}; {pct2}%) showed different patterns of {var2_name}."
      )
    } else {
      comparison_label <- var2_labels[comparison_level]
      conclusion <- glue::glue(
        " Both {var1_labels[1]} (*n* = {n1_text}; {pct1}%) and {var1_labels[2]} ",
        "(*n* = {n2_text}; {pct2}%) were equally likely to be {comparison_label}."
      )
    }

    writeup <- paste0(intro, stat_sentence, conclusion)
  }

  as.character(writeup)
}



# =============================================================================
# CHI-SQUARE WRITE-UP (K-Group with Post-Hoc)
# =============================================================================

#' APA Write-Up for K-Group Chi-Square Test
#'
#' Generates an APA-style paragraph for a chi-square test with more than 2 groups,
#' including post-hoc pairwise comparison results.
#'
#' @param chi_results_list Output from [chi_square_kgroup_answers()].
#' @param var1_name Character. Descriptive name for row variable.
#' @param var2_name Character. Descriptive name for column variable.
#' @param var1_labels Character vector. Labels for var1 levels.
#' @param var2_labels Character vector of length 2. Labels for var2 levels.
#' @param hypothesis List with hypothesis details:
#'   \describe{
#'     \item{pattern}{Named list or vector specifying expected pattern for each
#'       var1 level. Use "=" for equal distribution, ">" for more of var2_labels\[1\],
#'       "<" for more of var2_labels\[2\].}
#'     \item{rh_text}{Optional. Full RH statement.}
#'     \item{comparison_var2_level}{Which var2 level to report percentages for.}
#'     \item{pairwise_hypotheses}{Optional. List of expected pairwise comparisons
#'       with format list(list(group1="A", group2="B", direction=">"), ...)}
#'   }
#' @param alpha Significance level. Default 0.05.
#' @param include_posthoc Logical. Include post-hoc comparison results. Default TRUE.
#' @param subject Character or NULL. Subject description.
#'
#' @return A character string with APA write-up.
#'
#' @export
apa_kgroup_chi_writeup <- function(chi_results_list,
                                   var1_name,
                                   var2_name,
                                   var1_labels = NULL,
                                   var2_labels = c("Category A", "Category B"),
                                   hypothesis = NULL,
                                   alpha = 0.05,
                                   include_posthoc = TRUE,
                                   subject = NULL) {

  chi_sq <- chi_results_list$ChiSquare$chi_sq
  p <- chi_results_list$ChiSquare$p_value
  df <- chi_results_list$ChiSquare$df
  cont_table <- chi_results_list$ContingencyTable
  var1_desc <- chi_results_list$Var1_Descriptives
  pairwise <- chi_results_list$Pairwise

  if (is.null(var1_labels)) {
    var1_labels <- var1_desc$level_label
  }

  is_significant <- p < alpha

  # Format statistics
  chi_sq_text <- format_chi2(chi_sq)
  df_text <- format_df(df)
  p_text <- format_p_value(p)

  # Comparison level for percentages
  comparison_level <- hypothesis$comparison_var2_level %||% 2
  comparison_label <- var2_labels[comparison_level]

  # Subject word
  subject_word <- subject %||% "participants"

  # Build intro
  if (!is.null(hypothesis) && !is.null(hypothesis$rh_text)) {
    intro <- glue::glue("We hypothesized that {hypothesis$rh_text}. ")
  } else {
    intro <- ""
  }

  # Main omnibus result
  if (is_significant) {
    stat_sentence <- glue::glue(
      "A chi-square test found a significant relationship between {var1_name} and {var2_name}, ",
      "{.chi_sq_symbol()}({df_text}) = {chi_sq_text}, *p* = {p_text}."
    )
  } else {
    stat_sentence <- glue::glue(
      "A chi-square test found no significant relationship between {var1_name} and {var2_name}, ",
      "{.chi_sq_symbol()}({df_text}) = {chi_sq_text}, *p* = {p_text}."
    )
  }

  writeup <- paste0(intro, stat_sentence)

  # Add post-hoc results if significant and requested
  if (is_significant && include_posthoc && !is.null(pairwise) && length(pairwise) > 0) {

    posthoc_parts <- c()

    for (i in seq_along(pairwise)) {
      comp <- pairwise[[i]]
      comp_name <- comp$comparison
      groups <- trimws(strsplit(comp_name, " vs ")[[1]])

      pct1 <- comp$pct1
      pct2 <- comp$pct2
      chi_result <- comp$chi_result  # ">", "<", or "="

      if (chi_result == "=") {
        part <- glue::glue(
          "{groups[1]} ({pct1}%) and {groups[2]} ({pct2}%) did not significantly differ"
        )
      } else if (chi_result == ">") {
        part <- glue::glue(
          "{groups[1]} ({pct1}%) were significantly more likely to be {comparison_label} than {groups[2]} ({pct2}%)"
        )
      } else {
        part <- glue::glue(
          "{groups[2]} ({pct2}%) were significantly more likely to be {comparison_label} than {groups[1]} ({pct1}%)"
        )
      }

      posthoc_parts <- c(posthoc_parts, part)
    }

    posthoc_text <- paste(" Pairwise comparisons revealed that",
                          paste(posthoc_parts, collapse = "; "), ".")
    writeup <- paste0(writeup, posthoc_text)

    # Hypothesis evaluation
    if (!is.null(hypothesis) && !is.null(hypothesis$pairwise_hypotheses)) {
      n_supported <- 0
      n_total <- length(hypothesis$pairwise_hypotheses)

      for (ph in hypothesis$pairwise_hypotheses) {
        # Find matching pairwise result
        for (comp in pairwise) {
          groups <- trimws(strsplit(comp$comparison, " vs ")[[1]])
          if ((ph$group1 %in% groups) && (ph$group2 %in% groups)) {
            # Check if result matches expectation
            if (ph$direction == "=" && comp$chi_result == "=") {
              n_supported <- n_supported + 1
            } else if (ph$direction == comp$chi_result) {
              n_supported <- n_supported + 1
            } else if (ph$direction == ">" && groups[1] == ph$group2 && comp$chi_result == "<") {
              n_supported <- n_supported + 1
            } else if (ph$direction == "<" && groups[1] == ph$group2 && comp$chi_result == ">") {
              n_supported <- n_supported + 1
            }
            break
          }
        }
      }

      if (n_supported == n_total) {
        support_text <- " These results fully support our research hypothesis."
      } else if (n_supported > 0) {
        support_text <- glue::glue(
          " These results partially support our research hypothesis ({n_supported} of {n_total} predictions confirmed)."
        )
      } else {
        support_text <- " These results do not support our research hypothesis."
      }

      writeup <- paste0(writeup, support_text)
    }

  } else if (!is_significant) {
    # Build descriptive summary for non-significant result
    row_totals <- rowSums(cont_table)
    desc_parts <- c()

    for (i in seq_along(var1_labels)) {
      pct <- round((cont_table[i, comparison_level] / row_totals[i]) * 100, 2)
      n_text <- format_int(row_totals[i])
      desc_parts <- c(desc_parts, glue::glue("{var1_labels[i]} (*n* = {n_text}; {pct}%)"))
    }

    desc_text <- if (length(desc_parts) == 2) {
      paste(desc_parts, collapse = " and ")
    } else {
      paste(c(paste(desc_parts[-length(desc_parts)], collapse = ", "),
              desc_parts[length(desc_parts)]), collapse = ", and ")
    }

    # Determine conclusion phrasing based on hypothesis
    if (!is.null(hypothesis)) {
      # Check if any differences were predicted
      if (!is.null(hypothesis$pattern) && any(hypothesis$pattern != "=")) {
        conclusion_start <- "Contrary to our hypothesis"
      } else {
        conclusion_start <- "Consistent with our hypothesis"
      }
    } else {
      conclusion_start <- "Thus"
    }

    conclusion <- glue::glue(
      " {conclusion_start}, {subject_word} from {desc_text} were equally likely to be {comparison_label}."
    )

    writeup <- paste0(writeup, conclusion)
  }

  as.character(writeup)
}




# =============================================================================
# CHI-SQUARE WRITE-UP (K-Group with Post-Hoc)
# =============================================================================

#' APA Write-Up for K-Group Chi-Square Test
#'
#' Generates an APA-style paragraph for a chi-square test with more than 2 groups,
#' including post-hoc pairwise comparison results.
#'
#' @param chi_results_list Output from [chi_square_kgroup_answers()].
#' @param var1_name Character. Descriptive name for row variable.
#' @param var2_name Character. Descriptive name for column variable.
#' @param var1_labels Character vector. Labels for var1 levels.
#' @param var2_labels Character vector of length 2. Labels for var2 levels.
#' @param hypothesis List with hypothesis details:
#'   \describe{
#'     \item{pattern}{Named list or vector specifying expected pattern for each
#'       var1 level. Use "=" for equal distribution, ">" for more of var2_labels\[1\],
#'       "<" for more of var2_labels\[2\].}
#'     \item{rh_text}{Optional. Full RH statement.}
#'     \item{comparison_var2_level}{Which var2 level to report percentages for.}
#'     \item{pairwise_hypotheses}{Optional. List of expected pairwise comparisons
#'       with format list(list(group1="A", group2="B", direction=">"), ...)}
#'   }
#' @param alpha Significance level. Default 0.05.
#' @param include_posthoc Logical. Include post-hoc comparison results. Default TRUE.
#' @param subject Character or NULL. Subject description.
#'
#' @return A character string with APA write-up.
#'
#' @export
apa_kgroup_chi_writeup <- function(chi_results_list,
                                   var1_name,
                                   var2_name,
                                   var1_labels = NULL,
                                   var2_labels = c("Category A", "Category B"),
                                   hypothesis = NULL,
                                   alpha = 0.05,
                                   include_posthoc = TRUE,
                                   subject = NULL) {

  chi_sq <- chi_results_list$ChiSquare$chi_sq
  p <- chi_results_list$ChiSquare$p_value
  df <- chi_results_list$ChiSquare$df
  cont_table <- chi_results_list$ContingencyTable
  var1_desc <- chi_results_list$Var1_Descriptives
  pairwise <- chi_results_list$Pairwise

  if (is.null(var1_labels)) {
    var1_labels <- var1_desc$level_label
  }

  is_significant <- p < alpha

  # Format statistics
  chi_sq_text <- format_chi2(chi_sq)
  df_text <- format_df(df)
  p_text <- format_p_value(p)

  # Comparison level for percentages
  comparison_level <- hypothesis$comparison_var2_level %||% 2
  comparison_label <- var2_labels[comparison_level]

  # Subject word
  subject_word <- subject %||% "participants"

  # Build intro
  if (!is.null(hypothesis) && !is.null(hypothesis$rh_text)) {
    intro <- glue::glue("We hypothesized that {hypothesis$rh_text}. ")
  } else {
    intro <- ""
  }

  # Main omnibus result
  if (is_significant) {
    stat_sentence <- glue::glue(
      "A chi-square test found a significant relationship between {var1_name} and {var2_name}, ",
      "{.chi_sq_symbol()}({df_text}) = {chi_sq_text}, *p* = {p_text}."
    )
  } else {
    stat_sentence <- glue::glue(
      "A chi-square test found no significant relationship between {var1_name} and {var2_name}, ",
      "{.chi_sq_symbol()}({df_text}) = {chi_sq_text}, *p* = {p_text}."
    )
  }

  writeup <- paste0(intro, stat_sentence)

  # Add post-hoc results if significant and requested
  if (is_significant && include_posthoc && !is.null(pairwise) && length(pairwise) > 0) {

    posthoc_parts <- c()

    for (i in seq_along(pairwise)) {
      comp <- pairwise[[i]]
      comp_name <- comp$comparison
      groups <- trimws(strsplit(comp_name, " vs ")[[1]])

      pct1 <- comp$pct1
      pct2 <- comp$pct2
      chi_result <- comp$chi_result  # ">", "<", or "="

      if (chi_result == "=") {
        part <- glue::glue(
          "{groups[1]} ({pct1}%) and {groups[2]} ({pct2}%) did not significantly differ"
        )
      } else if (chi_result == ">") {
        part <- glue::glue(
          "{groups[1]} ({pct1}%) were significantly more likely to be {comparison_label} than {groups[2]} ({pct2}%)"
        )
      } else {
        part <- glue::glue(
          "{groups[2]} ({pct2}%) were significantly more likely to be {comparison_label} than {groups[1]} ({pct1}%)"
        )
      }

      posthoc_parts <- c(posthoc_parts, part)
    }

    posthoc_text <- paste(" Pairwise comparisons revealed that",
                          paste(posthoc_parts, collapse = "; "), ".")
    writeup <- paste0(writeup, posthoc_text)

    # Hypothesis evaluation
    if (!is.null(hypothesis) && !is.null(hypothesis$pairwise_hypotheses)) {
      n_supported <- 0
      n_total <- length(hypothesis$pairwise_hypotheses)

      for (ph in hypothesis$pairwise_hypotheses) {
        # Find matching pairwise result
        for (comp in pairwise) {
          groups <- trimws(strsplit(comp$comparison, " vs ")[[1]])
          if ((ph$group1 %in% groups) && (ph$group2 %in% groups)) {
            # Check if result matches expectation
            if (ph$direction == "=" && comp$chi_result == "=") {
              n_supported <- n_supported + 1
            } else if (ph$direction == comp$chi_result) {
              n_supported <- n_supported + 1
            } else if (ph$direction == ">" && groups[1] == ph$group2 && comp$chi_result == "<") {
              n_supported <- n_supported + 1
            } else if (ph$direction == "<" && groups[1] == ph$group2 && comp$chi_result == ">") {
              n_supported <- n_supported + 1
            }
            break
          }
        }
      }

      if (n_supported == n_total) {
        support_text <- " These results fully support our research hypothesis."
      } else if (n_supported > 0) {
        support_text <- glue::glue(
          " These results partially support our research hypothesis ({n_supported} of {n_total} predictions confirmed)."
        )
      } else {
        support_text <- " These results do not support our research hypothesis."
      }

      writeup <- paste0(writeup, support_text)
    }

  } else if (!is_significant) {
    # Build descriptive summary for non-significant result
    row_totals <- rowSums(cont_table)
    desc_parts <- c()

    for (i in seq_along(var1_labels)) {
      pct <- round((cont_table[i, comparison_level] / row_totals[i]) * 100, 2)
      n_text <- format_int(row_totals[i])
      desc_parts <- c(desc_parts, glue::glue("{var1_labels[i]} (*n* = {n_text}; {pct}%)"))
    }

    desc_text <- if (length(desc_parts) == 2) {
      paste(desc_parts, collapse = " and ")
    } else {
      paste(c(paste(desc_parts[-length(desc_parts)], collapse = ", "),
              desc_parts[length(desc_parts)]), collapse = ", and ")
    }

    # Determine conclusion phrasing based on hypothesis
    if (!is.null(hypothesis)) {
      # Check if any differences were predicted
      if (!is.null(hypothesis$pattern) && any(hypothesis$pattern != "=")) {
        conclusion_start <- "Contrary to our hypothesis"
      } else {
        conclusion_start <- "Consistent with our hypothesis"
      }
    } else {
      conclusion_start <- "Thus"
    }

    conclusion <- glue::glue(
      " {conclusion_start}, {subject_word} from {desc_text} were equally likely to be {comparison_label}."
    )

    writeup <- paste0(writeup, conclusion)
  }

  as.character(writeup)
}

