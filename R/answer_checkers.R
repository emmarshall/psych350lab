# =============================================================================
# Interactive Homework Checker Functions for Webexercises (tinytable)
# =============================================================================
# These functions create tinytable-based interactive widgets using
# webexercises::fitb() for fill-in-the-blank and webexercises::mcq() for
# multiple choice. They require the 'tinytable' and 'webexercises' packages.
#
# Formatting helpers are defined in utils.R:
#   .format_stat()   - unbounded stats (F, t, means, SDs, b)
#   .format_stat(bounded=TRUE) - bounded stats (r, R², effect sizes)
#   .format_p_apa()  - p-values
#   .format_int()    - integers (n, df, k)
#   .safe_format()   - wrapper that handles NULL/NA safely
#
# Updated for dplyr 1.2.0
# =============================================================================


# =============================================================================
# MCQ HELPER FUNCTIONS
# =============================================================================

#' Convert a p-value to significance stars
#'
#' @param p Numeric p-value.
#' @return Character: `"***"`, `"**"`, `"*"`, or `"ns"`.
#' @keywords internal
#' @export
p_to_stars <- function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  "ns"
}


#' Create a significance-level MCQ widget
#'
#' Generates a [webexercises::mcq()] dropdown with the correct
#' significance level pre-selected.
#'
#' @param p_value Numeric p-value.
#' @return HTML string from [webexercises::mcq()].
#'
#' @examples
#' \dontrun{
#' # Create MCQ for a p-value that would show "***"
#' sig_mcq(0.0001)
#'
#' # Create MCQ for a p-value that would show "*"
#' sig_mcq(0.03)
#' }
#'
#' @keywords internal
#' @export
sig_mcq <- function(p_value) {
  .check_packages("webexercises")

  stars <- p_to_stars(p_value)

  mcq_options <- list(
    "ns"  = c(answer = "ns", "*", "**", "***"),
    "*"   = c("ns", answer = "*", "**", "***"),
    "**"  = c("ns", "*", answer = "**", "***"),
    "***" = c("ns", "*", "**", answer = "***")
  )

  webexercises::mcq(mcq_options[[stars]])
}


#' Create reject/retain H0 MCQ based on p-value
#'
#' @param p_value Numeric p-value.
#' @param alpha Numeric significance level. Default 0.05.
#' @return HTML string from [webexercises::mcq()].
#' @keywords internal
#' @noRd
.make_reject_retain_mcq <- function(p_value, alpha = 0.05) {
  .check_packages("webexercises")

  if (is.na(p_value)) {
    return(webexercises::mcq(c(
      "Retain the H0 null hypothesis",
      "Reject the H0 null hypothesis"
    )))
  }

  if (p_value < alpha) {
    webexercises::mcq(c(
      "Retain the H0 null hypothesis",
      answer = "Reject the H0 null hypothesis"
    ))
  } else {
    webexercises::mcq(c(
      answer = "Retain the H0 null hypothesis",
      "Reject the H0 null hypothesis"
    ))
  }
}


#' Create posthoc needed MCQ based on p-value
#'
#' @param p_value Numeric p-value.
#' @param test_type Character. Type of test ("omnibus_f", "interaction", "chi_square").
#' @param alpha Numeric significance level. Default 0.05.
#' @return HTML string from [webexercises::mcq()].
#' @keywords internal
#' @noRd
.make_posthoc_mcq <- function(p_value, test_type = "omnibus_f", alpha = 0.05) {
  .check_packages("webexercises")

  options <- switch(test_type,
                    "omnibus_f" = c(
                      no  = "No - a nonsignificant Omnibus F-test",
                      yes = "Yes - significant Omnibus F-test"
                    ),
                    "interaction" = c(
                      no  = "No - a nonsignificant interaction",
                      yes = "Yes - significant interaction"
                    ),
                    "chi_square" = c(
                      no  = paste0("No - a nonsignificant Omnibus ", .chi_sq_symbol(), " test"),
                      yes = paste0("Yes - significant Omnibus ", .chi_sq_symbol(), " test")
                    ),
                    c(no = "No", yes = "Yes")
  )

  if (is.na(p_value)) {
    return(webexercises::mcq(c(unname(options["no"]), unname(options["yes"]))))
  }

  if (p_value < alpha) {
    webexercises::mcq(c(unname(options["no"]), answer = unname(options["yes"])))
  } else {
    webexercises::mcq(c(answer = unname(options["no"]), unname(options["yes"])))
  }
}


#' Create error type MCQ based on significance
#'
#' @param is_significant Logical. Whether the result is significant.
#' @return HTML string from [webexercises::mcq()].
#' @keywords internal
#' @noRd
.make_error_type_mcq <- function(is_significant) {
  .check_packages("webexercises")

  if (is_significant) {
    webexercises::mcq(c(answer = "Type I & III", "Type II"))
  } else {
    webexercises::mcq(c("Type I & III", answer = "Type II"))
  }
}


#' Create power problem MCQ based on power assessment
#'
#' @param power_text Character. Power problem description from results.
#' @return HTML string from [webexercises::mcq()].
#' @keywords internal
#' @noRd
.make_power_mcq <- function(power_text) {
  .check_packages("webexercises")

  options <- c(
    "No - rejecting H0: means there was sufficient power",
    "No - effect is \"too small to be interesting,\" (r < .10)",
    "Yes - The effect is \"large enough to be interesting,\" (r > .10)"
  )

  if (grepl("rejecting H0", power_text)) {
    names(options) <- c("answer", "", "")
  } else if (grepl("too small", power_text)) {
    names(options) <- c("", "answer", "")
  } else {
    names(options) <- c("", "", "answer")
  }

  webexercises::mcq(options)
}



#' Interactive Descriptive Statistics Homework Checker
#'
#' Creates a [tinytable::tt()] table with fill-in-the-blank and multiple
#' choice inputs for checking descriptive statistics results. Requires the
#' **tinytable** and **webexercises** packages.
#'
#' @param vars Character vector. Variable names to include, in display order.
#' @param desc_results_list Output from [descriptives_answers()] (a list with
#'   `$Descriptives`). Also accepts a raw tibble from [univariate_stats_answers()]
#'   for backwards compatibility.
#' @param var_labels Named character vector or `NULL`. Optional display labels,
#'   e.g. `c(clark_height_in = "Clark Height")`.
#' @param label Character vector or `NULL`. Variables that are IDs/labels
#'   (mean is NOT interpretable).
#' @param quantitative Character vector or `NULL`. Continuous variables
#'   (mean IS interpretable).
#' @param binary Character vector or `NULL`. Dichotomous variables
#'   (mean IS interpretable).
#' @param multi_category Character vector or `NULL`. Nominal variables with
#'   3+ levels (mean is NOT interpretable).
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @seealso [descriptives_answers()], [univariate_stats_answers()]
#'
#' @examples
#' \dontrun{
#' data(superman, package = "psych350data")
#' library(dplyr)
#'
#' my_data <- superman |>
#'   select(year, clark_height_in, height_diff) |>
#'   filter(!is.na(height_diff))
#'
#' result <- descriptives_answers(my_data,
#'   vars = c("year", "clark_height_in", "height_diff"))
#' create_descriptives_checker(
#'   vars = c("year", "clark_height_in", "height_diff"),
#'   desc_results_list = result,
#'   quantitative = c("clark_height_in", "height_diff"),
#'   label = "year"
#' )
#' }
#'
#' @export
create_descriptives_checker <- function(vars,
                                        desc_results_list,
                                        var_labels = NULL,
                                        label = NULL,
                                        quantitative = NULL,
                                        binary = NULL,
                                        multi_category = NULL) {

  .check_packages()

  # Accept either a list with $Descriptives (from descriptives_answers()) or

  # a raw tibble (from univariate_stats_answers()) for backwards compatibility
  if (is.data.frame(desc_results_list)) {
    desc_stats <- desc_results_list
  } else {
    desc_stats <- desc_results_list$Descriptives
  }

  var_type_map <- c(
    stats::setNames(rep("label",          length(label)),          label),
    stats::setNames(rep("quantitative",   length(quantitative)),   quantitative),
    stats::setNames(rep("binary",         length(binary)),         binary),
    stats::setNames(rep("multi_category", length(multi_category)), multi_category)
  )

  answer_text <- c(
    label          = "No - this is a label, not a variable",
    quantitative   = "Yes - this is a quantitative variable",
    binary         = "Yes - this is a binary variable",
    multi_category = "No - this is a multiple-category variable"
  )

  all_options <- c(
    "No - this is a label, not a variable",
    "Yes - this is a quantitative variable",
    "Yes - this is a binary variable",
    "No - this is a multiple-category variable"
  )

  build_mcq <- function(correct_type) {
    correct <- answer_text[correct_type]
    opts <- all_options
    names(opts) <- ifelse(opts == correct, "answer", opts)
    webexercises::mcq(opts)
  }

  get_label <- function(v) {
    if (!is.null(var_labels) && v %in% names(var_labels)) {
      return(unname(var_labels[v]))
    }
    v
  }

  rows_list <- purrr::map(vars, \(v) {
    var_stats <- desc_stats |>
      dplyr::filter(.data$variable == v)

    if (nrow(var_stats) == 0) {
      stop(
        "Variable '", v, "' not found in desc_results_list$Descriptives. ",
        "Available variables: ", paste(desc_stats$variable, collapse = ", ")
      )
    }

    mean_raw <- dplyr::pull(var_stats, "mean")
    sd_raw   <- dplyr::pull(var_stats, "sd")
    sem_raw  <- dplyr::pull(var_stats, "sem")

    vtype <- if (v %in% names(var_type_map)) var_type_map[[v]] else "quantitative"

    tibble::tibble(
      Variable         = get_label(v),
      Mean             = webexercises::fitb(.format_stat(mean_raw, bounded = FALSE)),
      SD               = webexercises::fitb(.format_stat(sd_raw,   bounded = FALSE)),
      SEM              = webexercises::fitb(.format_stat(sem_raw,  bounded = FALSE)),
      `Interpretable?` = build_mcq(vtype)
    )
  })

  table_data <- dplyr::bind_rows(rows_list)

  table_data |>
    tinytable::tt() |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )
}


# =============================================================================
# CORRELATION CHECKER
# =============================================================================

#' Interactive Pearson Correlation Homework Checker
#'
#' Creates a [tinytable::tt()] table with fill-in-the-blank and multiple
#' choice inputs for checking Pearson correlation results and descriptive
#' statistics. Requires the **tinytable** and **webexercises** packages.
#'
#' @param rh_name Character. Research hypothesis label shown in the header.
#' @param vars Character vector of length 2. Variable names used as row
#'   labels for the descriptive-statistics rows.
#' @param corr_results_list Output from [corr_answers()]. Must contain
#'   `$Correlation` (with `r`, `p_value`, `df`) and `$Descriptives`
#'   (with columns `variable`, `mean`, `sd`, `n`).
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' data(superman, package = "psych350data")
#' result <- corr_answers(superman, "clark_height_in", "rt_critics_score")
#' create_corr_checker("RH1", c("Clark Height", "Critics Score"), result)
#' }
#'
#' @export
create_corr_checker <- function(rh_name, vars, corr_results_list) {

  .check_packages()

  desc_stats <- corr_results_list$Descriptives

  # Filter and extract scalar values using pull()
  var1_stats <- desc_stats |>
    dplyr::filter(.data$variable == vars[1])
  var2_stats <- desc_stats |>
    dplyr::filter(.data$variable == vars[2])

  # Extract scalars safely and format (2 decimals for descriptives)
  var1_mean <- .format_stat(dplyr::pull(var1_stats, "mean"), bounded = FALSE)
  var1_sd <- .format_stat(dplyr::pull(var1_stats, "sd"), bounded = FALSE)
  var1_n <- .safe_fitb_value(dplyr::pull(var1_stats, "n"))

  var2_mean <- .format_stat(dplyr::pull(var2_stats, "mean"), bounded = FALSE)
  var2_sd <- .format_stat(dplyr::pull(var2_stats, "sd"), bounded = FALSE)
  var2_n <- .safe_fitb_value(dplyr::pull(var2_stats, "n"))

  # Format correlation coefficient (2 decimals, no leading zero)
  r_value <- .format_stat(corr_results_list$Correlation$r, bounded = TRUE)

  # Format p-value (3 decimals, no leading zero)
  p_value <- corr_results_list$Correlation$p_value
  p_formatted <- .format_p_apa(p_value)

  # df stays as integer
  df_value <- corr_results_list$Correlation$df

  reject_retain_mcq <- .make_reject_retain_mcq(p_value)

  corr_table_data <- tibble::tibble(
    ` `  = paste("Correlation:", rh_name),
    r    = webexercises::fitb(r_value),
    p    = webexercises::fitb(p_formatted),
    df   = webexercises::fitb(df_value),
    `Reject or Retain?` = reject_retain_mcq
  )
  corr_table <- tinytable::tt(corr_table_data) |>
    tinytable::format_tt(escape = FALSE)

  desc_table1_data <- tibble::tibble(
    ` `    = paste("Variable 1:", vars[1]),
    Mean   = webexercises::fitb(var1_mean),
    SD     = webexercises::fitb(var1_sd),
    N      = webexercises::fitb(var1_n),
    `  `   = ""
  )
  desc_table1 <- tinytable::tt(desc_table1_data) |>
    tinytable::format_tt(escape = FALSE)

  desc_table2_data <- tibble::tibble(
    ` `    = paste("Variable 2:", vars[2]),
    Mean   = webexercises::fitb(var2_mean),
    SD     = webexercises::fitb(var2_sd),
    N      = webexercises::fitb(var2_n),
    `   `  = ""
  )
  desc_table2 <- tinytable::tt(desc_table2_data) |>
    tinytable::format_tt(escape = FALSE)

  tinytable::rbind2(corr_table, desc_table1, use_names = FALSE) |>
    tinytable::rbind2(desc_table2, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 80%; margin-left: auto; margin-right: auto;"
    )
}


# =============================================================================
# CHI-SQUARE CHECKER (2-group)
# =============================================================================

#' Interactive Chi-Square Homework Checker (2-Group)
#'
#' Creates a [tinytable::tt()] table with fill-in-the-blank and MCQ inputs
#' for checking a 2 x 2 chi-square test of independence.
#'
#' @param rh_name Character. Research hypothesis label.
#' @param chi_results_list Output from chi-square analysis function.
#' @param var1_labels Character vector of length 2. Labels for variable 1.
#' @param var2_labels Character vector of length 2. Labels for variable 2.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @export
create_chisq_checker <- function(rh_name, chi_results_list,
                                 var1_labels, var2_labels) {

  .check_packages()

  # Format chi-square (2 decimals, keep leading zero for unbounded stat)
  chi_sq <- .format_stat(chi_results_list$ChiSquare$chi_sq, bounded = FALSE)

  # Format p-value (3 decimals, no leading zero)
  p_value <- chi_results_list$ChiSquare$p_value
  p_formatted <- .format_p_apa(p_value)

  df <- chi_results_list$ChiSquare$df
  var1_desc <- chi_results_list$Var1_Descriptives
  var2_desc <- chi_results_list$Var2_Descriptives

  # Extract scalar values safely (counts stay as integers)
  var1_n1 <- .safe_fitb_value(var1_desc$n[1])
  var1_n2 <- .safe_fitb_value(var1_desc$n[2])
  var2_n1 <- .safe_fitb_value(var2_desc$n[1])
  var2_n2 <- .safe_fitb_value(var2_desc$n[2])

  reject_retain_mcq <- .make_reject_retain_mcq(p_value)

  chi_table_data <- tibble::tibble(
    ` `  = paste("Chi-Square:", rh_name),
    chi2 = webexercises::fitb(chi_sq),
    p    = webexercises::fitb(p_formatted),
    df   = webexercises::fitb(df),
    `Reject or Retain H0?` = reject_retain_mcq
  )
  names(chi_table_data)[2] <- "\u03C7\u00B2"
  chi_table <- tinytable::tt(chi_table_data) |>
    tinytable::format_tt(escape = FALSE)

  desc_table_data <- tibble::tibble(
    ` ` = c(
      paste("Number of", var1_labels[1], "in the sample"),
      paste("Number of", var1_labels[2], "in the sample")
    ),
    `n ` = c(
      webexercises::fitb(var1_n1),
      webexercises::fitb(var1_n2)
    ),
    `  ` = c(
      paste("Number of", var2_labels[1], "in the sample"),
      paste("Number of", var2_labels[2], "in the sample")
    ),
    `n  ` = c(
      webexercises::fitb(var2_n1),
      webexercises::fitb(var2_n2)
    ),
    `   ` = ""
  )
  desc_table <- tinytable::tt(desc_table_data) |>
    tinytable::format_tt(escape = FALSE)

  tinytable::rbind2(chi_table, desc_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 80%; margin-left: auto; margin-right: auto;"
    )
}


# =============================================================================
# K-GROUP CHI-SQUARE OMNIBUS CHECKER
# =============================================================================

#' Interactive Chi-Square Omnibus Homework Checker
#'
#' Creates a tinytable with embedded webexercises elements
#' for checking chi-square omnibus statistics and sample descriptives.
#'
#' @param rh_name Character. Research hypothesis label.
#' @param chisq_results_list Output from [chi_square_kgroup_answers()].
#' @param var1_labels Character vector or NULL. Labels for var1 levels.
#' @param var2_labels Character vector or NULL. Labels for var2 levels.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' result <- chi_square_kgroup_answers(data, "group", "outcome")
#' create_chisq_omnibus_table("RH1", result)
#' }
#'
#' @export
create_chisq_omnibus_table <- function(rh_name, chisq_results_list,
                                       var1_labels = NULL,
                                       var2_labels = NULL) {

  .check_packages()

  # Format chi-square (2 decimals)
  chi_sq <- .format_stat(chisq_results_list$ChiSquare$chi_sq, bounded = FALSE)

  # Format p-value (3 decimals, no leading zero)
  p_value <- chisq_results_list$ChiSquare$p_value
  p_formatted <- .format_p_apa(p_value)

  df <- chisq_results_list$ChiSquare$df
  total_n <- chisq_results_list$Sample_Size

  var1_desc <- chisq_results_list$Var1_Descriptives
  var2_desc <- chisq_results_list$Var2_Descriptives

  if (is.null(var1_labels)) var1_labels <- var1_desc$level_label
  if (is.null(var2_labels)) var2_labels <- var2_desc$level_label

  #use unicode to fix chr
  posthoc_mcq <- if (p_value < 0.05) {
    webexercises::mcq(c(
      paste0("No \u2013 a nonsignificant Omnibus ", .chi_sq_symbol(), " test"),
      answer = paste0("Yes \u2013 significant Omnibus ", .chi_sq_symbol(), " test")
    ))
  } else {
    webexercises::mcq(c(
      answer = paste0("No \u2013 a nonsignificant Omnibus ", .chi_sq_symbol(), " test"),
      paste0("Yes \u2013 significant Omnibus ", .chi_sq_symbol(), " test")
    ))
  }

  chisq_table_data <- tibble::tibble(
    ` ` = paste("Chi-Square:", rh_name),
    chi2 = webexercises::fitb(chi_sq),
    p = webexercises::fitb(p_formatted),
    df = webexercises::fitb(df),
    N = webexercises::fitb(total_n),
    `  ` = "",
    `Do we need to perform pairwise comparisons?` = posthoc_mcq
  )
  names(chisq_table_data)[2] <- .chi_sq_symbol()

  chisq_table <- tinytable::tt(chisq_table_data) |>
    tinytable::format_tt(escape = FALSE)

  n_var1 <- nrow(var1_desc)
  n_var2 <- nrow(var2_desc)

  # Extract values safely (counts stay as integers)
  var1_col1 <- purrr::map_chr(seq_len(n_var1), \(i) {
    paste("Number of", var1_labels[i], "in sample")
  })
  var1_col2 <- purrr::map_chr(seq_len(n_var1), \(i) {
    webexercises::fitb(.safe_fitb_value(var1_desc$n[i]))
  })
  var2_col1 <- purrr::map_chr(seq_len(n_var2), \(i) {
    paste("Number of", var2_labels[i], "in sample")
  })
  var2_col2 <- purrr::map_chr(seq_len(n_var2), \(i) {
    webexercises::fitb(.safe_fitb_value(var2_desc$n[i]))
  })

  desc_table_data <- tibble::tibble(
    ` ` = c(var1_col1[1], var2_col1[1]),
    `  ` = c(var1_col2[1], var2_col2[1]),
    `   ` = c(if (n_var1 >= 2) var1_col1[2] else "", if (n_var2 >= 2) var2_col1[2] else ""),
    `    ` = c(if (n_var1 >= 2) var1_col2[2] else "", if (n_var2 >= 2) var2_col2[2] else ""),
    `     ` = c(if (n_var1 >= 3) var1_col1[3] else "", ""),
    `      ` = c(if (n_var1 >= 3) var1_col2[3] else "", ""),
    `       ` = ""
  )

  desc_table <- tinytable::tt(desc_table_data) |>
    tinytable::format_tt(escape = FALSE)

  tinytable::rbind2(chisq_table, desc_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )
}


#' Interactive Chi-Square Pairwise Comparisons Homework Checker
#'
#' Creates a tinytable with embedded webexercises elements
#' for checking chi-square pairwise comparisons including percentages,
#' chi-square values, effect sizes, and power assessments.
#'
#' @param chisq_results_list Output from [chi_square_kgroup_answers()].
#' @param var1_labels Character vector or NULL. Labels for var1 levels.
#' @param var2_labels Character vector or NULL. Labels for var2 levels.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' result <- chi_square_kgroup_answers(data, "group", "outcome")
#' create_chisq_pairwise_table(result)
#' }
#'
#' @export
create_chisq_pairwise_table <- function(chisq_results_list,
                                        var1_labels = NULL,
                                        var2_labels = NULL) {

  .check_packages()

  pairwise <- chisq_results_list$Pairwise
  n_pairwise <- length(pairwise)

  if (n_pairwise == 0) stop("No pairwise comparisons found in results")

  pct_label <- chisq_results_list$pct_var2_label
  if (is.null(pct_label)) pct_label <- "comparison"

  if (!is.null(var1_labels)) {
    original_labels <- chisq_results_list$var1_labels
    for (i in seq_len(n_pairwise)) {
      comparison_name <- pairwise[[i]]$comparison
      if (!is.null(original_labels)) {
        for (j in seq_along(original_labels)) {
          comparison_name <- gsub(original_labels[j], var1_labels[j],
                                  comparison_name, fixed = TRUE)
        }
        pairwise[[i]]$comparison <- comparison_name
      }
    }
  }

  # Format chi critical (2 decimals)
  chi_crit <- .format_stat(chisq_results_list$ChiCrit, bounded = FALSE)

  # Using unicode function to get symbol
  chi_crit_data <- tibble::tibble(
    ` ` = paste0(.chi_sq_symbol(), " critical"),
    `  ` = webexercises::fitb(chi_crit),
    `   ` = "",
    `    ` = "",
    `     ` = "",
    `      ` = ""
  )

  chi_crit_table <- tinytable::tt(chi_crit_data) |>
    tinytable::format_tt(escape = FALSE)

  pct_column_name <- paste0("% ", pct_label)

  pairwise_table_data <- tibble::tibble(
    ` ` = purrr::map_chr(seq_len(n_pairwise), \(i) pairwise[[i]]$comparison),
    !!pct_column_name := purrr::map_chr(seq_len(n_pairwise), \(i) {
      paste0(
        webexercises::fitb(.format_stat(pairwise[[i]]$pct1, bounded = FALSE)), "% vs ",
        webexercises::fitb(.format_stat(pairwise[[i]]$pct2, bounded = FALSE)), "%"
      )
    }),
    chi2_result = purrr::map_chr(seq_len(n_pairwise), \(i) {
      paste0(
        webexercises::fitb(.format_stat(pairwise[[i]]$chi_sq, bounded = FALSE)), " ",
        webexercises::fitb(.safe_fitb_value(pairwise[[i]]$chi_result))
      )
    }),
    `Type of Error` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      is_sig <- pairwise[[i]]$chi_result != "="
      .make_error_type_mcq(is_sig)
    }),
    # Effect size is bounded (-1 to 1), so no leading zero
    `Effect Size (r)` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      webexercises::fitb(.format_stat(pairwise[[i]]$effect_size, bounded = TRUE))
    }),
    `Power Problem?` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      .make_power_mcq(pairwise[[i]]$power_problem)
    })
  )

  names(pairwise_table_data)[names(pairwise_table_data) == "chi2_result"] <- paste0(.chi_sq_symbol(), " Result")


  pairwise_table <- tinytable::tt(pairwise_table_data) |>
    tinytable::format_tt(escape = FALSE)

  tinytable::rbind2(chi_crit_table, pairwise_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 95%; margin-left: auto; margin-right: auto;"
    )
}


# =============================================================================
# CHI-SQUARE PAIRWISE CHECKER
# =============================================================================

#' Interactive Chi-Square Pairwise Comparisons Homework Checker
#'
#' Creates a [tinytable::tt()] table for checking chi-square pairwise
#' comparisons including percentages, chi-square values, effect sizes,
#' and power assessments.
#'
#' @param chisq_results_list Output from chi-square kgroup analysis.
#' @param var1_labels Character vector or `NULL`. Optional relabelling.
#' @param var2_labels Character vector or `NULL`. Optional relabelling.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @export
create_chisq_pairwise_checker <- function(chisq_results_list,
                                          var1_labels = NULL,
                                          var2_labels = NULL) {

  .check_packages()

  pairwise   <- chisq_results_list$Pairwise
  n_pairwise <- length(pairwise)
  if (n_pairwise == 0) stop("No pairwise comparisons found in results")

  pct_label <- chisq_results_list$pct_var2_label
  if (is.null(pct_label)) pct_label <- "comparison"

  # Relabel comparisons if needed
  if (!is.null(var1_labels)) {
    original_labels <- chisq_results_list$var1_labels
    for (i in seq_len(n_pairwise)) {
      cname <- pairwise[[i]]$comparison
      if (!is.null(original_labels)) {
        for (j in seq_along(original_labels)) {
          cname <- gsub(original_labels[j], var1_labels[j], cname, fixed = TRUE)
        }
        pairwise[[i]]$comparison <- cname
      }
    }
  }

  # Format chi critical (2 decimals)
  chi_crit <- .format_stat(chisq_results_list$ChiCrit, bounded = FALSE)

  chi_crit_data <- tibble::tibble(
    ` `   = "Chi-Square critical",
    `  `  = webexercises::fitb(chi_crit),
    `   ` = "", `    ` = "", `     ` = "", `      ` = ""
  )
  chi_crit_table <- tinytable::tt(chi_crit_data) |>
    tinytable::format_tt(escape = FALSE)

  pairwise_table_data <- tibble::tibble(
    ` ` = purrr::map_chr(seq_len(n_pairwise), \(i) pairwise[[i]]$comparison),
    `% comparison` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      paste0(
        webexercises::fitb(.format_stat(pairwise[[i]]$pct1, bounded = FALSE)), "% vs ",
        webexercises::fitb(.format_stat(pairwise[[i]]$pct2, bounded = FALSE)), "%"
      )
    }),
    chi2_result = purrr::map_chr(seq_len(n_pairwise), \(i) {
      paste0(
        webexercises::fitb(.format_stat(pairwise[[i]]$chi_sq, bounded = FALSE)), " ",
        webexercises::fitb(.safe_fitb_value(pairwise[[i]]$chi_result))
      )
    }),
    `Type of Error` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      is_sig <- pairwise[[i]]$chi_result != "="
      .make_error_type_mcq(is_sig)
    }),
    # Effect size bounded
    `Effect Size (r)` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      webexercises::fitb(.format_stat(pairwise[[i]]$effect_size, bounded = TRUE))
    }),
    `Power Problem?` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      .make_power_mcq(pairwise[[i]]$power_problem)
    })
  )
  names(pairwise_table_data)[3] <- "\u03C7\u00B2 Result"
  pairwise_table <- tinytable::tt(pairwise_table_data) |>
    tinytable::format_tt(escape = FALSE)

  tinytable::rbind2(chi_crit_table, pairwise_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 95%; margin-left: auto; margin-right: auto;"
    )
}


# =============================================================================
# BETWEEN-GROUPS ANOVA CHECKER (2-group)
# =============================================================================

#' Interactive Between-Groups ANOVA Homework Checker (2-Group)
#'
#' Creates a [tinytable::tt()] table for checking a 2-group BG ANOVA.
#' Includes ANOVA type identification, F-test statistics, and group
#' descriptives.
#'
#' @param rh_name Character. Research hypothesis label.
#' @param vars Character vector. Variable names (for reference).
#' @param anova_results_list Output from [bg_anova_answers()].
#' @param group_labels Character vector of length 2 or `NULL`. Display
#'   labels for the two groups. If `NULL`, uses factor levels from the data.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' data(superman, package = "psych350data")
#' result <- bg_anova_answers(superman, iv = "clark_grp",
#'   dv = "rt_critics_score")
#' create_bg_anova_checker("RH1",
#'   c("clark_grp", "rt_critics_score"), result,
#'   group_labels = c("Under 6ft", "6ft+"))
#' }
#'
#' @export
create_bg_anova_checker <- function(rh_name, vars, anova_results_list,
                                    group_labels = NULL) {

  .check_packages()

  anova_type_mcq <- webexercises::mcq(c(
    answer = "Between-Groups (BG)",
    "Within-Groups (WG)"
  ))

  # Format F statistic (2 decimals, unbounded)
  f_stat <- .format_stat(anova_results_list$ANOVA$F, bounded = FALSE)

  # Format p-value (3 decimals, no leading zero)
  p_value <- anova_results_list$ANOVA$p_value
  p_formatted <- .format_p_apa(p_value)

  df_between <- anova_results_list$ANOVA$df_between
  df_within  <- anova_results_list$ANOVA$df_within

  # Format MSE (2 decimals)
  mse <- .format_stat(anova_results_list$ANOVA$mse, bounded = FALSE)

  desc_stats <- anova_results_list$Descriptives
  if (is.null(group_labels)) group_labels <- as.character(desc_stats$iv)

  reject_retain_mcq <- .make_reject_retain_mcq(p_value)

  type_table_data <- tibble::tibble(
    ` `    = paste("ANOVA Type:", rh_name),
    `Type` = anova_type_mcq,
    `  ` = "", `   ` = "", `    ` = "", `     ` = ""
  )
  type_table <- tinytable::tt(type_table_data) |>
    tinytable::format_tt(escape = FALSE)

  anova_table_data <- tibble::tibble(
    ` `           = paste("BG ANOVA:", rh_name),
    F             = webexercises::fitb(f_stat),
    p             = webexercises::fitb(p_formatted),
    `df(between)` = webexercises::fitb(df_between),
    `df(within)`  = webexercises::fitb(df_within),
    MSE           = webexercises::fitb(mse)
  )
  anova_table <- tinytable::tt(anova_table_data) |>
    tinytable::format_tt(escape = FALSE)

  decision_table_data <- tibble::tibble(
    ` ` = "Decision:",
    `Reject or Retain H0?` = reject_retain_mcq,
    `  ` = "", `   ` = "", `    ` = "", `     ` = ""
  )
  decision_table <- tinytable::tt(decision_table_data) |>
    tinytable::format_tt(escape = FALSE)

  # Extract scalar values safely for descriptives (format to 2 decimals)
  desc_table_data <- tibble::tibble(
    ` `  = group_labels,
    Mean = purrr::map_chr(seq_along(group_labels), \(i) {
      webexercises::fitb(.format_stat(desc_stats$mean[i], bounded = FALSE))
    }),
    SD   = purrr::map_chr(seq_along(group_labels), \(i) {
      webexercises::fitb(.format_stat(desc_stats$sd[i], bounded = FALSE))
    }),
    n    = purrr::map_chr(seq_along(group_labels), \(i) {
      webexercises::fitb(.safe_fitb_value(desc_stats$n[i]))
    }),
    `  ` = rep("", length(group_labels)),
    `   ` = rep("", length(group_labels))
  )
  desc_table <- tinytable::tt(desc_table_data) |>
    tinytable::format_tt(escape = FALSE)

  tinytable::rbind2(type_table, anova_table, use_names = FALSE) |>
    tinytable::rbind2(decision_table, use_names = FALSE) |>
    tinytable::rbind2(desc_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 85%; margin-left: auto; margin-right: auto;"
    )
}


# =============================================================================
# WITHIN-GROUPS ANOVA CHECKER (2-condition)
# =============================================================================

#' Interactive Within-Groups ANOVA Homework Checker (2-Condition)
#'
#' Creates a [tinytable::tt()] table for checking a 2-condition WG ANOVA.
#' Includes ANOVA type identification, F-test statistics, and condition
#' descriptives.
#'
#' @param rh_name Character. Research hypothesis label.
#' @param vars Character vector. Variable names (for reference).
#' @param anova_results_list Output from [wg_anova_answers()]. Must contain
#'   `$ANOVA` (with `F`, `p_value`, `df_effect`, `df_error`, `mse`) and
#'   `$Descriptives`.
#' @param condition_labels Character vector of length 2 or `NULL`. Display
#'   labels for the two conditions.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' data(superman, package = "psych350data")
#' result <- wg_anova_answers(superman,
#'   dv1 = "rt_critics_score", dv2 = "rt_audience_score")
#' create_wg_anova_checker("RH1",
#'   c("rt_critics_score", "rt_audience_score"), result,
#'   condition_labels = c("Critics", "Audience"))
#' }
#'
#' @export
create_wg_anova_checker <- function(rh_name, vars, anova_results_list,
                                    condition_labels = NULL) {

  .check_packages()

  anova_type_mcq <- webexercises::mcq(c(
    "Between-Groups (BG)",
    answer = "Within-Groups (WG)"
  ))

  f_stat      <- .safe_format(anova_results_list$ANOVA$F, "stat")
  p_value     <- anova_results_list$ANOVA$p_value
  p_formatted <- .format_p_apa(p_value)
  df_effect   <- .safe_format(anova_results_list$ANOVA$df_effect, "int")
  df_error    <- .safe_format(anova_results_list$ANOVA$df_error, "int")
  mse         <- .safe_format(anova_results_list$ANOVA$mse, "stat")

  desc_stats <- anova_results_list$Descriptives
  if (is.null(condition_labels)) {
    condition_labels <- as.character(desc_stats$condition_label)  # FIXED
  }

  reject_retain_mcq <- .make_reject_retain_mcq(p_value)

  type_table_data <- tibble::tibble(
    ` `    = paste("ANOVA Type:", rh_name),
    `Type` = anova_type_mcq,
    `  ` = "", `   ` = "", `    ` = "", `     ` = ""
  )
  type_table <- tinytable::tt(type_table_data) |>
    tinytable::format_tt(escape = FALSE)

  anova_table_data <- tibble::tibble(
    ` `          = paste("WG ANOVA:", rh_name),
    F            = webexercises::fitb(f_stat),
    p            = webexercises::fitb(p_formatted),
    `df(effect)` = webexercises::fitb(df_effect),
    `df(error)`  = webexercises::fitb(df_error),
    MSE          = webexercises::fitb(mse)
  )
  anova_table <- tinytable::tt(anova_table_data) |>
    tinytable::format_tt(escape = FALSE)

  decision_table_data <- tibble::tibble(
    ` ` = "Decision:",
    `Reject or Retain H0?` = reject_retain_mcq,
    `  ` = "", `   ` = "", `    ` = "", `     ` = ""
  )
  decision_table <- tinytable::tt(decision_table_data) |>
    tinytable::format_tt(escape = FALSE)

  desc_table_data <- tibble::tibble(
    ` `  = condition_labels,
    Mean = purrr::map_chr(seq_along(condition_labels), \(i) {
      webexercises::fitb(.safe_format(desc_stats$mean[i], "stat"))
    }),
    SD   = purrr::map_chr(seq_along(condition_labels), \(i) {
      webexercises::fitb(.safe_format(desc_stats$sd[i], "stat"))
    }),
    n    = purrr::map_chr(seq_along(condition_labels), \(i) {
      webexercises::fitb(.safe_format(desc_stats$n[i], "int"))
    }),
    `  ` = rep("", length(condition_labels)),
    `   ` = rep("", length(condition_labels))
  )
  desc_table <- tinytable::tt(desc_table_data) |>
    tinytable::format_tt(escape = FALSE)

  tinytable::rbind2(type_table, anova_table, use_names = FALSE) |>
    tinytable::rbind2(decision_table, use_names = FALSE) |>
    tinytable::rbind2(desc_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 85%; margin-left: auto; margin-right: auto;"
    )
}


# =============================================================================
# K-GROUP ANOVA OMNIBUS CHECKER
# =============================================================================

#' Interactive k-Group ANOVA Omnibus Homework Checker
#'
#' Creates a [tinytable::tt()] table for checking omnibus F-test statistics
#' and group descriptives for a k-group one way between-subjects ANOVA.
#'
#' @param rh_name Character. Research hypothesis label.
#' @param anova_results_list Output from `anova_kgroup_answers()`. Must
#'   contain `$ANOVA` (with `F`, `p_value`, `df_between`, `df_within`, `mse`,
#'   `total_n`, `k`, `mean_n`) and `$Descriptives` (with `group_label`,
#'   `mean`, `sd`, `n`).
#' @param group_labels Character vector or `NULL`. Display labels for groups.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' result <- anova_kgroup_answers(data, dv = "sentence",
#'   iv = "attract",
#'   group_labels = c("Beautiful", "Average", "Unattractive"))
#' create_anova_omnibus_checker("RH1", result)
#' }
#'
#' @export
create_anova_omnibus_checker <- function(rh_name, anova_results_list,
                                         group_labels = NULL) {

  .check_packages()

  # Format all statistics properly
  f_stat     <- .safe_format(anova_results_list$ANOVA$F, "stat")
  p_value    <- anova_results_list$ANOVA$p_value
  p_formatted <- .format_p_apa(p_value)
  df_between <- .safe_format(anova_results_list$ANOVA$df_between, "int")
  df_within  <- .safe_format(anova_results_list$ANOVA$df_within, "int")
  mse        <- .safe_format(anova_results_list$ANOVA$mse, "stat")
  total_n    <- .safe_format(anova_results_list$ANOVA$total_n, "int")
  k          <- .safe_format(anova_results_list$ANOVA$k, "int")
  mean_n     <- .safe_format(anova_results_list$ANOVA$mean_n, "stat")

  desc_stats <- anova_results_list$Descriptives
  n_groups   <- nrow(desc_stats)
  if (is.null(group_labels)) group_labels <- desc_stats$group_label

  posthoc_mcq <- .make_posthoc_mcq(p_value, "omnibus_f")

  anova_table_data <- tibble::tibble(
    ` `            = paste("BG ANOVA:", rh_name),
    F              = webexercises::fitb(f_stat),
    p              = webexercises::fitb(p_formatted),
    `df (between)` = webexercises::fitb(df_between),
    `df (within)`  = webexercises::fitb(df_within),
    MSE            = webexercises::fitb(mse),
    `Do we need to perform LSD pairwise comparisons?` = posthoc_mcq
  )
  anova_table <- tinytable::tt(anova_table_data) |>
    tinytable::format_tt(escape = FALSE)

  sample_table_data <- tibble::tibble(
    ` `         = "",
    `N`         = webexercises::fitb(total_n),
    k           = webexercises::fitb(k),
    `average n` = webexercises::fitb(mean_n),
    `  ` = "", `   ` = "", `    ` = ""
  )
  sample_table <- tinytable::tt(sample_table_data) |>
    tinytable::format_tt(escape = FALSE)

  # Descriptives: means/SDs to 2 decimals, n as integer
  desc_table_data <- tibble::tibble(
    ` `  = group_labels,
    Mean = purrr::map_chr(seq_len(n_groups), \(i) {
      webexercises::fitb(.safe_format(desc_stats$mean[i], "stat"))
    }),
    SD   = purrr::map_chr(seq_len(n_groups), \(i) {
      webexercises::fitb(.safe_format(desc_stats$sd[i], "stat"))
    }),
    n    = purrr::map_chr(seq_len(n_groups), \(i) {
      webexercises::fitb(.safe_format(desc_stats$n[i], "int"))
    }),
    `  ` = rep("", n_groups),
    `   ` = rep("", n_groups),
    `    ` = rep("", n_groups)
  )
  desc_table <- tinytable::tt(desc_table_data) |>
    tinytable::format_tt(escape = FALSE)

  tinytable::rbind2(anova_table, sample_table, use_names = FALSE) |>
    tinytable::rbind2(desc_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )
}


# =============================================================================
# K-GROUP ANOVA LSD PAIRWISE CHECKER
# =============================================================================

#' Interactive LSD Pairwise Comparisons Homework Checker
#'
#' Creates a tinytable with embedded webexercises elements
#' for checking LSD pairwise comparison results.
#'
#' @param anova_results_list Output from [anova_kgroup_answers()].
#' @param group_labels Character vector or NULL. Display labels for groups.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' result <- anova_kgroup_answers(data, "dv", "iv")
#' create_lsd_pairwise_table(result)
#' }
#'
#' @export
create_lsd_pairwise_checker <- function(anova_results_list,
                                        group_labels = NULL) {

  .check_packages()

  # Format LSD (2 decimals)
  lsd_mmd    <- .safe_format(anova_results_list$LSD$lsd_mmd, "stat")
  pairwise   <- anova_results_list$Pairwise
  n_pairwise <- length(pairwise)

  # Relabel comparisons if needed
  if (!is.null(group_labels)) {
    original_labels <- anova_results_list$group_labels
    for (i in seq_len(n_pairwise)) {
      cname <- pairwise[[i]]$comparison
      if (!is.null(original_labels)) {
        for (j in seq_along(original_labels)) {
          cname <- gsub(original_labels[j], group_labels[j], cname, fixed = TRUE)
        }
        pairwise[[i]]$comparison <- cname
      }
    }
  }

  lsd_mmd_data <- tibble::tibble(
    ` `   = "LSDmmd",
    `  `  = webexercises::fitb(lsd_mmd),
    `   ` = "", `    ` = "", `     ` = "", `      ` = ""
  )
  lsd_mmd_table <- tinytable::tt(lsd_mmd_data) |>
    tinytable::format_tt(escape = FALSE)

  pairwise_table_data <- tibble::tibble(
    ` ` = purrr::map_chr(seq_len(n_pairwise), \(i) pairwise[[i]]$comparison),
    `Mean Difference` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      webexercises::fitb(.safe_format(pairwise[[i]]$mean_diff, "stat"))
    }),
    `LSD Result` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      webexercises::fitb(pairwise[[i]]$lsd_result)  # This is a character: "<", ">", "="
    }),
    `Type of Error` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      is_sig <- pairwise[[i]]$lsd_result != "="
      .make_error_type_mcq(is_sig)
    }),
    `Effect Size (r)` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      webexercises::fitb(.safe_format(pairwise[[i]]$effect_size, "bounded"))
    }),
    `Power Problem?` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      .make_power_mcq(pairwise[[i]]$power_problem)
    })
  )
  pairwise_table <- tinytable::tt(pairwise_table_data) |>
    tinytable::format_tt(escape = FALSE)

  tinytable::rbind2(lsd_mmd_table, pairwise_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 95%; margin-left: auto; margin-right: auto;"
    )
}


# =============================================================================
# FACTORIAL ANOVA CHECKER
# =============================================================================

#' Interactive Factorial ANOVA Statistics Homework Checker
#'
#' Creates a [tinytable::tt()] table for checking interaction and main
#' effect F-tests plus LSD computation components for a 2 x 2 factorial
#' ANOVA.
#'
#' @param rh_name Character. Research hypothesis label.
#' @param anova_results_list Output from [anova_factorial_answers()].
#' @param iv1_name Character. Display name for IV1.
#' @param iv2_name Character. Display name for IV2.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' result <- anova_factorial_answers(data, dv = "dv",
#'   iv1 = "iv1", iv2 = "iv2",
#'   iv1_labels = c("Low", "High"),
#'   iv2_labels = c("Control", "Treatment"))
#' create_factbg_anova_checker("RH1", result,
#'   iv1_name = "Anxiety", iv2_name = "Condition")
#' }
#'
#' @export
create_factbg_anova_checker <- function(rh_name, anova_results_list,
                                        iv1_name = "IV1",
                                        iv2_name = "IV2") {

  .check_packages()

  # Format all statistics properly
  f_iv1          <- .safe_format(anova_results_list$ANOVA$MainEffect_IV1$F, "stat")
  p_iv1          <- anova_results_list$ANOVA$MainEffect_IV1$p_value
  p_iv1_fmt      <- .format_p_apa(p_iv1)
  df_iv1         <- .safe_format(anova_results_list$ANOVA$MainEffect_IV1$df, "int")

  f_iv2          <- .safe_format(anova_results_list$ANOVA$MainEffect_IV2$F, "stat")
  p_iv2          <- anova_results_list$ANOVA$MainEffect_IV2$p_value
  p_iv2_fmt      <- .format_p_apa(p_iv2)
  df_iv2         <- .safe_format(anova_results_list$ANOVA$MainEffect_IV2$df, "int")

  f_interaction  <- .safe_format(anova_results_list$ANOVA$Interaction$F, "stat")
  p_interaction  <- anova_results_list$ANOVA$Interaction$p_value
  p_interaction_fmt <- .format_p_apa(p_interaction)
  df_interaction <- .safe_format(anova_results_list$ANOVA$Interaction$df, "int")

  df_within      <- .safe_format(anova_results_list$ANOVA$df_within, "int")
  mse            <- .safe_format(anova_results_list$ANOVA$mse, "stat")
  k              <- .safe_format(anova_results_list$ANOVA$k, "int")
  mean_n         <- .safe_format(anova_results_list$ANOVA$mean_n, "stat")
  lsd_mmd        <- .safe_format(anova_results_list$LSD$lsd_mmd, "stat")

  posthoc_mcq <- .make_posthoc_mcq(p_interaction, "interaction")

  interaction_data <- tibble::tibble(
    ` `            = paste("Interaction:", iv1_name, "x", iv2_name),
    F              = webexercises::fitb(f_interaction),
    p              = webexercises::fitb(p_interaction_fmt),
    `df (between)` = webexercises::fitb(df_interaction),
    `df (within)`  = webexercises::fitb(df_within),
    MSE            = webexercises::fitb(mse),
    `Do we need to perform LSD pairwise comparisons?` = posthoc_mcq
  )
  interaction_table <- tinytable::tt(interaction_data) |>
    tinytable::format_tt(escape = FALSE)

  lsd_data <- tibble::tibble(
    ` `               = "Components for LSDmmd:",
    `# of conditions` = webexercises::fitb(k),
    `average n`       = webexercises::fitb(mean_n),
    `df error`        = webexercises::fitb(df_within),
    `MSe`             = webexercises::fitb(mse),
    `LSDmmd`          = webexercises::fitb(lsd_mmd),
    `   ` = ""
  )
  lsd_table <- tinytable::tt(lsd_data) |>
    tinytable::format_tt(escape = FALSE)

  iv1_data <- tibble::tibble(
    ` `            = paste("Main Effect:", iv1_name),
    F              = webexercises::fitb(f_iv1),
    p              = webexercises::fitb(p_iv1_fmt),
    `df (between)` = webexercises::fitb(df_iv1),
    `df (within)`  = webexercises::fitb(df_within),
    MSE            = webexercises::fitb(mse),
    `  ` = ""
  )
  iv1_table <- tinytable::tt(iv1_data) |>
    tinytable::format_tt(escape = FALSE)

  iv2_data <- tibble::tibble(
    ` `            = paste("Main Effect:", iv2_name),
    F              = webexercises::fitb(f_iv2),
    p              = webexercises::fitb(p_iv2_fmt),
    `df (between)` = webexercises::fitb(df_iv2),
    `df (within)`  = webexercises::fitb(df_within),
    MSE            = webexercises::fitb(mse),
    `   ` = ""
  )
  iv2_table <- tinytable::tt(iv2_data) |>
    tinytable::format_tt(escape = FALSE)

  tinytable::rbind2(interaction_table, lsd_table, use_names = FALSE) |>
    tinytable::rbind2(iv1_table, use_names = FALSE) |>
    tinytable::rbind2(iv2_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )
}


# =============================================================================
# FACTORIAL ANOVA DESCRIPTIVES CHECKER
# =============================================================================

#' Interactive Factorial ANOVA Descriptives Homework Checker
#'
#' Creates a [tinytable::tt()] grid showing cell means and estimated
#' marginal means (EMMs) for a 2 x 2 factorial design with
#' fill-in-the-blank inputs.
#'
#' @param anova_results_list Output from [anova_factorial_answers()].
#'   Must include `$Descriptives`, `$EMMs`, and `$FactorLevels`.
#' @param iv1_name Character. Display name for IV1.
#' @param iv2_name Character. Display name for IV2.
#' @param iv1_labels Character vector or `NULL`. Overrides default IV1 labels.
#' @param iv2_labels Character vector or `NULL`. Overrides default IV2 labels.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @export
create_factbg_desc_checker <- function(anova_results_list,
                                       iv1_name = "IV1",
                                       iv2_name = "IV2",
                                       iv1_labels = NULL,
                                       iv2_labels = NULL) {

  .check_packages()

  desc_stats <- anova_results_list$Descriptives
  emm_iv1    <- anova_results_list$EMMs$IV1
  emm_iv2    <- anova_results_list$EMMs$IV2

  if (!is.null(anova_results_list$FactorLevels)) {
    iv1_levels_actual <- anova_results_list$FactorLevels$iv1_levels
    iv2_levels_actual <- anova_results_list$FactorLevels$iv2_levels
    final_iv1_labels  <- if (!is.null(iv1_labels)) iv1_labels else anova_results_list$FactorLevels$iv1_labels
    final_iv2_labels  <- if (!is.null(iv2_labels)) iv2_labels else anova_results_list$FactorLevels$iv2_labels
  } else {
    stop("FactorLevels not found. Please re-run anova_factorial_answers().")
  }

  # Get cell means for each combination - format to 2 decimals
  cell_data <- tibble::tibble(
    ` ` = final_iv1_labels,
    col1 = purrr::map_chr(seq_along(final_iv1_labels), \(i) {
      cell <- desc_stats |>
        dplyr::filter(.data$iv1_level == iv1_levels_actual[i],
                      .data$iv2_level == iv2_levels_actual[1])
      webexercises::fitb(.safe_format(dplyr::pull(cell, "mean"), "stat"))
    }),
    col2 = purrr::map_chr(seq_along(final_iv1_labels), \(i) {
      cell <- desc_stats |>
        dplyr::filter(.data$iv1_level == iv1_levels_actual[i],
                      .data$iv2_level == iv2_levels_actual[2])
      webexercises::fitb(.safe_format(dplyr::pull(cell, "mean"), "stat"))
    }),
    `EMM` = purrr::map_chr(seq_along(final_iv1_labels), \(i) {
      webexercises::fitb(.safe_format(emm_iv1$mean[i], "stat"))
    })
  )
  names(cell_data)[2:3] <- final_iv2_labels

  # Add EMM row for IV2
  emm_row <- tibble::tibble(
    ` ` = "EMM",
    col1 = webexercises::fitb(.safe_format(emm_iv2$mean[1], "stat")),
    col2 = webexercises::fitb(.safe_format(emm_iv2$mean[2], "stat")),
    `EMM` = ""
  )
  names(emm_row)[2:3] <- final_iv2_labels

  full_data <- dplyr::bind_rows(cell_data, emm_row)

  tinytable::tt(full_data) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-bordered table-sm",
      bootstrap_css_rule = "width: 70%; margin-left: auto; margin-right: auto;"
    )
}

# =============================================================================
# MIXED FACTORIAL ANOVA CHECKERS (BG × WG)
# =============================================================================

#' Interactive Mixed Factorial ANOVA Homework Checker
#'
#' Creates a [tinytable::tt()] table for checking interaction, within-groups
#' main effect, and between-groups main effect F-tests for a mixed (BG × WG)
#' factorial ANOVA. Mixed designs have separate error terms for WS and BS
#' effects.
#'
#' @param rh_name Character. Research hypothesis label.
#' @param anova_results_list Output from [anova_factmg_answers()].
#' @param bg_name Character. Display name for the between-groups IV.
#' @param wg_name Character. Display name for the within-groups IV.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @export
create_factmg_anova_checker <- function(rh_name, anova_results_list,
                                        bg_name = "BG IV",
                                        wg_name = "WG IV") {
  .check_packages()

  # Interaction (uses WS error)
  f_ix   <- .safe_format(anova_results_list$WithinSubjects$Interaction$F, "stat")
  p_ix   <- anova_results_list$WithinSubjects$Interaction$p_value
  p_ix_fmt <- .format_p_apa(p_ix)
  df_ix  <- .safe_format(anova_results_list$WithinSubjects$Interaction$df, "int")
  df_ws_err <- .safe_format(anova_results_list$WithinSubjects$Error$df, "int")
  ms_ws_err <- .safe_format(anova_results_list$WithinSubjects$Error$ms, "stat")

  # WG main effect (uses WS error)
  f_wg   <- .safe_format(anova_results_list$WithinSubjects$MainEffect_WG$F, "stat")
  p_wg   <- anova_results_list$WithinSubjects$MainEffect_WG$p_value
  p_wg_fmt <- .format_p_apa(p_wg)
  df_wg  <- .safe_format(anova_results_list$WithinSubjects$MainEffect_WG$df, "int")

  # BG main effect (uses BS error)
  f_bg   <- .safe_format(anova_results_list$BetweenSubjects$MainEffect_BG$F, "stat")
  p_bg   <- anova_results_list$BetweenSubjects$MainEffect_BG$p_value
  p_bg_fmt <- .format_p_apa(p_bg)
  df_bg  <- .safe_format(anova_results_list$BetweenSubjects$MainEffect_BG$df, "int")
  df_bs_err <- .safe_format(anova_results_list$BetweenSubjects$Error$df, "int")
  ms_bs_err <- .safe_format(anova_results_list$BetweenSubjects$Error$ms, "stat")

  # Build tables
  ix_data <- tibble::tibble(
    ` `           = paste("Interaction:", bg_name, "\u00D7", wg_name),
    F             = webexercises::fitb(f_ix),
    p             = webexercises::fitb(p_ix_fmt),
    `df (effect)` = webexercises::fitb(df_ix),
    `df (error)`  = webexercises::fitb(df_ws_err),
    MSE           = webexercises::fitb(ms_ws_err)
  )
  ix_table <- tinytable::tt(ix_data) |> tinytable::format_tt(escape = FALSE)

  wg_data <- tibble::tibble(
    ` `           = paste("WG Main Effect:", wg_name),
    F             = webexercises::fitb(f_wg),
    p             = webexercises::fitb(p_wg_fmt),
    `df (effect)` = webexercises::fitb(df_wg),
    `df (error)`  = webexercises::fitb(df_ws_err),
    MSE           = webexercises::fitb(ms_ws_err)
  )
  wg_table <- tinytable::tt(wg_data) |> tinytable::format_tt(escape = FALSE)

  bg_data <- tibble::tibble(
    ` `           = paste("BG Main Effect:", bg_name),
    F             = webexercises::fitb(f_bg),
    p             = webexercises::fitb(p_bg_fmt),
    `df (effect)` = webexercises::fitb(df_bg),
    `df (error)`  = webexercises::fitb(df_bs_err),
    MSE           = webexercises::fitb(ms_bs_err)
  )
  bg_table <- tinytable::tt(bg_data) |> tinytable::format_tt(escape = FALSE)

  tinytable::rbind2(ix_table, wg_table, use_names = FALSE) |>
    tinytable::rbind2(bg_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )
}

#' Interactive Mixed Factorial Descriptives Homework Checker
#'
#' Creates a [tinytable::tt()] grid showing cell means and estimated
#' marginal means (EMMs) for a mixed (BG × WG) factorial design with
#' fill-in-the-blank inputs.
#'
#' @param anova_results_list Output from [anova_factmg_answers()].
#' @param bg_name Character. Display name for the BG IV.
#' @param wg_name Character. Display name for the WG IV.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @export
create_factmg_desc_checker <- function(anova_results_list,
                                       bg_name = "BG IV",
                                       wg_name = "WG IV") {
  .check_packages()

  desc_stats <- anova_results_list$Descriptives
  emm_bg     <- anova_results_list$EMMs$BG
  emm_wg     <- anova_results_list$EMMs$WG
  info       <- anova_results_list$FactorInfo

  bg_levels <- info$bg_levels
  bg_labels <- info$bg_labels
  wg_labels <- info$wg_labels

  # Build cell means grid: BG levels as rows, WG levels as columns
  cell_data <- tibble::tibble(
    ` ` = bg_labels
  )

  for (j in seq_along(wg_labels)) {
    col_vals <- purrr::map_chr(seq_along(bg_levels), \(i) {
      cell <- desc_stats[desc_stats$bg_level == bg_levels[i] &
                           desc_stats$wg_level == wg_labels[j], ]
      webexercises::fitb(.safe_format(cell$mean[1], "stat"))
    })
    cell_data[[wg_labels[j]]] <- col_vals
  }

  # EMM column for BG
  cell_data[["EMM"]] <- purrr::map_chr(seq_along(bg_levels), \(i) {
    webexercises::fitb(.safe_format(emm_bg$mean[i], "stat"))
  })

  # EMM row for WG
  emm_row <- tibble::tibble(` ` = "EMM")
  for (j in seq_along(wg_labels)) {
    emm_row[[wg_labels[j]]] <- webexercises::fitb(.safe_format(emm_wg$mean[j], "stat"))
  }
  emm_row[["EMM"]] <- ""

  full_data <- dplyr::bind_rows(cell_data, emm_row)

  tinytable::tt(full_data) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-bordered table-sm",
      bootstrap_css_rule = "width: 70%; margin-left: auto; margin-right: auto;"
    )
}

# =============================================================================
# WITHIN-GROUPS FACTORIAL ANOVA CHECKERS
# =============================================================================

#' Interactive Within-Groups Factorial ANOVA Homework Checker
#'
#' Creates a [tinytable::tt()] table for checking interaction and main
#' effect F-tests for a within-groups factorial ANOVA. Each effect has
#' its own error term (separate residuals).
#'
#' @param rh_name Character. Research hypothesis label.
#' @param anova_results_list Output from [anova_factwg_answers()].
#' @param iv1_name Character. Display name for IV1.
#' @param iv2_name Character. Display name for IV2.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @export
create_factwg_anova_checker <- function(rh_name, anova_results_list,
                                        iv1_name = "IV1",
                                        iv2_name = "IV2") {
  .check_packages()

  # Interaction
  f_ix    <- .safe_format(anova_results_list$ANOVA$Interaction$F, "stat")
  p_ix    <- anova_results_list$ANOVA$Interaction$p_value
  p_ix_fmt <- .format_p_apa(p_ix)
  df_ix   <- .safe_format(anova_results_list$ANOVA$Interaction$df, "int")
  df_err_ix <- .safe_format(anova_results_list$ANOVA$Error_Interaction$df, "int")
  ms_err_ix <- .safe_format(anova_results_list$ANOVA$Error_Interaction$ms, "stat")

  # IV1 main effect
  f_iv1    <- .safe_format(anova_results_list$ANOVA$MainEffect_IV1$F, "stat")
  p_iv1    <- anova_results_list$ANOVA$MainEffect_IV1$p_value
  p_iv1_fmt <- .format_p_apa(p_iv1)
  df_iv1   <- .safe_format(anova_results_list$ANOVA$MainEffect_IV1$df, "int")
  df_err_iv1 <- .safe_format(anova_results_list$ANOVA$Error_IV1$df, "int")
  ms_err_iv1 <- .safe_format(anova_results_list$ANOVA$Error_IV1$ms, "stat")

  # IV2 main effect
  f_iv2    <- .safe_format(anova_results_list$ANOVA$MainEffect_IV2$F, "stat")
  p_iv2    <- anova_results_list$ANOVA$MainEffect_IV2$p_value
  p_iv2_fmt <- .format_p_apa(p_iv2)
  df_iv2   <- .safe_format(anova_results_list$ANOVA$MainEffect_IV2$df, "int")
  df_err_iv2 <- .safe_format(anova_results_list$ANOVA$Error_IV2$df, "int")
  ms_err_iv2 <- .safe_format(anova_results_list$ANOVA$Error_IV2$ms, "stat")

  # Build tables — each effect row has its own df(error) and MSE
  ix_data <- tibble::tibble(
    ` `           = paste("Interaction:", iv1_name, "\u00D7", iv2_name),
    F             = webexercises::fitb(f_ix),
    p             = webexercises::fitb(p_ix_fmt),
    `df (effect)` = webexercises::fitb(df_ix),
    `df (error)`  = webexercises::fitb(df_err_ix),
    MSE           = webexercises::fitb(ms_err_ix)
  )
  ix_table <- tinytable::tt(ix_data) |> tinytable::format_tt(escape = FALSE)

  iv1_data <- tibble::tibble(
    ` `           = paste("Main Effect:", iv1_name),
    F             = webexercises::fitb(f_iv1),
    p             = webexercises::fitb(p_iv1_fmt),
    `df (effect)` = webexercises::fitb(df_iv1),
    `df (error)`  = webexercises::fitb(df_err_iv1),
    MSE           = webexercises::fitb(ms_err_iv1)
  )
  iv1_table <- tinytable::tt(iv1_data) |> tinytable::format_tt(escape = FALSE)

  iv2_data <- tibble::tibble(
    ` `           = paste("Main Effect:", iv2_name),
    F             = webexercises::fitb(f_iv2),
    p             = webexercises::fitb(p_iv2_fmt),
    `df (effect)` = webexercises::fitb(df_iv2),
    `df (error)`  = webexercises::fitb(df_err_iv2),
    MSE           = webexercises::fitb(ms_err_iv2)
  )
  iv2_table <- tinytable::tt(iv2_data) |> tinytable::format_tt(escape = FALSE)

  tinytable::rbind2(ix_table, iv1_table, use_names = FALSE) |>
    tinytable::rbind2(iv2_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )
}

#' Interactive Within-Groups Factorial Descriptives Homework Checker
#'
#' Creates a [tinytable::tt()] grid showing cell means and estimated
#' marginal means (EMMs) for a within-groups factorial design with
#' fill-in-the-blank inputs. Supports arbitrary k × j designs.
#'
#' @param anova_results_list Output from [anova_factwg_answers()].
#' @param iv1_name Character. Display name for IV1 (rows).
#' @param iv2_name Character. Display name for IV2 (columns).
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @export
create_factwg_desc_checker <- function(anova_results_list,
                                       iv1_name = "IV1",
                                       iv2_name = "IV2") {
  .check_packages()

  desc_stats <- anova_results_list$Descriptives
  emm_iv1    <- anova_results_list$EMMs$IV1
  emm_iv2    <- anova_results_list$EMMs$IV2
  info       <- anova_results_list$FactorInfo

  iv1_labels <- info$iv1_labels
  iv2_labels <- info$iv2_labels

  # Build cell means grid: IV1 levels as rows, IV2 levels as columns
  cell_data <- tibble::tibble(` ` = iv1_labels)

  for (j in seq_along(iv2_labels)) {
    col_vals <- purrr::map_chr(seq_along(iv1_labels), \(i) {
      cell <- desc_stats[desc_stats$iv1_level == iv1_labels[i] &
                           desc_stats$iv2_level == iv2_labels[j], ]
      webexercises::fitb(.safe_format(cell$mean[1], "stat"))
    })
    cell_data[[iv2_labels[j]]] <- col_vals
  }

  # EMM column for IV1
  cell_data[["EMM"]] <- purrr::map_chr(seq_along(iv1_labels), \(i) {
    webexercises::fitb(.safe_format(emm_iv1$mean[i], "stat"))
  })

  # EMM row for IV2
  emm_row <- tibble::tibble(` ` = "EMM")
  for (j in seq_along(iv2_labels)) {
    emm_row[[iv2_labels[j]]] <- webexercises::fitb(.safe_format(emm_iv2$mean[j], "stat"))
  }
  emm_row[["EMM"]] <- ""

  full_data <- dplyr::bind_rows(cell_data, emm_row)

  tinytable::tt(full_data) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-bordered table-sm",
      bootstrap_css_rule = "width: 70%; margin-left: auto; margin-right: auto;"
    )
}

# =============================================================================
# REGRESSION MODEL CHECKER
# =============================================================================

#' Interactive Regression Model Summary Homework Checker
#'
#' Creates a [tinytable::tt()] table for checking regression model
#' statistics including R, R-squared, F, df, and model significance.
#'
#' @param reg_results_list Output from [linear_reg_answers()].
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' result <- linear_reg_answers(data, criterion = "dv",
#'   quant_predictors = c("x1", "x2"),
#'   quant_labels = c("Pred 1", "Pred 2"),
#'   criterion_label = "Outcome")
#' create_regression_model_checker(result)
#' }
#'
#' @export
create_regression_model_checker <- function(reg_results_list) {

  .check_packages()

  # Format all statistics properly
  # R and R² are bounded (0-1), so use bounded format
  r         <- .safe_format(reg_results_list$Model$R, "bounded")
  r_sq      <- .safe_format(reg_results_list$Model$R_squared, "bounded")
  f_stat    <- .safe_format(reg_results_list$Model$F, "stat")
  df1       <- .safe_format(reg_results_list$Model$df1, "int")
  df2       <- .safe_format(reg_results_list$Model$df2, "int")
  p_val_fmt <- .format_p_apa(reg_results_list$Model$p_value)

  model_works_mcq <- if (reg_results_list$Model$p_value < 0.05) {
    webexercises::mcq(c(answer = "Yes", "No"))
  } else {
    webexercises::mcq(c("Yes", answer = "No"))
  }

  model_table <- tibble::tibble(
    ` `        = "Model Summary",
    `R`        = webexercises::fitb(r),
    R2         = webexercises::fitb(r_sq),
    `F`        = webexercises::fitb(f_stat),
    `df1, df2` = paste0(webexercises::fitb(df1), ", ", webexercises::fitb(df2)),
    `p`        = webexercises::fitb(p_val_fmt),
    `Does the model work?` = model_works_mcq
  )
  names(model_table)[3] <- "R\u00B2"

  tinytable::tt(model_table) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      j = 5,
      bootstrap_css = "min-width: 120px; white-space: nowrap;"
    ) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-bordered table-sm",
      bootstrap_css_rule = "width: 95%; margin-left: auto; margin-right: auto;"
    )
}


# =============================================================================
# REGRESSION PREDICTOR CHECKER
# =============================================================================

#' Interactive Regression Predictor Results Homework Checker
#'
#' Creates a [tinytable::tt()] table for checking individual predictor
#' statistics including variable type, bivariate r, regression weight b,
#' significance levels, and bivariate/multivariate result categories.
#'
#' @param reg_results_list Output from [linear_reg_answers()].
#' @param show_legend Logical. If `TRUE` (default), prints a collapsible
#'   significance key and result category legend above the table.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @export
create_regression_predictor_checker <- function(reg_results_list,
                                                show_legend = TRUE) {

  .check_packages()

  predictors       <- reg_results_list$Labels$predictors
  predictor_labels <- reg_results_list$Labels$predictor_labels
  predictor_types  <- reg_results_list$Labels$predictor_types

  predictor_rows <- purrr::map_dfr(seq_along(predictors), \(i) {
    p     <- predictors[i]
    bivar <- reg_results_list$Bivariate[[p]]
    regwt <- reg_results_list$Regression_Weights[[p]]

    p_type <- predictor_types[i]
    if (is.na(p_type)) p_type <- predictor_types[p]

    # Type MCQ
    type_mcq <- if (p_type == "Binary") {
      webexercises::mcq(c(answer = "Binary", "Quant"))
    } else {
      webexercises::mcq(c("Binary", answer = "Quant"))
    }

    r_sig_mcq <- sig_mcq(bivar$p_value)
    b_sig_mcq <- sig_mcq(regwt$p_value)

    # Category MCQ
    cat_choice <- regwt$category
    category_mcq <- switch(cat_choice,
                           "a" = webexercises::mcq(c(answer = "a", "b", "c", "d")),
                           "b" = webexercises::mcq(c("a", answer = "b", "c", "d")),
                           "c" = webexercises::mcq(c("a", "b", answer = "c", "d")),
                           "d" = webexercises::mcq(c("a", "b", "c", answer = "d")),
                           webexercises::mcq(c("a", "b", "c", "d"))
    )

    tibble::tibble(
      `Predictor` = predictor_labels[i],
      `Type`      = type_mcq,
      # r is bounded (-1 to 1)
      `r`         = webexercises::fitb(.safe_format(bivar$r, "bounded")),
      `r sig`     = r_sig_mcq,
      # b is unbounded
      `b`         = webexercises::fitb(.safe_format(regwt$b, "stat")),
      `b sig`     = b_sig_mcq,
      `Result`    = category_mcq
    )
  })

  if (show_legend) {
    cat(webexercises::hide("Click here for significance key"))
    cat("\n\n**Significance Key:**\n\n")
    cat("- **ns** = p > .05 (not significant)\n")
    cat("- **\\*** = p < .05\n")
    cat("- **\\*\\*** = p < .01\n")
    cat("- **\\*\\*\\*** = p < .001\n\n")
    cat("**Result Categories:**\n\n")
    cat("- **a** = Neither r nor b significant\n")
    cat("- **b** = r & b both significant & same sign\n")
    cat("- **c** = r significant but not b\n")
    cat("- **d** = suppressor effect\n")
    cat(webexercises::unhide())
    cat("\n\n")
  }

  tinytable::tt(predictor_rows) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-bordered table-sm",
      bootstrap_css_rule = "width: 95%; margin-left: auto; margin-right: auto;"
    )
}


#' Interactive Correlation Interpretations (Webexercise)
#'
#' Creates a tinytable showing correlation interpretation for each predictor,
#' either filled (answer key with red HTML) or blank. Requires \code{tinytable}.
#'
#' @param reg_results_list Output from \code{linear_reg_answers()}.
#' @param interpretations Named list or \code{NULL}. Custom interpretations.
#' @param KEY Logical. If \code{TRUE} (default), show filled. If \code{FALSE}, blank.
#'
#' @return A tinytable object.
#'
#' @examples
#' \dontrun{
#' data(superman, package = "psych350data")
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- linear_reg_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_correlation_interpretations(result, KEY = TRUE)
#' }
#'
#' @export
create_correlation_interpretations <- function(reg_results_list,
                                               interpretations = NULL,
                                               KEY = TRUE) {

  if (!requireNamespace("tinytable", quietly = TRUE)) {
    stop("Package 'tinytable' is required. Install with install.packages('tinytable')")
  }

  predictors <- reg_results_list$Labels$predictors
  predictor_labels <- reg_results_list$Labels$predictor_labels
  predictor_types <- reg_results_list$Labels$predictor_types
  criterion_label <- reg_results_list$Labels$criterion_label

  # Auto-generate interpretations if not provided
  if (is.null(interpretations)) {
    interpretations <- list()

    for (i in seq_along(predictors)) {
      p <- predictors[i]
      bivar <- reg_results_list$Bivariate[[p]]
      p_type <- predictor_types[i]

      if (bivar$significant) {
        if (p_type == "Binary") {
          if (bivar$r > 0) {
            interpretations[[p]] <- paste0("Higher coded group tends to have higher ",
                                           criterion_label, " scores")
          } else {
            interpretations[[p]] <- paste0("Higher coded group tends to have lower ",
                                           criterion_label, " scores")
          }
        } else {
          if (bivar$r > 0) {
            interpretations[[p]] <- paste0("As ", predictor_labels[i],
                                           " increases, ", criterion_label,
                                           " scores tend to increase")
          } else {
            interpretations[[p]] <- paste0("As ", predictor_labels[i],
                                           " increases, ", criterion_label,
                                           " scores tend to decrease")
          }
        }
      } else {
        interpretations[[p]] <- paste0(predictor_labels[i],
                                       " is not correlated with ", criterion_label)
      }
    }
  }

  if (KEY) {
    table_data <- tibble::tibble(
      `Predictor` = predictor_labels,
      `Interpretation` = sapply(predictors, function(p) {
        paste0('<span style="color: red;">', interpretations[[p]], '</span>')
      })
    )
  } else {
    table_data <- tibble::tibble(
      `Predictor` = predictor_labels,
      `Interpretation` = rep("", length(predictors))
    )
  }

  result_table <- tinytable::tt(table_data) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )

  return(result_table)
}


#' Interactive Regression Weight Interpretations (Webexercise)
#'
#' Creates a tinytable showing regression weight interpretation for each
#' predictor, either filled (answer key with red HTML) or blank.
#' Requires \code{tinytable}.
#'
#' @param reg_results_list Output from \code{linear_reg_answers()}.
#' @param interpretations Named list or \code{NULL}. Custom interpretations.
#' @param KEY Logical. If \code{TRUE} (default), show filled. If \code{FALSE}, blank.
#'
#' @return A tinytable object.
#'
#' @examples
#' \dontrun{
#' data(superman, package = "psych350data")
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- linear_reg_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_regression_weight_interpretations(result, KEY = TRUE)
#' }
#'
#' @export
create_regression_weight_interpretations <- function(reg_results_list,
                                                     interpretations = NULL,
                                                     KEY = TRUE) {

  if (!requireNamespace("tinytable", quietly = TRUE)) {
    stop("Package 'tinytable' is required. Install with install.packages('tinytable')")
  }

  predictors <- reg_results_list$Labels$predictors
  predictor_labels <- reg_results_list$Labels$predictor_labels
  predictor_types <- reg_results_list$Labels$predictor_types
  criterion_label <- reg_results_list$Labels$criterion_label

  # Auto-generate interpretations if not provided
  if (is.null(interpretations)) {
    interpretations <- list()

    for (i in seq_along(predictors)) {
      p <- predictors[i]
      regwt <- reg_results_list$Regression_Weights[[p]]
      p_type <- predictor_types[i]

      if (!regwt$significant) {
        interpretations[[p]] <- paste0(predictor_labels[i],
                                       " does not contribute to the model")
      } else {
        if (p_type == "Binary") {
          if (regwt$b > 0) {
            interpretations[[p]] <- paste0(
              "Higher coded group has ", criterion_label,
              " scores ", abs(regwt$b),
              " higher than lower coded group, ",
              "after controlling for all other variables")
          } else {
            interpretations[[p]] <- paste0(
              "Higher coded group has ", criterion_label,
              " scores ", abs(regwt$b),
              " lower than lower coded group, ",
              "after controlling for all other variables")
          }
        } else {
          direction <- ifelse(regwt$b > 0, "increase", "decrease")
          interpretations[[p]] <- paste0(
            "For each 1-unit increase in ",
            predictor_labels[i], ", ", criterion_label,
            " is expected to ", direction, " by ",
            abs(regwt$b),
            ", after controlling for all other variables")
        }
      }
    }
  }

  if (KEY) {
    table_data <- tibble::tibble(
      `Predictor` = predictor_labels,
      `Interpretation` = sapply(predictors, function(p) {
        paste0('<span style="color: red;">', interpretations[[p]], '</span>')
      })
    )
  } else {
    table_data <- tibble::tibble(
      `Predictor` = predictor_labels,
      `Interpretation` = rep("", length(predictors))
    )
  }

  result_table <- tinytable::tt(table_data) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )

  return(result_table)
}

