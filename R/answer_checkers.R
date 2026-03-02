# =============================================================================
# answer_checkers.R
# Interactive Homework Checker Functions for Webexercises (tinytable)
# =============================================================================
# These functions create tinytable-based interactive widgets using
# webexercises::fitb() for fill-in-the-blank and webexercises::mcq() for multiple choice.
# They require the 'tinytable' and 'webexercises' packages (Suggests).
#
# Updated for dplyr 1.2.0
# =============================================================================


# -----------------------------------------------------------------------------
# INTERNAL HELPERS
# -----------------------------------------------------------------------------

#' Create chi-square symbol using unicode package
#' @keywords internal
#' @noRd
.chi_sq_symbol <- function() {

  paste0(intToUtf8(0x03C7), intToUtf8(0x00B2))
}

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
#' @keywords internal
#' @export
sig_mcq <- function(p_value) {
  if (!requireNamespace("webexercises", quietly = TRUE)) {
    stop("Package 'webexercises' is required.")
  }

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
  if (!requireNamespace("webexercises", quietly = TRUE)) {
    stop("Package 'webexercises' is required.")
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
  if (!requireNamespace("webexercises", quietly = TRUE)) {
    stop("Package 'webexercises' is required.")
  }

  options <- switch(test_type,
                    "omnibus_f" = c(
                      no = "No - a nonsignificant Omnibus F-test",
                      yes = "Yes - significant Omnibus F-test"
                    ),
                    "interaction" = c(
                      no = "No - a nonsignificant interaction",
                      yes = "Yes - significant interaction"
                    ),
                    "chi_square" = c(
                      no = "No - a nonsignificant Omnibus Chi-Square test",
                      yes = "Yes - significant Omnibus Chi-Square test"
                    ),
                    c(no = "No", yes = "Yes")
  )

  if (p_value < alpha) {
    webexercises::mcq(c(options["no"], answer = options["yes"]))
  } else {
    webexercises::mcq(c(answer = options["no"], options["yes"]))
  }
}


#' Create error type MCQ based on significance
#'
#' @param is_significant Logical. Whether the result is significant.
#' @return HTML string from [webexercises::mcq()].
#' @keywords internal
#' @noRd
.make_error_type_mcq <- function(is_significant) {
  if (!requireNamespace("webexercises", quietly = TRUE)) {
    stop("Package 'webexercises' is required.")
  }

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
  if (!requireNamespace("webexercises", quietly = TRUE)) {
    stop("Package 'webexercises' is required.")
  }

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


#' Format p-value for display
#'
#' @param p_value Numeric p-value.
#' @return Character formatted p-value.
#' @keywords internal
#' @noRd
.format_p_display <- function(p_value) {
  if (is.na(p_value) || p_value < 0.001) ".001" else p_value
}


#' Safely extract scalar value for fitb
#'
#' @param value Value to check and convert.
#' @param default Default value if NA or empty.
#' @return Scalar value safe for webexercises::fitb().
#' @keywords internal
#' @noRd
.safe_fitb_value <- function(value, default = "NA") {
  if (is.null(value) || length(value) == 0 || (length(value) == 1 && is.na(value))) {
    default

  } else {
    value[[1]]
  }
}


#' Check and load required packages
#'
#' @param packages Character vector of package names.
#' @keywords internal
#' @noRd
.check_packages <- function(packages = c("tinytable", "webexercises")) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Package '", pkg, "' is required. ",
        "Install with install.packages('", pkg, "')"
      )
    }
  }
}


# =============================================================================
# DESCRIPTIVE STATISTICS CHECKER
# =============================================================================

#' Interactive Descriptive Statistics Homework Checker
#'
#' Creates a [tinytable::tt()] table with fill-in-the-blank and multiple
#' choice inputs for checking descriptive statistics results. Requires the
#' **tinytable** and **webexercises** packages.
#'
#' @param vars Character vector. Variable names to include, in display order.
#' @param desc_results_list Output from [descriptives_answers()]. Must contain
#'   `$Descriptives` (a tibble with columns `variable`, `mean`, `sd`, `n`, `sem`).
#' @param var_labels Named character vector or `NULL`. Optional display labels,
#'
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
#' @examples
#' \dontrun{
#' data(superman)
#' result <- descriptives_answers(superman,
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

  desc_stats <- desc_results_list$Descriptives

  # Build type lookup from category parameters
  var_type_map <- c(
    stats::setNames(rep("label", length(label)), label),
    stats::setNames(rep("quantitative", length(quantitative)), quantitative),
    stats::setNames(rep("binary", length(binary)), binary),
    stats::setNames(rep("multi_category", length(multi_category)), multi_category)
  )

  # MCQ answer text for each type
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

  # Helper to build MCQ
  build_mcq <- function(correct_type) {
    correct <- answer_text[correct_type]
    opts <- all_options
    names(opts) <- ifelse(opts == correct, "answer", opts)
    webexercises::mcq(opts)
  }

  # Helper to get display label
  get_label <- function(v) {
    if (!is.null(var_labels) && v %in% names(var_labels)) {
      return(unname(var_labels[v]))
    }
    v
  }

  # Build all rows using purrr::map
  rows_list <- purrr::map(vars, \(v) {
    # Filter to get the matching row
    var_stats <- desc_stats |>
      dplyr::filter(.data$variable == v)

    # Check if variable was found
    if (nrow(var_stats) == 0) {
      stop(
        "Variable '", v, "' not found in desc_results_list$Descriptives. ",
        "Available variables: ", paste(desc_stats$variable, collapse = ", ")
      )
    }

    # Extract scalar values safely using dplyr::pull()
    mean_val <- .safe_fitb_value(dplyr::pull(var_stats, "mean"))
    sd_val <- .safe_fitb_value(dplyr::pull(var_stats, "sd"))
    sem_val <- .safe_fitb_value(dplyr::pull(var_stats, "sem"))

    # Determine variable type (default to quantitative)
    vtype <- if (v %in% names(var_type_map)) var_type_map[[v]] else "quantitative"

    tibble::tibble(
      Variable = get_label(v),
      Mean = webexercises::fitb(mean_val),
      SD = webexercises::fitb(sd_val),
      SEM = webexercises::fitb(sem_val),
      `Interpretable?` = build_mcq(vtype)
    )
  })

  # Combine all rows
  table_data <- dplyr::bind_rows(rows_list)

  # Create tinytable and apply styling
  table_data |>
    tinytable::tt() |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )
}


# =============================================================================
# CORRELATION CHECKER
# =============================================================================

#' Interactive Correlation Homework Checker
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
#' data(superman)
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

  # Extract scalars safely
  var1_mean <- .safe_fitb_value(dplyr::pull(var1_stats, "mean"))
  var1_sd <- .safe_fitb_value(dplyr::pull(var1_stats, "sd"))
  var1_n <- .safe_fitb_value(dplyr::pull(var1_stats, "n"))

  var2_mean <- .safe_fitb_value(dplyr::pull(var2_stats, "mean"))
  var2_sd <- .safe_fitb_value(dplyr::pull(var2_stats, "sd"))
  var2_n <- .safe_fitb_value(dplyr::pull(var2_stats, "n"))

  p_value <- corr_results_list$Correlation$p_value

  reject_retain_mcq <- .make_reject_retain_mcq(p_value)

  corr_table_data <- tibble::tibble(
    ` `  = paste("Correlation:", rh_name),
    r    = webexercises::fitb(corr_results_list$Correlation$r),
    p    = webexercises::fitb(p_value),
    df   = webexercises::fitb(corr_results_list$Correlation$df),
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

  chi_sq    <- chi_results_list$ChiSquare$chi_sq
  p_value   <- chi_results_list$ChiSquare$p_value
  df        <- chi_results_list$ChiSquare$df
  var1_desc <- chi_results_list$Var1_Descriptives
  var2_desc <- chi_results_list$Var2_Descriptives

  # Extract scalar values safely
  var1_n1 <- .safe_fitb_value(var1_desc$n[1])
  var1_n2 <- .safe_fitb_value(var1_desc$n[2])
  var2_n1 <- .safe_fitb_value(var2_desc$n[1])
  var2_n2 <- .safe_fitb_value(var2_desc$n[2])

  reject_retain_mcq <- .make_reject_retain_mcq(p_value)

  chi_table_data <- tibble::tibble(
    ` `  = paste("Chi-Square:", rh_name),
    chi2 = webexercises::fitb(chi_sq),
    p    = webexercises::fitb(p_value),
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

  chi_sq <- chisq_results_list$ChiSquare$chi_sq
  p_value <- chisq_results_list$ChiSquare$p_value
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
    p = webexercises::fitb(p_value),
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

  # Extract values safely
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

  chi_crit <- chisq_results_list$ChiCrit

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
        webexercises::fitb(.safe_fitb_value(pairwise[[i]]$pct1)), "% vs ",
        webexercises::fitb(.safe_fitb_value(pairwise[[i]]$pct2)), "%"
      )
    }),
    #use unicode package to fix chr
    chi2_result = purrr::map_chr(seq_len(n_pairwise), \(i) {
      paste0(
        webexercises::fitb(.safe_fitb_value(pairwise[[i]]$chi_sq)), " ",
        webexercises::fitb(.safe_fitb_value(pairwise[[i]]$chi_result))
      )
    }),
    `Type of Error` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      is_sig <- pairwise[[i]]$chi_result != "="
      .make_error_type_mcq(is_sig)
    }),
    `Effect Size (r)` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      webexercises::fitb(.safe_fitb_value(pairwise[[i]]$effect_size))
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

  chi_crit <- chisq_results_list$ChiCrit

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
        webexercises::fitb(.safe_fitb_value(pairwise[[i]]$pct1)), "% vs ",
        webexercises::fitb(.safe_fitb_value(pairwise[[i]]$pct2)), "%"
      )
    }),
    chi2_result = purrr::map_chr(seq_len(n_pairwise), \(i) {
      paste0(
        webexercises::fitb(.safe_fitb_value(pairwise[[i]]$chi_sq)), " ",
        webexercises::fitb(.safe_fitb_value(pairwise[[i]]$chi_result))
      )
    }),
    `Type of Error` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      is_sig <- pairwise[[i]]$chi_result != "="
      .make_error_type_mcq(is_sig)
    }),
    `Effect Size (r)` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      webexercises::fitb(.safe_fitb_value(pairwise[[i]]$effect_size))
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
#' data(superman)
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

  f_stat     <- anova_results_list$ANOVA$F
  p_value    <- anova_results_list$ANOVA$p_value
  df_between <- anova_results_list$ANOVA$df_between
  df_within  <- anova_results_list$ANOVA$df_within
  mse        <- anova_results_list$ANOVA$mse

  p_value_display <- .format_p_display(p_value)

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
    p             = webexercises::fitb(p_value_display),
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

  # Extract scalar values safely for descriptives
  desc_table_data <- tibble::tibble(
    ` `  = group_labels,
    Mean = purrr::map_chr(seq_along(group_labels), \(i) {
      webexercises::fitb(.safe_fitb_value(desc_stats$mean[i]))
    }),
    SD   = purrr::map_chr(seq_along(group_labels), \(i) {
      webexercises::fitb(.safe_fitb_value(desc_stats$sd[i]))
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
#' data(superman)
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

  f_stat    <- .safe_fitb_value(anova_results_list$ANOVA$F, "___")
  p_value   <- anova_results_list$ANOVA$p_value
  df_effect <- .safe_fitb_value(anova_results_list$ANOVA$df_effect, "___")
  df_error  <- .safe_fitb_value(anova_results_list$ANOVA$df_error, "___")
  mse       <- .safe_fitb_value(anova_results_list$ANOVA$mse, "___")

  p_value_display <- .format_p_display(p_value)

  desc_stats <- anova_results_list$Descriptives
  if (is.null(condition_labels)) {
    condition_labels <- as.character(desc_stats$condition)
  }

  # Create reject/retain MCQ
  if (!is.na(p_value) && length(p_value) > 0 && p_value < 0.05) {
    reject_retain_mcq <- webexercises::mcq(c(
      "Retain the H0 null hypothesis",
      answer = "Reject the H0 null hypothesis"
    ))
  } else {
    reject_retain_mcq <- webexercises::mcq(c(
      answer = "Retain the H0 null hypothesis",
      "Reject the H0 null hypothesis"
    ))
  }

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
    p            = webexercises::fitb(p_value_display),
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
      webexercises::fitb(.safe_fitb_value(desc_stats$mean[i]))
    }),
    SD   = purrr::map_chr(seq_along(condition_labels), \(i) {
      webexercises::fitb(.safe_fitb_value(desc_stats$sd[i]))
    }),
    n    = purrr::map_chr(seq_along(condition_labels), \(i) {
      webexercises::fitb(.safe_fitb_value(desc_stats$n[i]))
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

  f_stat     <- anova_results_list$ANOVA$F
  p_value    <- anova_results_list$ANOVA$p_value
  df_between <- anova_results_list$ANOVA$df_between
  df_within  <- anova_results_list$ANOVA$df_within
  mse        <- anova_results_list$ANOVA$mse
  total_n    <- anova_results_list$ANOVA$total_n
  k          <- anova_results_list$ANOVA$k
  mean_n     <- anova_results_list$ANOVA$mean_n

  desc_stats <- anova_results_list$Descriptives
  n_groups   <- nrow(desc_stats)
  if (is.null(group_labels)) group_labels <- desc_stats$group_label

  posthoc_mcq <- .make_posthoc_mcq(p_value, "omnibus_f")

  anova_table_data <- tibble::tibble(
    ` `            = paste("BG ANOVA:", rh_name),
    F              = webexercises::fitb(f_stat),
    p              = webexercises::fitb(p_value),
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

  desc_table_data <- tibble::tibble(
    ` `  = group_labels,
    Mean = purrr::map_chr(seq_len(n_groups), \(i) {
      webexercises::fitb(.safe_fitb_value(desc_stats$mean[i]))
    }),
    SD   = purrr::map_chr(seq_len(n_groups), \(i) {
      webexercises::fitb(.safe_fitb_value(desc_stats$sd[i]))
    }),
    n    = purrr::map_chr(seq_len(n_groups), \(i) {
      webexercises::fitb(.safe_fitb_value(desc_stats$n[i]))
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

  lsd_mmd    <- anova_results_list$LSD$lsd_mmd
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
      webexercises::fitb(.safe_fitb_value(pairwise[[i]]$mean_diff))
    }),
    `LSD Result` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      webexercises::fitb(.safe_fitb_value(pairwise[[i]]$lsd_result))
    }),
    `Type of Error` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      is_sig <- pairwise[[i]]$lsd_result != "="
      .make_error_type_mcq(is_sig)
    }),
    `Effect Size (r)` = purrr::map_chr(seq_len(n_pairwise), \(i) {
      webexercises::fitb(.safe_fitb_value(pairwise[[i]]$effect_size))
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
#' create_factorial_anova_checker("RH1", result,
#'   iv1_name = "Anxiety", iv2_name = "Condition")
#' }
#'
#' @export
create_factorial_anova_checker <- function(rh_name, anova_results_list,
                                           iv1_name = "IV1",
                                           iv2_name = "IV2") {

  .check_packages()

  f_iv1          <- anova_results_list$ANOVA$MainEffect_IV1$F
  p_iv1          <- anova_results_list$ANOVA$MainEffect_IV1$p_value
  df_iv1         <- anova_results_list$ANOVA$MainEffect_IV1$df
  f_iv2          <- anova_results_list$ANOVA$MainEffect_IV2$F
  p_iv2          <- anova_results_list$ANOVA$MainEffect_IV2$p_value
  df_iv2         <- anova_results_list$ANOVA$MainEffect_IV2$df
  f_interaction  <- anova_results_list$ANOVA$Interaction$F
  p_interaction  <- anova_results_list$ANOVA$Interaction$p_value
  df_interaction <- anova_results_list$ANOVA$Interaction$df
  df_within      <- anova_results_list$ANOVA$df_within
  mse            <- anova_results_list$ANOVA$mse
  k              <- anova_results_list$ANOVA$k
  mean_n         <- anova_results_list$ANOVA$mean_n
  lsd_mmd        <- anova_results_list$LSD$lsd_mmd

  # Format p-values
  p_iv1_fmt         <- ifelse(p_iv1 < 0.001, "<.001", sprintf("%.2f", p_iv1))
  p_iv2_fmt         <- ifelse(p_iv2 < 0.001, "<.001", sprintf("%.2f", p_iv2))
  p_interaction_fmt <- ifelse(p_interaction < 0.001, "<.001", sprintf("%.2f", p_interaction))

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
create_factorial_desc_checker <- function(anova_results_list,
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

  # Get cell means for each combination
  cell_data <- tibble::tibble(
    ` ` = final_iv1_labels,
    col1 = purrr::map_chr(seq_along(final_iv1_labels), \(i) {
      cell <- desc_stats |>
        dplyr::filter(.data$iv1 == iv1_levels_actual[i],
                      .data$iv2 == iv2_levels_actual[1])
      webexercises::fitb(.safe_fitb_value(dplyr::pull(cell, "mean")))
    }),
    col2 = purrr::map_chr(seq_along(final_iv1_labels), \(i) {
      cell <- desc_stats |>
        dplyr::filter(.data$iv1 == iv1_levels_actual[i],
                      .data$iv2 == iv2_levels_actual[2])
      webexercises::fitb(.safe_fitb_value(dplyr::pull(cell, "mean")))
    }),
    `EMM` = purrr::map_chr(seq_along(final_iv1_labels), \(i) {
      webexercises::fitb(.safe_fitb_value(emm_iv1$emm[i]))
    })
  )
  names(cell_data)[2:3] <- final_iv2_labels

  # Add EMM row for IV2
  emm_row <- tibble::tibble(
    ` ` = "EMM",
    col1 = webexercises::fitb(.safe_fitb_value(emm_iv2$emm[1])),
    col2 = webexercises::fitb(.safe_fitb_value(emm_iv2$emm[2])),
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

  r         <- reg_results_list$Model$R
  r_sq      <- reg_results_list$Model$R_squared
  f_stat    <- reg_results_list$Model$F
  df1       <- reg_results_list$Model$df1
  df2       <- reg_results_list$Model$df2
  p_val_fmt <- reg_results_list$Model$p_value_formatted

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
      `r`         = webexercises::fitb(bivar$r),
      `r sig`     = r_sig_mcq,
      `b`         = webexercises::fitb(regwt$b),
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
