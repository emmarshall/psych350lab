# =============================================================================
# answer_checkers.R
# Interactive Homework Checker Functions for Webexercises (tinytable)
# =============================================================================
# These functions create tinytable-based interactive widgets using
# fitb() for fill-in-the-blank and mcq() for multiple choice.
# They require the 'tinytable' and 'webexercises' packages (Suggests).
# =============================================================================


# -----------------------------------------------------------------------------
# Internal helpers for regression checkers
# -----------------------------------------------------------------------------

#' Convert a p-value to significance stars
#'
#' @param p Numeric p-value.
#' @return Character: `"***"`, `"**"`, `"*"`, or `"ns"`.
#' @keywords internal
#' @export
p_to_stars <- function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  return("ns")
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
  if (stars == "ns") {
    return(webexercises::mcq(c(answer = "ns", "*", "**", "***")))
  } else if (stars == "*") {
    return(webexercises::mcq(c("ns", answer = "*", "**", "***")))
  } else if (stars == "**") {
    return(webexercises::mcq(c("ns", "*", answer = "**", "***")))
  } else {
    return(webexercises::mcq(c("ns", "*", "**", answer = "***")))
  }
}

#' Interactive Descriptive Statistics Checker (Webexercise)
#'
#' Creates a tibble with fill-in-the-blank inputs for mean, SD, and SEM,
#' plus a multiple choice dropdown for whether the mean is interpretable.
#' Designed for use in Quarto HTML homework checkers. Pass the result to
#' \code{tinytable::tt()} with \code{format_tt(escape = FALSE)} inside
#' a chunk with \code{results: asis} and \code{echo: false}.
#'
#' @param vars Character vector. Variable names to include, in display order.
#' @param stats_data A data frame with columns \code{variable}, \code{mean},
#'   \code{sd}, and \code{sem}. Typically the output of
#'   \code{compute_summary_stats()}.
#' @param label Character vector or \code{NULL}. Variables that are IDs/labels.
#' @param quantitative Character vector or \code{NULL}. Continuous variables.
#' @param binary Character vector or \code{NULL}. Dichotomous variables.
#' @param multi_category Character vector or \code{NULL}. Nominal variables
#'   with 3+ levels.
#'
#' @return A tibble with columns Variable, Mean, SD, SEM, and Interpretable
#'   containing HTML strings from webexercises.
#'
#' @examples
#' \dontrun{
#' data(superman)
#' library(dplyr)
#'
#' walkthrough <- superman |>
#'   select(num, year, type, clark_height_in, clark_grp,
#'          height_diff, height_gap) |>
#'   filter(year > 1975)
#'
#' stats <- compute_summary_stats(walkthrough)
#'
#' tbl <- create_univariate_checker(
#'   vars       = names(walkthrough),
#'   stats_data = stats,
#'   label          = "num",
#'   binary         = "clark_grp",
#'   multi_category = c("type", "height_gap")
#' )
#' }
#'
#' @export
create_univariate_checker <- function(vars,
                                      stats_data,
                                      label          = NULL,
                                      quantitative   = NULL,
                                      binary         = NULL,
                                      multi_category = NULL) {

  if (!requireNamespace("webexercises", quietly = TRUE)) {
    stop("Package 'webexercises' is required. ",
         "Install with install.packages('webexercises')")
  }

  # Build the type lookup from the category parameters
  var_type_map <- c(
    stats::setNames(rep("label",
                        length(label)), label),
    stats::setNames(rep("quantitative",
                        length(quantitative)), quantitative),
    stats::setNames(rep("binary",
                        length(binary)), binary),
    stats::setNames(rep("multi_category",
                        length(multi_category)), multi_category)
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

  build_mcq <- function(correct_type) {
    correct <- answer_text[correct_type]
    opts <- all_options
    names(opts) <- ifelse(opts == correct, "answer", opts)
    webexercises::mcq(opts)
  }

  stats_data |>
    dplyr::filter(.data$variable %in% vars) |>
    dplyr::slice(match(vars, .data$variable)) |>
    dplyr::mutate(
      Mean_input = purrr::map_chr(
        .data$mean, ~webexercises::fitb(.x)
      ),
      SD_input = purrr::map_chr(
        .data$sd, ~webexercises::fitb(.x)
      ),
      SEM_input = purrr::map_chr(
        .data$sem, ~webexercises::fitb(.x)
      ),
      Interpretable_input = purrr::map_chr(
        .data$variable, function(v) {
          vtype <- if (v %in% names(var_type_map)) {
            var_type_map[v]
          } else {
            "quantitative"
          }
          build_mcq(vtype)
        }
      )
    ) |>
    dplyr::select(
      Variable         = "variable",
      Mean             = "Mean_input",
      SD               = "SD_input",
      SEM              = "SEM_input",
      `Interpretable?` = "Interpretable_input"
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

  if (!requireNamespace("tinytable", quietly = TRUE))
    stop("Package 'tinytable' is required. Install with install.packages('tinytable')")
  if (!requireNamespace("webexercises", quietly = TRUE))
    stop("Package 'webexercises' is required. Install with install.packages('webexercises')")

  desc_stats <- corr_results_list$Descriptives
  var1_stats <- desc_stats[desc_stats$variable == vars[1], ]
  var2_stats <- desc_stats[desc_stats$variable == vars[2], ]
  p_value    <- corr_results_list$Correlation$p_value

  if (p_value < 0.05) {
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
    Mean   = webexercises::fitb(var1_stats$mean),
    SD     = webexercises::fitb(var1_stats$sd),
    N      = webexercises::fitb(var1_stats$n),
    `  `   = ""
  )
  desc_table1 <- tinytable::tt(desc_table1_data) |>
    tinytable::format_tt(escape = FALSE)

  desc_table2_data <- tibble::tibble(
    ` `    = paste("Variable 2:", vars[2]),
    Mean   = webexercises::fitb(var2_stats$mean),
    SD     = webexercises::fitb(var2_stats$sd),
    N      = webexercises::fitb(var2_stats$n),
    `   `  = ""
  )
  desc_table2 <- tinytable::tt(desc_table2_data) |>
    tinytable::format_tt(escape = FALSE)

  combined <- tinytable::rbind2(corr_table, desc_table1, use_names = FALSE) |>
    tinytable::rbind2(desc_table2, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 80%; margin-left: auto; margin-right: auto;"
    )

  return(combined)
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
#' @param vars Character vector of length 2. Variable names (for reference).
#' @param chi_results_list Output from [chi_square_answers()].
#' @param var1_labels Character vector of length 2. Labels for variable 1
#'   levels. Default `c("1", "2")`.
#' @param var2_labels Character vector of length 2. Labels for variable 2
#'   levels. Default `c("1", "2")`.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' data(superman)
#' result <- chi_square_answers(superman, "clark_grp", "tomatometer")
#' create_chi_checker("RH1", c("clark_grp", "tomatometer"), result,
#'   var1_labels = c("Under 6ft", "6ft+"),
#'   var2_labels = c("Rotten", "Fresh"))
#' }
#'
#' @export
create_chi_checker <- function(rh_name, vars, chi_results_list,
                               var1_labels = c("1", "2"),
                               var2_labels = c("1", "2")) {

  if (!requireNamespace("tinytable", quietly = TRUE))
    stop("Package 'tinytable' is required.")
  if (!requireNamespace("webexercises", quietly = TRUE))
    stop("Package 'webexercises' is required.")

  chi_sq   <- chi_results_list$ChiSquare$chi_sq
  p_value  <- chi_results_list$ChiSquare$p_value
  df       <- chi_results_list$ChiSquare$df
  var1_desc <- chi_results_list$Var1_Descriptives
  var2_desc <- chi_results_list$Var2_Descriptives

  if (p_value < 0.05) {
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
      webexercises::fitb(var1_desc$n[1]),
      webexercises::fitb(var1_desc$n[2])
    ),
    `  ` = c(
      paste("Number of", var2_labels[1], "in the sample"),
      paste("Number of", var2_labels[2], "in the sample")
    ),
    `n  ` = c(
      webexercises::fitb(var2_desc$n[1]),
      webexercises::fitb(var2_desc$n[2])
    ),
    `   ` = ""
  )
  desc_table <- tinytable::tt(desc_table_data) |>
    tinytable::format_tt(escape = FALSE)

  combined <- tinytable::rbind2(chi_table, desc_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 80%; margin-left: auto; margin-right: auto;"
    )

  return(combined)
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

  if (!requireNamespace("tinytable", quietly = TRUE))
    stop("Package 'tinytable' is required.")
  if (!requireNamespace("webexercises", quietly = TRUE))
    stop("Package 'webexercises' is required.")

  anova_type_mcq <- webexercises::mcq(c(
    answer = "Between-Groups (BG)",
    "Within-Groups (WG)"
  ))

  f_stat     <- anova_results_list$ANOVA$F
  p_value    <- anova_results_list$ANOVA$p_value
  df_between <- anova_results_list$ANOVA$df_between
  df_within  <- anova_results_list$ANOVA$df_within
  mse        <- anova_results_list$ANOVA$mse

  if (is.na(p_value) || p_value < 0.001) {
    p_value_display <- ".001"
  } else {
    p_value_display <- p_value
  }

  desc_stats <- anova_results_list$Descriptives
  if (is.null(group_labels)) group_labels <- as.character(desc_stats$iv)

  if (p_value < 0.05) {
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
    ` `      = paste("ANOVA Type:", rh_name),
    `Type`   = anova_type_mcq,
    `  ` = "", `   ` = "", `    ` = "", `     ` = "", `      ` = ""
  )
  type_table <- tinytable::tt(type_table_data) |>
    tinytable::format_tt(escape = FALSE)

  anova_table_data <- tibble::tibble(
    ` `              = paste("ANOVA:", rh_name),
    F                = webexercises::fitb(f_stat),
    p                = webexercises::fitb(p_value_display),
    `df (between)`   = webexercises::fitb(df_between),
    `df (within)`    = webexercises::fitb(df_within),
    MSE              = webexercises::fitb(mse),
    `Reject or Retain?` = reject_retain_mcq
  )
  anova_table <- tinytable::tt(anova_table_data) |>
    tinytable::format_tt(escape = FALSE)

  desc_table1_data <- tibble::tibble(
    ` `  = paste("Group 1:", group_labels[1]),
    Mean = webexercises::fitb(desc_stats$mean[1]),
    SD   = webexercises::fitb(desc_stats$sd[1]),
    N    = webexercises::fitb(desc_stats$n[1]),
    `  ` = "", `   ` = "", `    ` = ""
  )
  desc_table1 <- tinytable::tt(desc_table1_data) |>
    tinytable::format_tt(escape = FALSE)

  desc_table2_data <- tibble::tibble(
    ` `  = paste("Group 2:", group_labels[2]),
    Mean = webexercises::fitb(desc_stats$mean[2]),
    SD   = webexercises::fitb(desc_stats$sd[2]),
    N    = webexercises::fitb(desc_stats$n[2]),
    `  ` = "", `   ` = "", `    ` = ""
  )
  desc_table2 <- tinytable::tt(desc_table2_data) |>
    tinytable::format_tt(escape = FALSE)

  combined <- tinytable::rbind2(type_table, anova_table, use_names = FALSE) |>
    tinytable::rbind2(desc_table1, use_names = FALSE) |>
    tinytable::rbind2(desc_table2, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )

  return(combined)
}


# =============================================================================
# WITHIN-GROUPS ANOVA CHECKER (2-group)
# =============================================================================

#' Interactive Within-Groups ANOVA Homework Checker (2-Group)
#'
#' Creates a [tinytable::tt()] table for checking a 2-condition
#' within-groups (repeated measures) ANOVA. Includes ANOVA type
#' identification, F-test statistics, and condition descriptives.
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

  if (!requireNamespace("tinytable", quietly = TRUE))
    stop("Package 'tinytable' is required.")
  if (!requireNamespace("webexercises", quietly = TRUE))
    stop("Package 'webexercises' is required.")

  anova_type_mcq <- webexercises::mcq(c(
    "Between-Groups (BG)",
    answer = "Within-Groups (WG)"
  ))

  f_stat    <- anova_results_list$ANOVA$F
  p_value   <- anova_results_list$ANOVA$p_value
  df_effect <- anova_results_list$ANOVA$df_effect
  df_error  <- anova_results_list$ANOVA$df_error
  mse       <- anova_results_list$ANOVA$mse

  if (is.na(p_value) || p_value < 0.001) {
    p_value_display <- ".001"
  } else {
    p_value_display <- p_value
  }

  desc_stats <- anova_results_list$Descriptives
  if (is.null(condition_labels)) condition_labels <- as.character(desc_stats$condition)

  # Handle NA values
  f_stat          <- if (is.na(f_stat) || length(f_stat) == 0) "___" else f_stat
  p_value_display <- if (is.na(p_value_display) || length(p_value_display) == 0) "___" else p_value_display
  df_effect       <- if (is.na(df_effect) || length(df_effect) == 0) "___" else df_effect
  df_error        <- if (is.na(df_error) || length(df_error) == 0) "___" else df_error
  mse             <- if (is.na(mse) || length(mse) == 0) "___" else mse

  if (!is.na(as.numeric(p_value)) && length(p_value) > 0 && as.numeric(p_value) < 0.05) {
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
    `  ` = "", `   ` = "", `    ` = "", `     ` = "", `      ` = ""
  )
  type_table <- tinytable::tt(type_table_data) |>
    tinytable::format_tt(escape = FALSE)

  anova_table_data <- tibble::tibble(
    ` `            = paste("ANOVA:", rh_name),
    F              = webexercises::fitb(f_stat),
    p              = webexercises::fitb(p_value_display),
    `df (effect)`  = webexercises::fitb(df_effect),
    `df (error)`   = webexercises::fitb(df_error),
    MSE            = webexercises::fitb(mse),
    `Reject or Retain?` = reject_retain_mcq
  )
  anova_table <- tinytable::tt(anova_table_data) |>
    tinytable::format_tt(escape = FALSE)

  desc_table1_data <- tibble::tibble(
    ` `  = paste("Condition 1:", condition_labels[1]),
    Mean = webexercises::fitb(desc_stats$mean[1]),
    SD   = webexercises::fitb(desc_stats$sd[1]),
    N    = webexercises::fitb(desc_stats$n[1]),
    `  ` = "", `   ` = "", `    ` = ""
  )
  desc_table1 <- tinytable::tt(desc_table1_data) |>
    tinytable::format_tt(escape = FALSE)

  desc_table2_data <- tibble::tibble(
    ` `  = paste("Condition 2:", condition_labels[2]),
    Mean = webexercises::fitb(desc_stats$mean[2]),
    SD   = webexercises::fitb(desc_stats$sd[2]),
    N    = webexercises::fitb(desc_stats$n[2]),
    `  ` = "", `   ` = "", `    ` = ""
  )
  desc_table2 <- tinytable::tt(desc_table2_data) |>
    tinytable::format_tt(escape = FALSE)

  combined <- tinytable::rbind2(type_table, anova_table, use_names = FALSE) |>
    tinytable::rbind2(desc_table1, use_names = FALSE) |>
    tinytable::rbind2(desc_table2, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )

  return(combined)
}


# =============================================================================
# K-GROUP ANOVA OMNIBUS CHECKER
# =============================================================================

#' Interactive K-Group ANOVA Omnibus Homework Checker
#'
#' Creates a [tinytable::tt()] table for checking omnibus ANOVA statistics,
#' sample information, and per-group descriptives from a multi-group
#' between-groups ANOVA.
#'
#' @param rh_name Character. Research hypothesis label.
#' @param anova_results_list Output from `anova_multigroup_answers()`. Must
#'   contain `$ANOVA` (with `F`, `p_value`, `df_between`, `df_within`, `mse`,
#'   `total_n`, `k`, `mean_n`) and `$Descriptives` (with `group_label`,
#'   `mean`, `sd`, `n`).
#' @param group_labels Character vector or `NULL`. Display labels for groups.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' result <- anova_multigroup_answers(data, dv = "sentence",
#'   iv = "attract",
#'   group_labels = c("Beautiful", "Average", "Unattractive"))
#' create_anova_omnibus_checker("RH1", result)
#' }
#'
#' @export
create_anova_omnibus_checker <- function(rh_name, anova_results_list,
                                         group_labels = NULL) {

  if (!requireNamespace("tinytable", quietly = TRUE))
    stop("Package 'tinytable' is required.")
  if (!requireNamespace("webexercises", quietly = TRUE))
    stop("Package 'webexercises' is required.")

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

  if (p_value < 0.05) {
    posthoc_mcq <- webexercises::mcq(c(
      "No - a nonsignificant Omnibus F-test",
      answer = "Yes - significant Omnibus F-test"
    ))
  } else {
    posthoc_mcq <- webexercises::mcq(c(
      answer = "No - a nonsignificant Omnibus F-test",
      "Yes - significant Omnibus F-test"
    ))
  }

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
    ` `          = "",
    `N`          = webexercises::fitb(total_n),
    k            = webexercises::fitb(k),
    `average n`  = webexercises::fitb(mean_n),
    `  ` = "", `   ` = "", `    ` = ""
  )
  sample_table <- tinytable::tt(sample_table_data) |>
    tinytable::format_tt(escape = FALSE)

  desc_table_data <- tibble::tibble(
    ` `  = group_labels,
    Mean = sapply(seq_len(n_groups), function(i) webexercises::fitb(desc_stats$mean[i])),
    SD   = sapply(seq_len(n_groups), function(i) webexercises::fitb(desc_stats$sd[i])),
    n    = sapply(seq_len(n_groups), function(i) webexercises::fitb(desc_stats$n[i])),
    `  ` = rep("", n_groups),
    `   ` = rep("", n_groups),
    `    ` = rep("", n_groups)
  )
  desc_table <- tinytable::tt(desc_table_data) |>
    tinytable::format_tt(escape = FALSE)

  combined <- tinytable::rbind2(anova_table, sample_table, use_names = FALSE) |>
    tinytable::rbind2(desc_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )

  return(combined)
}


# =============================================================================
# K-GROUP ANOVA LSD PAIRWISE CHECKER
# =============================================================================

#' Interactive K-Group ANOVA LSD Pairwise Homework Checker
#'
#' Creates a [tinytable::tt()] table for checking LSD MMD value, pairwise
#' mean differences, comparison results, error types, effect sizes, and
#' power assessments.
#'
#' @param anova_results_list Output from `anova_multigroup_answers()`.
#' @param group_labels Character vector or `NULL`. Optional relabelling of
#'   comparison group names.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @export
create_lsd_pairwise_checker <- function(anova_results_list,
                                        group_labels = NULL) {

  if (!requireNamespace("tinytable", quietly = TRUE))
    stop("Package 'tinytable' is required.")
  if (!requireNamespace("webexercises", quietly = TRUE))
    stop("Package 'webexercises' is required.")

  lsd_mmd    <- anova_results_list$LSD$lsd_mmd
  pairwise   <- anova_results_list$Pairwise
  n_pairwise <- length(pairwise)

  # Relabel if needed
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
    ` `    = "LSDmmd",
    `  `   = webexercises::fitb(lsd_mmd),
    `   ` = "", `    ` = "", `     ` = "", `      ` = ""
  )
  lsd_mmd_table <- tinytable::tt(lsd_mmd_data) |>
    tinytable::format_tt(escape = FALSE)

  pairwise_table_data <- tibble::tibble(
    ` ` = sapply(seq_len(n_pairwise), function(i) pairwise[[i]]$comparison),
    `Mean Difference` = sapply(seq_len(n_pairwise), function(i)
      webexercises::fitb(pairwise[[i]]$mean_diff)),
    `LSD Result` = sapply(seq_len(n_pairwise), function(i)
      webexercises::fitb(pairwise[[i]]$lsd_result)),
    `Type of Error` = sapply(seq_len(n_pairwise), function(i) {
      is_sig <- pairwise[[i]]$lsd_result != "="
      if (is_sig) {
        webexercises::mcq(c(answer = "Type I & III", "Type II"))
      } else {
        webexercises::mcq(c("Type I & III", answer = "Type II"))
      }
    }),
    `Effect Size (r)` = sapply(seq_len(n_pairwise), function(i)
      webexercises::fitb(pairwise[[i]]$effect_size)),
    `Power Problem?` = sapply(seq_len(n_pairwise), function(i) {
      power <- pairwise[[i]]$power_problem
      if (grepl("rejecting H0", power)) {
        webexercises::mcq(c(
          answer = "No - rejecting H0: means there was sufficient power",
          "No - effect is \"too small to be interesting,\" (r < .10)",
          "Yes - The effect is \"large enough to be interesting,\" (r > .10)"
        ))
      } else if (grepl("too small", power)) {
        webexercises::mcq(c(
          "No - rejecting H0: means there was sufficient power",
          answer = "No - effect is \"too small to be interesting,\" (r < .10)",
          "Yes - The effect is \"large enough to be interesting,\" (r > .10)"
        ))
      } else {
        webexercises::mcq(c(
          "No - rejecting H0: means there was sufficient power",
          "No - effect is \"too small to be interesting,\" (r < .10)",
          answer = "Yes - The effect is \"large enough to be interesting,\" (r > .10)"
        ))
      }
    })
  )
  pairwise_table <- tinytable::tt(pairwise_table_data) |>
    tinytable::format_tt(escape = FALSE)

  combined <- tinytable::rbind2(lsd_mmd_table, pairwise_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 95%; margin-left: auto; margin-right: auto;"
    )

  return(combined)
}


# =============================================================================
# K-GROUP CHI-SQUARE OMNIBUS CHECKER
# =============================================================================

#' Interactive K-Group Chi-Square Omnibus Homework Checker
#'
#' Creates a [tinytable::tt()] table for checking omnibus chi-square
#' statistics and sample descriptives for a multi-group analysis.
#'
#' @param rh_name Character. Research hypothesis label.
#' @param chisq_results_list Output from `chi_square_multigroup_answers()`.
#' @param var1_labels Character vector or `NULL`. Labels for variable 1 levels.
#' @param var2_labels Character vector or `NULL`. Labels for variable 2 levels.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @export
create_chisq_omnibus_checker <- function(rh_name, chisq_results_list,
                                         var1_labels = NULL,
                                         var2_labels = NULL) {

  if (!requireNamespace("tinytable", quietly = TRUE))
    stop("Package 'tinytable' is required.")
  if (!requireNamespace("webexercises", quietly = TRUE))
    stop("Package 'webexercises' is required.")

  chi_sq  <- chisq_results_list$ChiSquare$chi_sq
  p_value <- chisq_results_list$ChiSquare$p_value
  df      <- chisq_results_list$ChiSquare$df
  total_n <- chisq_results_list$Sample_Size

  var1_desc <- chisq_results_list$Var1_Descriptives
  var2_desc <- chisq_results_list$Var2_Descriptives

  if (is.null(var1_labels)) var1_labels <- var1_desc$level_label
  if (is.null(var2_labels)) var2_labels <- var2_desc$level_label

  if (p_value < 0.05) {
    posthoc_mcq <- webexercises::mcq(c(
      "No - a nonsignificant Omnibus Chi-Square test",
      answer = "Yes - significant Omnibus Chi-Square test"
    ))
  } else {
    posthoc_mcq <- webexercises::mcq(c(
      answer = "No - a nonsignificant Omnibus Chi-Square test",
      "Yes - significant Omnibus Chi-Square test"
    ))
  }

  chisq_table_data <- tibble::tibble(
    ` `  = paste("Chi-Square:", rh_name),
    chi2 = webexercises::fitb(chi_sq),
    p    = webexercises::fitb(p_value),
    df   = webexercises::fitb(df),
    N    = webexercises::fitb(total_n),
    `  ` = "",
    `Do we need to perform pairwise comparisons?` = posthoc_mcq
  )
  names(chisq_table_data)[2] <- "\u03C7\u00B2"
  chisq_table <- tinytable::tt(chisq_table_data) |>
    tinytable::format_tt(escape = FALSE)

  n_var1 <- nrow(var1_desc)
  n_var2 <- nrow(var2_desc)

  var1_col1 <- sapply(seq_len(n_var1), function(i)
    paste("Number of", var1_labels[i], "in sample"))
  var1_col2 <- sapply(seq_len(n_var1), function(i)
    webexercises::fitb(var1_desc$n[i]))
  var2_col1 <- sapply(seq_len(n_var2), function(i)
    paste("Number of", var2_labels[i], "in sample"))
  var2_col2 <- sapply(seq_len(n_var2), function(i)
    webexercises::fitb(var2_desc$n[i]))

  desc_table_data <- tibble::tibble(
    ` `      = c(var1_col1[1], var2_col1[1]),
    `  `     = c(var1_col2[1], var2_col2[1]),
    `   `    = c(if (n_var1 >= 2) var1_col1[2] else "",
                 if (n_var2 >= 2) var2_col1[2] else ""),
    `    `   = c(if (n_var1 >= 2) var1_col2[2] else "",
                 if (n_var2 >= 2) var2_col2[2] else ""),
    `     `  = c(if (n_var1 >= 3) var1_col1[3] else "", ""),
    `      ` = c(if (n_var1 >= 3) var1_col2[3] else "", ""),
    `       ` = ""
  )
  desc_table <- tinytable::tt(desc_table_data) |>
    tinytable::format_tt(escape = FALSE)

  combined <- tinytable::rbind2(chisq_table, desc_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )

  return(combined)
}


# =============================================================================
# K-GROUP CHI-SQUARE PAIRWISE CHECKER
# =============================================================================

#' Interactive K-Group Chi-Square Pairwise Homework Checker
#'
#' Creates a [tinytable::tt()] table for checking pairwise chi-square
#' comparisons including critical value, percentages, chi-square results,
#' error types, effect sizes, and power assessments.
#'
#' @param chisq_results_list Output from `chi_square_multigroup_answers()`.
#' @param var1_labels Character vector or `NULL`. Optional relabelling.
#' @param var2_labels Character vector or `NULL`. Optional relabelling.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @export
create_chisq_pairwise_checker <- function(chisq_results_list,
                                          var1_labels = NULL,
                                          var2_labels = NULL) {

  if (!requireNamespace("tinytable", quietly = TRUE))
    stop("Package 'tinytable' is required.")
  if (!requireNamespace("webexercises", quietly = TRUE))
    stop("Package 'webexercises' is required.")

  pairwise   <- chisq_results_list$Pairwise
  n_pairwise <- length(pairwise)
  if (n_pairwise == 0) stop("No pairwise comparisons found in results")

  pct_label <- chisq_results_list$pct_var2_label
  if (is.null(pct_label)) pct_label <- "comparison"

  # Relabel
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
    ` `    = "Chi-Square critical",
    `  `   = webexercises::fitb(chi_crit),
    `   ` = "", `    ` = "", `     ` = "", `      ` = ""
  )
  chi_crit_table <- tinytable::tt(chi_crit_data) |>
    tinytable::format_tt(escape = FALSE)

  pct_column_name <- paste0("% ", pct_label)

  pairwise_table_data <- tibble::tibble(
    ` ` = sapply(seq_len(n_pairwise), function(i) pairwise[[i]]$comparison),
    `% comparison` = sapply(seq_len(n_pairwise), function(i) {
      paste0(webexercises::fitb(pairwise[[i]]$pct1), "% vs ",
             webexercises::fitb(pairwise[[i]]$pct2), "%")
    }),
    chi2_result = sapply(seq_len(n_pairwise), function(i) {
      paste0(webexercises::fitb(pairwise[[i]]$chi_sq), " ",
             webexercises::fitb(pairwise[[i]]$chi_result))
    }),
    `Type of Error` = sapply(seq_len(n_pairwise), function(i) {
      is_sig <- pairwise[[i]]$chi_result != "="
      if (is_sig) {
        webexercises::mcq(c(answer = "Type I & III", "Type II"))
      } else {
        webexercises::mcq(c("Type I & III", answer = "Type II"))
      }
    }),
    `Effect Size (r)` = sapply(seq_len(n_pairwise), function(i)
      webexercises::fitb(pairwise[[i]]$effect_size)),
    `Power Problem?` = sapply(seq_len(n_pairwise), function(i) {
      power <- pairwise[[i]]$power_problem
      if (grepl("rejecting H0", power)) {
        webexercises::mcq(c(
          answer = "No - rejecting H0: means there was sufficient power",
          "No - effect is \"too small to be interesting,\" (r < .10)",
          "Yes - The effect is \"large enough to be interesting,\" (r > .10)"
        ))
      } else if (grepl("too small", power)) {
        webexercises::mcq(c(
          "No - rejecting H0: means there was sufficient power",
          answer = "No - effect is \"too small to be interesting,\" (r < .10)",
          "Yes - The effect is \"large enough to be interesting,\" (r > .10)"
        ))
      } else {
        webexercises::mcq(c(
          "No - rejecting H0: means there was sufficient power",
          "No - effect is \"too small to be interesting,\" (r < .10)",
          answer = "Yes - The effect is \"large enough to be interesting,\" (r > .10)"
        ))
      }
    })
  )
  names(pairwise_table_data)[3] <- "\u03C7\u00B2 Result"
  pairwise_table <- tinytable::tt(pairwise_table_data) |>
    tinytable::format_tt(escape = FALSE)

  combined <- tinytable::rbind2(chi_crit_table, pairwise_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 95%; margin-left: auto; margin-right: auto;"
    )

  return(combined)
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

  if (!requireNamespace("tinytable", quietly = TRUE))
    stop("Package 'tinytable' is required.")
  if (!requireNamespace("webexercises", quietly = TRUE))
    stop("Package 'webexercises' is required.")

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

  p_iv1_fmt         <- ifelse(p_iv1 < 0.001, "<.001", sprintf("%.2f", p_iv1))
  p_iv2_fmt         <- ifelse(p_iv2 < 0.001, "<.001", sprintf("%.2f", p_iv2))
  p_interaction_fmt <- ifelse(p_interaction < 0.001, "<.001", sprintf("%.2f", p_interaction))

  if (p_interaction < 0.05) {
    posthoc_mcq <- webexercises::mcq(c(
      "No - a nonsignificant interaction",
      answer = "Yes - significant interaction"
    ))
  } else {
    posthoc_mcq <- webexercises::mcq(c(
      answer = "No - a nonsignificant interaction",
      "Yes - significant interaction"
    ))
  }

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
    ` `                = "Components for LSDmmd:",
    `# of conditions`  = webexercises::fitb(k),
    `average n`        = webexercises::fitb(mean_n),
    `df error`         = webexercises::fitb(df_within),
    `MSe`              = webexercises::fitb(mse),
    `LSDmmd`           = webexercises::fitb(lsd_mmd),
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

  combined <- tinytable::rbind2(interaction_table, lsd_table, use_names = FALSE) |>
    tinytable::rbind2(iv1_table, use_names = FALSE) |>
    tinytable::rbind2(iv2_table, use_names = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )

  return(combined)
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

  if (!requireNamespace("tinytable", quietly = TRUE))
    stop("Package 'tinytable' is required.")
  if (!requireNamespace("webexercises", quietly = TRUE))
    stop("Package 'webexercises' is required.")

  desc_stats <- anova_results_list$Descriptives
  emm_iv1    <- anova_results_list$EMMs$IV1
  emm_iv2    <- anova_results_list$EMMs$IV2

  if (!is.null(anova_results_list$FactorLevels)) {
    iv1_levels_actual <- anova_results_list$FactorLevels$iv1_levels
    iv2_levels_actual <- anova_results_list$FactorLevels$iv2_levels
    final_iv1_labels  <- anova_results_list$FactorLevels$iv1_labels
    final_iv2_labels  <- anova_results_list$FactorLevels$iv2_labels
  } else {
    stop("FactorLevels not found. Please re-run anova_factorial_answers().")
  }

  n_iv1 <- length(final_iv1_labels)
  n_iv2 <- length(final_iv2_labels)

  row_labels <- final_iv1_labels

  # Cell means columns
  col_data <- list()
  for (j in seq_len(n_iv2)) {
    col_values <- c()
    for (i in seq_len(n_iv1)) {
      cell_idx <- which(desc_stats$iv1_level == iv1_levels_actual[i] &
                          desc_stats$iv2_level == iv2_levels_actual[j])
      if (length(cell_idx) > 0) {
        col_values <- c(col_values, webexercises::fitb(desc_stats$mean[cell_idx[1]]))
      } else {
        col_values <- c(col_values, "")
      }
    }
    col_data[[j]] <- col_values
  }

  # EMM column for IV1
  emm_iv1_values <- c()
  for (i in seq_len(n_iv1)) {
    emm_row <- which(emm_iv1$iv1_label == final_iv1_labels[i])
    if (length(emm_row) > 0) {
      emm_iv1_values <- c(emm_iv1_values,
                          webexercises::fitb(round(as.numeric(emm_iv1$mean[emm_row[1]]), 2)))
    } else {
      emm_iv1_values <- c(emm_iv1_values, webexercises::fitb("ERROR"))
    }
  }

  # Footer row: EMMs for IV2
  row_labels <- c(row_labels, paste0("EMM: ", iv2_name))
  for (j in seq_len(n_iv2)) {
    emm_row <- which(emm_iv2$iv2_label == final_iv2_labels[j])
    if (length(emm_row) > 0) {
      col_data[[j]] <- c(col_data[[j]],
                         webexercises::fitb(round(as.numeric(emm_iv2$mean[emm_row[1]]), 2)))
    } else {
      col_data[[j]] <- c(col_data[[j]], webexercises::fitb("ERROR"))
    }
  }
  emm_iv1_values <- c(emm_iv1_values, "")

  table_data <- tibble::tibble(
    ` `    = row_labels,
    `  `   = col_data[[1]],
    `   `  = col_data[[2]],
    `    ` = emm_iv1_values
  )
  colnames(table_data) <- c(iv1_name, final_iv2_labels[1], final_iv2_labels[2],
                            paste0("EMM: ", iv1_name))

  desc_table <- tinytable::tt(table_data) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )

  return(desc_table)
}


# =============================================================================
# REGRESSION MODEL CHECKER
# =============================================================================

#' Interactive Regression Model Summary Homework Checker
#'
#' Creates a [tinytable::tt()] table for checking overall regression model
#' statistics including R, R-squared, F, df, p, and a model significance MCQ.
#'
#' @param reg_results_list Output from [regression_answers()].
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' result <- regression_answers(data, criterion = "dv",
#'   quant_predictors = c("x1", "x2"),
#'   quant_labels = c("Pred 1", "Pred 2"),
#'   criterion_label = "Outcome")
#' create_regression_model_checker(result)
#' }
#'
#' @export
create_regression_model_checker <- function(reg_results_list) {

  if (!requireNamespace("tinytable", quietly = TRUE))
    stop("Package 'tinytable' is required.")
  if (!requireNamespace("webexercises", quietly = TRUE))
    stop("Package 'webexercises' is required.")

  r              <- reg_results_list$Model$R
  r_sq           <- reg_results_list$Model$R_squared
  f_stat         <- reg_results_list$Model$F
  df1            <- reg_results_list$Model$df1
  df2            <- reg_results_list$Model$df2
  p_val_fmt      <- reg_results_list$Model$p_value_formatted

  if (reg_results_list$Model$p_value < 0.05) {
    model_works_mcq <- webexercises::mcq(c(answer = "Yes", "No"))
  } else {
    model_works_mcq <- webexercises::mcq(c("Yes", answer = "No"))
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

  result_table <- tinytable::tt(model_table) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(j = 5,
                        bootstrap_css = "min-width: 120px; white-space: nowrap;") |>
    tinytable::style_tt(
      bootstrap_class    = "table table-bordered table-sm",
      bootstrap_css_rule = "width: 95%; margin-left: auto; margin-right: auto;"
    )

  return(result_table)
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
#' @param reg_results_list Output from [regression_answers()].
#' @param show_legend Logical. If `TRUE` (default), prints a collapsible
#'   significance key and result category legend above the table.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @export
create_regression_predictor_checker <- function(reg_results_list,
                                                show_legend = TRUE) {

  if (!requireNamespace("tinytable", quietly = TRUE))
    stop("Package 'tinytable' is required.")
  if (!requireNamespace("webexercises", quietly = TRUE))
    stop("Package 'webexercises' is required.")

  predictors      <- reg_results_list$Labels$predictors
  predictor_labels <- reg_results_list$Labels$predictor_labels
  predictor_types  <- reg_results_list$Labels$predictor_types

  predictor_rows <- tibble::tibble(
    `Predictor` = character(),
    `Type`      = character(),
    `r`         = character(),
    `r sig`     = character(),
    `b`         = character(),
    `b sig`     = character(),
    `Result`    = character()
  )

  for (i in seq_along(predictors)) {
    p    <- predictors[i]
    bivar <- reg_results_list$Bivariate[[p]]
    regwt <- reg_results_list$Regression_Weights[[p]]

    p_type <- predictor_types[i]
    if (is.na(p_type)) p_type <- predictor_types[p]

    # Type MCQ
    if (p_type == "Binary") {
      type_mcq <- webexercises::mcq(c(answer = "Binary", "Quant"))
    } else {
      type_mcq <- webexercises::mcq(c("Binary", answer = "Quant"))
    }

    r_sig_mcq <- sig_mcq(bivar$p_value)
    b_sig_mcq <- sig_mcq(regwt$p_value)

    # Category MCQ
    cat_choice <- regwt$category
    if (cat_choice == "a") {
      category_mcq <- webexercises::mcq(c(answer = "a", "b", "c", "d"))
    } else if (cat_choice == "b") {
      category_mcq <- webexercises::mcq(c("a", answer = "b", "c", "d"))
    } else if (cat_choice == "c") {
      category_mcq <- webexercises::mcq(c("a", "b", answer = "c", "d"))
    } else {
      category_mcq <- webexercises::mcq(c("a", "b", "c", answer = "d"))
    }

    new_row <- tibble::tibble(
      `Predictor` = predictor_labels[i],
      `Type`      = type_mcq,
      `r`         = webexercises::fitb(bivar$r),
      `r sig`     = r_sig_mcq,
      `b`         = webexercises::fitb(regwt$b),
      `b sig`     = b_sig_mcq,
      `Result`    = category_mcq
    )

    predictor_rows <- dplyr::bind_rows(predictor_rows, new_row)
  }

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

  result_table <- tinytable::tt(predictor_rows) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class    = "table table-striped table-bordered table-sm",
      bootstrap_css_rule = "width: 95%; margin-left: auto; margin-right: auto;"
    )

  return(result_table)
}
