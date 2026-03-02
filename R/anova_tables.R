# =============================================================================
# anova_tables.R
# APA-Style ANOVA Tables and Formatted Results
# =============================================================================
# Creates publication-ready ANOVA tables using flextable.
# Supports both between-groups and within-groups (repeated measures) designs.
# Can generate filled answer KEYs or blank student worksheets.
#
# Updated for dplyr 1.2.0 (February 2026)
# =============================================================================


# -----------------------------------------------------------------------------
# INTERNAL HELPERS
# -----------------------------------------------------------------------------

#' Format p-value for APA style
#' @noRd
.format_p_anova <- function(p_value) {
  if (p_value < 0.001) "< .001" else sprintf("%.3f", p_value)
}


#' Determine H0 decision based on p-value
#' @noRd
.get_h0_decision <- function(p_value, alpha = 0.05) {
  if (p_value < alpha) "Reject H0" else "Retain H0"
}


#' Determine research hypothesis support
#' @noRd
.get_rh_support <- function(p_value, alpha = 0.05) {
  if (p_value < alpha) "Yes" else "No"
}


#' Detect ANOVA type from results structure
#' @noRd
.detect_anova_type <- function(anova_results_list) {
  desc <- anova_results_list$Descriptives
  anova <- anova_results_list$ANOVA

  # Factorial: has Interaction component in ANOVA
  if (!is.null(anova$Interaction)) {
    return("factorial")
  }

  # K-group: has group_label column
  if ("group_label" %in% names(desc)) {
    return("kgroup")
  }

  # Two-group BG ANOVA: has iv column
  if ("iv" %in% names(desc)) {
    return("bg")
  }

  return("bg")
}


#' Extract group labels from descriptives based on ANOVA type
#' @noRd
.extract_group_labels <- function(desc_stats, anova_type) {
  switch(anova_type,
         "factorial" = as.character(desc_stats$group_label),
         "kgroup" = as.character(desc_stats$group_label),
         "bg" = as.character(desc_stats$iv),
         as.character(desc_stats$iv)
  )
}


# =============================================================================
# FORMAT FUNCTIONS (Fill-in-the-Blank Style)
# =============================================================================

#' Format Between-Groups ANOVA Results for Fill-in-the-Blank
#'
#' Returns formatted between-groups ANOVA results as either a filled-in
#' answer KEY or a blank student worksheet.
#'
#' @param rh_name Character. Research hypothesis name/label.
#' @param vars Character vector. Variable names (for display purposes).
#' @param anova_results_list Output from [bg_anova_answers()].
#' @param iv_labels Character vector or `NULL`. Display labels for IV levels.
#'   Must have the same number of elements as groups in the ANOVA.
#' @param KEY Logical. If `TRUE` (default), show answers; if `FALSE`, show blanks.
#'
#' @return A character string with formatted results.
#'
#' @examples
#' data(superman)
#' result <- bg_anova_answers(superman, iv = "clark_grp", dv = "rt_critics_score")
#'
#' # Answer KEY
#' cat(format_bg_anova_results("RH1", c("clark_grp", "rt_critics_score"),
#'   result, iv_labels = c("Under 6ft", "6ft+")))
#'
#' # Blank version
#' cat(format_bg_anova_results("RH1", c("clark_grp", "rt_critics_score"),
#'   result, iv_labels = c("Under 6ft", "6ft+"), KEY = FALSE))
#'
#' @export
format_bg_anova_results <- function(rh_name, vars, anova_results_list,
                                    iv_labels = NULL, KEY = TRUE) {

  if (!KEY) {
    # Blank version for student worksheets
    output <- glue::glue("
#### {rh_name} Results

**Between-Groups ANOVA**

For {iv_labels[1]}:    n = ___    Mean = ___    SD = ___

For {iv_labels[2]}:    n = ___    Mean = ___    SD = ___

F = ___    df = ___, ___    p = ___    MSE = ___

**State the H0:**

**Retain or reject H0?**

**Support research hypothesis?**
")
  } else {
    # Answer KEY version
    desc <- anova_results_list$Descriptives
    anova <- anova_results_list$ANOVA

    # Format p-value and decisions using helpers
    p_text <- .format_p_anova(anova$p_value)
    h0_decision <- .get_h0_decision(anova$p_value)
    rh_support <- .get_rh_support(anova$p_value)

    output <- glue::glue("
#### {rh_name} Results

**Between-Groups ANOVA**

For {iv_labels[1]}:    n = {desc$n[1]}    Mean = {desc$mean[1]}    SD = {desc$sd[1]}

For {iv_labels[2]}:    n = {desc$n[2]}    Mean = {desc$mean[2]}    SD = {desc$sd[2]}

F = {anova$F}    df = {anova$df_between}, {anova$df_within}    p = {p_text}    MSE = {anova$mse}

State the H0: There is no difference in {vars[2]} between {iv_labels[1]} and {iv_labels[2]}

Retain or reject H0? {h0_decision}

Support research hypothesis? {rh_support}
")
  }

  return(as.character(output))
}


#' Format Within-Groups ANOVA Results for Fill-in-the-Blank
#'
#' Returns formatted repeated measures ANOVA results as either a filled-in
#' answer KEY or a blank student worksheet.
#'
#' @param rh_name Character. Research hypothesis name/label.
#' @param vars Character vector. Variable names (for display purposes).
#' @param anova_results_list Output from [wg_anova_answers()].
#' @param condition_labels Character vector or `NULL`. Display labels for conditions.
#'   Must have 2 elements.
#' @param KEY Logical. If `TRUE` (default), show answers; if `FALSE`, show blanks.
#'
#' @return A character string with formatted results.
#'
#' @examples
#' data(superman)
#' result <- wg_anova_answers(superman,
#'   dv1 = "rt_critics_score", dv2 = "rt_audience_score")
#'
#' # Answer KEY
#' cat(format_wg_anova_results("RH1",
#'   c("rt_critics_score", "rt_audience_score"), result,
#'   condition_labels = c("Critics", "Audience")))
#'
#' # Blank version
#' cat(format_wg_anova_results("RH1",
#'   c("rt_critics_score", "rt_audience_score"), result,
#'   condition_labels = c("Critics", "Audience"), KEY = FALSE))
#'
#' @export
format_wg_anova_results <- function(rh_name, vars, anova_results_list,
                                    condition_labels = NULL, KEY = TRUE) {

  if (!KEY) {
    # Blank version for student worksheets
    output <- glue::glue("
#### {rh_name} Results

**Within-Groups ANOVA**

For {condition_labels[1]}:    n = ___    Mean = ___    SD = ___

For {condition_labels[2]}:    n = ___    Mean = ___    SD = ___

F = ___    df = ___, ___    p = ___    MSE = ___

**State the H0:**

**Retain or reject H0?**

**Support research hypothesis?**
")
  } else {
    # Answer KEY version
    desc <- anova_results_list$Descriptives
    anova <- anova_results_list$ANOVA

    # Format p-value and decisions using helpers
    p_text <- .format_p_anova(anova$p_value)
    h0_decision <- .get_h0_decision(anova$p_value)
    rh_support <- .get_rh_support(anova$p_value)

    output <- glue::glue("
#### {rh_name} Results

**Within-Groups ANOVA**

For {condition_labels[1]}:    n = {desc$n[1]}    Mean = {desc$mean[1]}    SD = {desc$sd[1]}

For {condition_labels[2]}:    n = {desc$n[2]}    Mean = {desc$mean[2]}    SD = {desc$sd[2]}

F = {anova$F}    df = {anova$df_effect}, {anova$df_error}    p = {p_text}    MSE = {anova$mse}

State the H0: There is no difference in scores between {condition_labels[1]} and {condition_labels[2]}

Retain or reject H0? {h0_decision}

Support research hypothesis? {rh_support}
")
  }

  return(as.character(output))
}


# =============================================================================
# DESCRIPTIVE STATISTICS TABLES
# =============================================================================

#' APA Descriptive Statistics Table for Between-Groups ANOVA
#'
#' Creates an APA-formatted descriptive statistics table for between-groups
#' ANOVA designs, showing mean and SD for each group. Compatible with output
#' from [bg_anova_answers()], [anova_kgroup_answers()], and
#' [anova_factorial_answers()].
#'
#' @param anova_results_list Output from [bg_anova_answers()],
#'   [anova_kgroup_answers()], or [anova_factorial_answers()]. Can be `NULL`
#'   for a blank table template.
#' @param iv_name Character. Display name for the independent variable.
#' @param dv_name Character. Display name for the dependent variable.
#' @param group_labels Character vector or `NULL`. Display labels for groups.
#'   If `NULL` and `KEY = TRUE`, labels are extracted from the results.
#'   If `NULL` and `KEY = FALSE`, defaults to "Group 1", "Group 2", "Group 3".
#' @param KEY Logical. If `TRUE` (default), fill with computed values.
#'   If `FALSE`, create a blank template.
#' @param table_title Character or `NULL`. Optional table caption.
#' @param table_number Integer or `NULL`. Optional table number.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' # Two-group design (bg_anova_answers)
#' data(superman)
#' result_2g <- bg_anova_answers(superman, iv = "clark_grp", dv = "rt_critics_score")
#' create_bg_anova_table(result_2g,
#'   iv_name = "Height Group", dv_name = "Critics Score",
#'   group_labels = c("Under 6ft", "6ft+"))
#'
#' # Blank template
#' create_bg_anova_table(NULL,
#'   iv_name = "Condition", dv_name = "Score",
#'   group_labels = c("Low", "Medium", "High"),
#'   KEY = FALSE)
#'
#' @export
create_bg_anova_table <- function(anova_results_list = NULL,
                                  iv_name = "Independent Variable",
                                  dv_name = "Dependent Variable",
                                  group_labels = NULL,
                                  KEY = TRUE,
                                  table_title = NULL,
                                  table_number = NULL) {

  # Build data based on KEY
  if (KEY && !is.null(anova_results_list)) {
    desc_stats <- anova_results_list$Descriptives
    anova_type <- .detect_anova_type(anova_results_list)

    # Extract group labels based on ANOVA type
    if (is.null(group_labels)) {
      group_labels <- .extract_group_labels(desc_stats, anova_type)
    }

    # Build data frame with groups as columns
    data <- data.frame(
      Statistic = c("Mean", "Standard Deviation"),
      check.names = FALSE
    )

    for (i in seq_along(group_labels)) {
      data[[group_labels[i]]] <- c(
        sprintf("%.2f", desc_stats$mean[i]),
        sprintf("%.2f", desc_stats$sd[i])
      )
    }

  } else {
    # Blank table template
    if (is.null(group_labels)) {
      group_labels <- c("Group 1", "Group 2", "Group 3")
    }

    data <- data.frame(
      Statistic = c("Mean", "Standard Deviation"),
      check.names = FALSE
    )

    for (label in group_labels) {
      data[[label]] <- rep("", 2)
    }
  }

  # Create flextable
  apa_table <- data |>
    flextable::flextable() |>
    flextable::set_table_properties(layout = "autofit", align = "left") |>
    flextable::set_header_labels(Statistic = dv_name)

  # Add IV header row
  apa_table <- apa_table |>
    flextable::add_header_row(
      values = c("", iv_name),
      colwidths = c(1, length(group_labels)),
      top = TRUE
    )

  # Add title rows if provided
  has_title <- !is.null(table_title)
  has_number <- !is.null(table_number)

  if (has_number && has_title) {
    apa_table <- apa_table |>
      flextable::add_header_lines(values = table_title, top = TRUE) |>
      flextable::add_header_lines(values = paste0("Table ", table_number), top = TRUE)
    title_rows <- 2
  } else if (has_title) {
    apa_table <- apa_table |>
      flextable::add_header_lines(values = table_title, top = TRUE)
    title_rows <- 1
  } else {
    title_rows <- 0
  }

  # Get header row count after all additions
  n_header_rows <- flextable::nrow_part(apa_table, part = "header")

  # Apply APA-style formatting
  apa_table <- apa_table |>
    flextable::border_remove()

  # Add title underline only if there are title rows
  if (title_rows > 0) {
    apa_table <- apa_table |>
      flextable::hline(i = title_rows, part = "header",
                       border = officer::fp_border(width = 2))
  }

  # Add remaining borders and formatting
  apa_table <- apa_table |>
    flextable::hline(i = n_header_rows - 1, part = "header",
                     border = officer::fp_border(width = 1)) |>
    flextable::hline_bottom(part = "header",
                            border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "body",
                            border = officer::fp_border(width = 2)) |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "body")

  # Align header rows - handle title rows separately
  if (title_rows > 0) {
    apa_table <- apa_table |>
      flextable::align(j = 1, align = "left", part = "header",
                       i = seq_len(title_rows))
  }

  # Align IV/DV header rows
  apa_table <- apa_table |>
    flextable::align(j = 1, align = "left", part = "header",
                     i = (title_rows + 1):n_header_rows) |>
    flextable::fontsize(size = 11, part = "all") |>
    flextable::italic(i = title_rows + 1, part = "header") |>
    flextable::italic(i = title_rows + 2, part = "header")

  return(apa_table)
}


#' APA Descriptive Statistics Table for Within-Groups ANOVA
#'
#' Creates an APA-formatted descriptive statistics table for a repeated
#' measures ANOVA, showing mean, SD, and n for each condition. Can be
#' filled (answer KEY) or blank (student worksheet).
#'
#' @param anova_results_list Output from [wg_anova_answers()] or `NULL`
#'   (for blank table).
#' @param dv_name Character. Display name for the dependent variable.
#'   Used as the row header label.
#' @param condition_labels Character vector or `NULL`. Display labels for
#'   conditions. If `NULL` and `KEY = TRUE`, uses condition names from results.
#'   If `NULL` and `KEY = FALSE`, defaults to "Condition 1", "Condition 2".
#' @param KEY Logical. If `TRUE` (default), fill with computed values.
#'   If `FALSE`, create a blank template.
#' @param table_title Character or `NULL`. Optional table caption.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman)
#' result <- wg_anova_answers(superman,
#'   dv1 = "rt_critics_score", dv2 = "rt_audience_score")
#'
#' # Filled table
#' ft <- create_wg_anova_table(result,
#'   dv_name = "Rating",
#'   condition_labels = c("Critics Score", "Audience Score"),
#'   table_title = "Table 1. Descriptive Statistics for Ratings")
#' ft
#'
#' # Blank table
#' ft_blank <- create_wg_anova_table(NULL,
#'   dv_name = "Rating",
#'   condition_labels = c("Critics Score", "Audience Score"),
#'   KEY = FALSE)
#' ft_blank
#'
#' @export
create_wg_anova_table <- function(anova_results_list = NULL,
                                  dv_name = "Dependent Variable",
                                  condition_labels = NULL,
                                  KEY = TRUE,
                                  table_title = NULL) {

  if (KEY && !is.null(anova_results_list)) {
    # FILLED TABLE - extract data from results
    desc_stats <- anova_results_list$Descriptives

    # Use labels if provided, otherwise use condition names from data
    if (is.null(condition_labels)) {
      condition_labels <- as.character(desc_stats$condition)
    }

    # Create data frame with descriptive stats
    data <- tibble::tibble(
      Condition = condition_labels,
      Mean = as.character(desc_stats$mean),
      SD = as.character(desc_stats$sd),
      N = as.character(desc_stats$n)
    )

  } else {
    # BLANK TABLE - empty template
    if (is.null(condition_labels)) {
      condition_labels <- c("Condition 1", "Condition 2")
    }

    data <- tibble::tibble(
      Condition = condition_labels,
      Mean = rep("", length(condition_labels)),
      SD = rep("", length(condition_labels)),
      N = rep("", length(condition_labels))
    )
  }

  # Create the flextable
  apa_table <- data |>
    flextable::flextable() |>
    flextable::set_table_properties(layout = "autofit", align = "left") |>
    flextable::set_header_labels(
      Condition = dv_name,
      Mean = "M",
      SD = "SD",
      N = "n"
    )

  # Add title if provided
  if (!is.null(table_title)) {
    apa_table <- apa_table |>
      flextable::set_caption(caption = table_title)
  }

  # APA-style formatting
  apa_table <- apa_table |>
    flextable::border_remove() |>
    flextable::hline_top(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "body", border = officer::fp_border(width = 2)) |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = "Condition", align = "left", part = "all") |>
    flextable::fontsize(size = 11, part = "all")

  return(apa_table)
}


# =============================================================================
# ANOVA SOURCE TABLE
# =============================================================================

#' APA ANOVA Source Table
#'
#' Creates an APA-formatted ANOVA source table showing Source, df, MS, F,
#' and p for a between-groups ANOVA. This provides the full ANOVA source table
#' rather than just descriptive statistics.
#'
#' @param anova_results_list Output from [bg_anova_answers()].
#' @param iv_name Character. Display name for the independent variable.
#' @param dv_name Character. Display name for the dependent variable.
#' @param KEY Logical. If `TRUE` (default), fill with computed values.
#'   If `FALSE`, create a blank template.
#' @param table_title Character or `NULL`. Optional table caption.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman)
#' result <- bg_anova_answers(superman, iv = "clark_grp", dv = "rt_critics_score")
#' ft <- create_anova_source_table(result,
#'   iv_name = "Height Group", dv_name = "Critics Score")
#' ft
#'
#' @export
create_anova_source_table <- function(anova_results_list = NULL,
                                      iv_name = "Independent Variable",
                                      dv_name = "Dependent Variable",
                                      KEY = TRUE,
                                      table_title = NULL) {

  if (KEY && !is.null(anova_results_list)) {
    # Extract ANOVA statistics
    anova <- anova_results_list$ANOVA

    # Format p-value using helper
    p_text <- .format_p_anova(anova$p_value)

    # Create the source table data
    data <- tibble::tibble(
      Source = c(
        paste0("Between Groups (", iv_name, ")"),
        "Within Groups (Error)",
        "Total"
      ),
      df = c(
        as.character(anova$df_between),
        as.character(anova$df_within),
        as.character(anova$df_between + anova$df_within)
      ),
      MS = c("", as.character(anova$mse), ""),
      `F` = c(as.character(anova$F), "", ""),
      p = c(p_text, "", "")
    )

  } else {
    # BLANK TABLE
    data <- tibble::tibble(
      Source = c(
        paste0("Between Groups (", iv_name, ")"),
        "Within Groups (Error)",
        "Total"
      ),
      df = rep("", 3),
      MS = rep("", 3),
      `F` = rep("", 3),
      p = rep("", 3)
    )
  }

  # Create the flextable
  apa_table <- data |>
    flextable::flextable() |>
    flextable::set_table_properties(layout = "autofit", align = "left")

  # Add title if provided
  if (!is.null(table_title)) {
    apa_table <- apa_table |>
      flextable::set_caption(caption = table_title)
  }

  # APA-style formatting
  apa_table <- apa_table |>
    flextable::border_remove() |>
    flextable::hline_top(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "body", border = officer::fp_border(width = 2)) |>
    flextable::hline(i = 2, part = "body", border = officer::fp_border(width = 1)) |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = "Source", align = "left", part = "all") |>
    flextable::fontsize(size = 11, part = "all") |>
    flextable::width(j = "Source", width = 3)

  return(apa_table)
}


# =============================================================================
# COMBINED TABLES (Descriptives + ANOVA Results)
# =============================================================================

#' Combined Between-Groups ANOVA Results and Descriptives Table
#'
#' Creates a single flextable that combines the ANOVA test results
#' (F, df, p, MSE) with the descriptive statistics for each group.
#' Useful for compact reporting or worksheets.
#'
#' @param anova_results_list Output from [bg_anova_answers()].
#' @param rh_name Character. Research hypothesis name/label.
#' @param iv_name Character. Display name for the IV.
#' @param dv_name Character. Display name for the DV.
#' @param iv_labels Character vector or `NULL`. Labels for IV levels.
#' @param KEY Logical. If `TRUE`, fill with values; if `FALSE`, blank.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman)
#' result <- bg_anova_answers(superman, iv = "clark_grp", dv = "rt_critics_score")
#' ft <- create_bg_anova_combined_table(result,
#'   rh_name = "RH1",
#'   iv_name = "Height Group", dv_name = "Critics Score",
#'   iv_labels = c("Under 6ft", "6ft+"))
#' ft
#'
#' @export
create_bg_anova_combined_table <- function(anova_results_list,
                                           rh_name = "RH1",
                                           iv_name = "Independent Variable",
                                           dv_name = "Dependent Variable",
                                           iv_labels = NULL,
                                           KEY = TRUE) {

  if (KEY && !is.null(anova_results_list)) {
    desc_stats <- anova_results_list$Descriptives
    anova <- anova_results_list$ANOVA

    if (is.null(iv_labels)) {
      iv_labels <- as.character(desc_stats$iv)
    }

    # Format p-value and decision using helpers
    p_formatted <- .format_p_anova(anova$p_value)
    decision <- .get_h0_decision(anova$p_value)

    # Build combined table: row 1 = ANOVA stats, rows 2+ = group descriptives
    combined_data <- tibble::tibble(
      ` ` = c(
        paste0("ANOVA: ", rh_name),
        paste0("  ", iv_labels[1]),
        paste0("  ", iv_labels[2])
      ),
      Column2 = c(
        as.character(anova$F),
        as.character(desc_stats$mean[1]),
        as.character(desc_stats$mean[2])
      ),
      Column3 = c(
        p_formatted,
        as.character(desc_stats$sd[1]),
        as.character(desc_stats$sd[2])
      ),
      Column4 = c(
        paste0(anova$df_between, ", ", anova$df_within),
        as.character(desc_stats$n[1]),
        as.character(desc_stats$n[2])
      ),
      Column5 = c(as.character(anova$mse), "", ""),
      Column6 = c(decision, "", "")
    )

  } else {
    if (is.null(iv_labels)) {
      iv_labels <- c("Group 1", "Group 2")
    }

    combined_data <- tibble::tibble(
      ` ` = c(
        paste0("ANOVA: ", rh_name),
        paste0("  ", iv_labels[1]),
        paste0("  ", iv_labels[2])
      ),
      Column2 = rep("", 3),
      Column3 = rep("", 3),
      Column4 = rep("", 3),
      Column5 = rep("", 3),
      Column6 = rep("", 3)
    )
  }

  ft <- combined_data |>
    flextable::flextable() |>
    flextable::set_header_labels(
      ` ` = " ",
      Column2 = "F / Mean",
      Column3 = "p / SD",
      Column4 = "df / N",
      Column5 = "MSE",
      Column6 = "Decision"
    ) |>
    flextable::theme_box() |>
    flextable::autofit() |>
    flextable::fontsize(size = 10, part = "all")

  return(ft)
}


#' Combined Within-Groups ANOVA Results and Descriptives Table
#'
#' Creates a single flextable that combines the repeated measures ANOVA
#' test results with descriptive statistics for each condition.
#' Useful for compact reporting or worksheets.
#'
#' @param anova_results_list Output from [wg_anova_answers()].
#' @param rh_name Character. Research hypothesis name/label.
#' @param dv_name Character. Display name for the DV.
#' @param condition_labels Character vector or `NULL`. Labels for conditions.
#' @param KEY Logical. If `TRUE`, fill with values; if `FALSE`, blank.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman)
#' result <- wg_anova_answers(superman,
#'   dv1 = "rt_critics_score", dv2 = "rt_audience_score")
#' ft <- create_wg_anova_combined_table(result,
#'   rh_name = "RH1",
#'   dv_name = "Rating",
#'   condition_labels = c("Critics Score", "Audience Score"))
#' ft
#'
#' @export
create_wg_anova_combined_table <- function(anova_results_list,
                                           rh_name = "RH1",
                                           dv_name = "Dependent Variable",
                                           condition_labels = NULL,
                                           KEY = TRUE) {

  if (KEY && !is.null(anova_results_list)) {
    desc_stats <- anova_results_list$Descriptives
    anova <- anova_results_list$ANOVA

    if (is.null(condition_labels)) {
      condition_labels <- as.character(desc_stats$condition)
    }

    # Format p-value and decision using helpers
    p_formatted <- .format_p_anova(anova$p_value)
    decision <- .get_h0_decision(anova$p_value)

    combined_data <- tibble::tibble(
      ` ` = c(
        paste0("RM ANOVA: ", rh_name),
        paste0("  ", condition_labels[1]),
        paste0("  ", condition_labels[2])
      ),
      Column2 = c(
        as.character(anova$F),
        as.character(desc_stats$mean[1]),
        as.character(desc_stats$mean[2])
      ),
      Column3 = c(
        p_formatted,
        as.character(desc_stats$sd[1]),
        as.character(desc_stats$sd[2])
      ),
      Column4 = c(
        paste0(anova$df_effect, ", ", anova$df_error),
        as.character(desc_stats$n[1]),
        as.character(desc_stats$n[2])
      ),
      Column5 = c(as.character(anova$mse), "", ""),
      Column6 = c(decision, "", "")
    )

  } else {
    if (is.null(condition_labels)) {
      condition_labels <- c("Condition 1", "Condition 2")
    }

    combined_data <- tibble::tibble(
      ` ` = c(
        paste0("RM ANOVA: ", rh_name),
        paste0("  ", condition_labels[1]),
        paste0("  ", condition_labels[2])
      ),
      Column2 = rep("", 3),
      Column3 = rep("", 3),
      Column4 = rep("", 3),
      Column5 = rep("", 3),
      Column6 = rep("", 3)
    )
  }

  ft <- combined_data |>
    flextable::flextable() |>
    flextable::set_header_labels(
      ` ` = " ",
      Column2 = "F / Mean",
      Column3 = "p / SD",
      Column4 = "df / N",
      Column5 = "MSE",
      Column6 = "Decision"
    ) |>
    flextable::theme_box() |>
    flextable::autofit() |>
    flextable::fontsize(size = 10, part = "all")

  return(ft)
}


# =============================================================================
# LSD PAIRWISE COMPARISONS TABLE
# =============================================================================

#' LSD Pairwise Comparisons Table (Answer KEY)
#'
#' Creates a flextable showing LSD pairwise comparison results with
#' comparisons as columns.
#'
#' @param anova_results_list Output from [anova_kgroup_answers()].
#' @param KEY Logical. If `TRUE` (default), fill with values; if `FALSE`, blank.
#' @param group_labels Character vector or `NULL`. Display labels for groups.
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

    for (i in seq_len(n_pairwise)) {
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

    for (i in seq_len(n_pairwise)) {
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
    ft <- ft |>
      flextable::color(i = 2:6, j = 2:(n_pairwise + 1), color = "#d00000", part = "body")
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
