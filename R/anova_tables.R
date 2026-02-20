#' Format Between-Groups ANOVA Results for Fill-in-the-Blank
#'
#' Returns formatted between-groups ANOVA results as either a filled-in
#' answer key or a blank student worksheet.
#'
#' @param rh_name Character. Research hypothesis name/label.
#' @param vars Character vector. Variable names (for display purposes).
#' @param anova_results_list Output from [bg_anova_answers()].
#' @param iv_labels Character vector or `NULL`. Display labels for IV levels.
#'   Must have the same number of elements as groups in the ANOVA.
#' @param Key Logical. If `TRUE` (default), show answers; if `FALSE`, show blanks.
#'
#' @return A character string with formatted results.
#'
#' @examples
#' data(superman)
#' result <- bg_anova_answers(superman, iv = "clark_grp", dv = "rt_critics_score")
#'
#' # Answer key
#' cat(format_bg_anova_results("RH1", c("clark_grp", "rt_critics_score"),
#'   result, iv_labels = c("Under 6ft", "6ft+")))
#'
#' # Blank version
#' cat(format_bg_anova_results("RH1", c("clark_grp", "rt_critics_score"),
#'   result, iv_labels = c("Under 6ft", "6ft+"), Key = FALSE))
#'
#' @export
format_bg_anova_results <- function(rh_name, vars, anova_results_list,
                                    iv_labels = NULL, Key = TRUE) {

  if (!Key) {
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
    # Answer key version
    desc <- anova_results_list$Descriptives
    anova <- anova_results_list$ANOVA

    # Format p-value
    p_text <- if (anova$p_value < 0.001) {
      "< .001"
    } else {
      sprintf("%.3f", anova$p_value)
    }

    # Determine H0 decision
    if (anova$p_value < 0.05) {
      h0_decision <- "Reject H0"
      rh_support <- "Yes"
    } else {
      h0_decision <- "Retain H0"
      rh_support <- "No"
    }

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


#' APA Descriptive Statistics Table for Between-Groups ANOVA
#'
#' Creates an APA-formatted descriptive statistics table for a between-groups
#' ANOVA, showing mean, SD, and n for each group. Can be filled (answer key)
#' or blank (student worksheet).
#'
#' @param anova_results_list Output from [bg_anova_answers()] or `NULL`
#'   (for blank table).
#' @param iv_name Character. Display name for the independent variable.
#'   Used as the row header label.
#' @param dv_name Character. Display name for the dependent variable.
#'   Used in the table title if provided.
#' @param iv_labels Character vector or `NULL`. Display labels for IV levels.
#'   If `NULL` and `Key = TRUE`, uses factor levels from the results.
#'   If `NULL` and `Key = FALSE`, defaults to "Group 1", "Group 2".
#' @param Key Logical. If `TRUE` (default), fill with computed values.
#'   If `FALSE`, create a blank template.
#' @param table_title Character or `NULL`. Optional table caption.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman)
#' result <- bg_anova_answers(superman, iv = "clark_grp", dv = "rt_critics_score")
#'
#' # Filled table
#' ft <- create_bg_anova_table(result,
#'   iv_name = "Height Group", dv_name = "Critics Score",
#'   iv_labels = c("Under 6ft", "6ft+"),
#'   table_title = "Table 1. Descriptive Statistics for Critics Score by Height Group")
#' ft
#'
#' # Blank table
#' ft_blank <- create_bg_anova_table(
#'   iv_name = "Height Group", dv_name = "Critics Score",
#'   iv_labels = c("Under 6ft", "6ft+"),
#'   Key = FALSE)
#' ft_blank
#'
#' @export
create_bg_anova_table <- function(anova_results_list = NULL,
                                  iv_name = "Independent Variable",
                                  dv_name = "Dependent Variable",
                                  iv_labels = NULL,
                                  Key = TRUE,
                                  table_title = NULL) {

  if (Key && !is.null(anova_results_list)) {
    # FILLED TABLE - extract data from results
    desc_stats <- anova_results_list$Descriptives

    # Use labels if provided, otherwise use factor levels from data
    if (is.null(iv_labels)) {
      iv_labels <- as.character(desc_stats$iv)
    }

    # Create data frame with descriptive stats
    data <- data.frame(
      Group = iv_labels,
      Mean = as.character(desc_stats$mean),
      SD = as.character(desc_stats$sd),
      N = as.character(desc_stats$n),
      check.names = FALSE
    )

  } else {
    # BLANK TABLE - empty template
    if (is.null(iv_labels)) {
      iv_labels <- c("Group 1", "Group 2")
    }

    data <- data.frame(
      Group = iv_labels,
      Mean = rep("", length(iv_labels)),
      SD = rep("", length(iv_labels)),
      N = rep("", length(iv_labels)),
      check.names = FALSE
    )
  }

  # Create the flextable
  apa_table <- data |>
    flextable::flextable() |>
    flextable::set_table_properties(layout = "autofit", align = "left") |>
    flextable::set_header_labels(
      Group = iv_name,
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
    flextable::align(j = "Group", align = "left", part = "all") |>
    flextable::fontsize(size = 11, part = "all")

  return(apa_table)
}


#' Format Within-Groups ANOVA Results for Fill-in-the-Blank
#'
#' Returns formatted repeated measures ANOVA results as either a filled-in
#' answer key or a blank student worksheet.
#'
#' @param rh_name Character. Research hypothesis name/label.
#' @param vars Character vector. Variable names (for display purposes).
#' @param anova_results_list Output from [wg_anova_answers()].
#' @param condition_labels Character vector or `NULL`. Display labels for conditions.
#'   Must have 2 elements.
#' @param Key Logical. If `TRUE` (default), show answers; if `FALSE`, show blanks.
#'
#' @return A character string with formatted results.
#'
#' @examples
#' data(superman)
#' result <- wg_anova_answers(superman,
#'   dv1 = "rt_critics_score", dv2 = "rt_audience_score")
#'
#' # Answer key
#' cat(format_wg_anova_results("RH1",
#'   c("rt_critics_score", "rt_audience_score"), result,
#'   condition_labels = c("Critics", "Audience")))
#'
#' # Blank version
#' cat(format_wg_anova_results("RH1",
#'   c("rt_critics_score", "rt_audience_score"), result,
#'   condition_labels = c("Critics", "Audience"), Key = FALSE))
#'
#' @export
format_wg_anova_results <- function(rh_name, vars, anova_results_list,
                                    condition_labels = NULL, Key = TRUE) {

  if (!Key) {
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
    # Answer key version
    desc <- anova_results_list$Descriptives
    anova <- anova_results_list$ANOVA

    # Format p-value
    p_text <- if (anova$p_value < 0.001) {
      "< .001"
    } else {
      sprintf("%.3f", anova$p_value)
    }

    # Determine H0 decision
    if (anova$p_value < 0.05) {
      h0_decision <- "Reject H0"
      rh_support <- "Yes"
    } else {
      h0_decision <- "Retain H0"
      rh_support <- "No"
    }

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


#' APA Descriptive Statistics Table for Within-Groups ANOVA
#'
#' Creates an APA-formatted descriptive statistics table for a repeated
#' measures ANOVA, showing mean, SD, and n for each condition. Can be
#' filled (answer key) or blank (student worksheet).
#'
#' @param anova_results_list Output from [wg_anova_answers()] or `NULL`
#'   (for blank table).
#' @param dv_name Character. Display name for the dependent variable.
#'   Used as the row header label.
#' @param condition_labels Character vector or `NULL`. Display labels for
#'   conditions. If `NULL` and `Key = TRUE`, uses condition names from results.
#'   If `NULL` and `Key = FALSE`, defaults to "Condition 1", "Condition 2".
#' @param Key Logical. If `TRUE` (default), fill with computed values.
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
#' ft_blank <- create_wg_anova_table(
#'   dv_name = "Rating",
#'   condition_labels = c("Critics Score", "Audience Score"),
#'   Key = FALSE)
#' ft_blank
#'
#' @export
create_wg_anova_table <- function(anova_results_list = NULL,
                                  dv_name = "Dependent Variable",
                                  condition_labels = NULL,
                                  Key = TRUE,
                                  table_title = NULL) {

  if (Key && !is.null(anova_results_list)) {
    # FILLED TABLE - extract data from results
    desc_stats <- anova_results_list$Descriptives

    # Use labels if provided, otherwise use condition names from data
    if (is.null(condition_labels)) {
      condition_labels <- as.character(desc_stats$condition)
    }

    # Create data frame with descriptive stats
    data <- data.frame(
      Condition = condition_labels,
      Mean = as.character(desc_stats$mean),
      SD = as.character(desc_stats$sd),
      N = as.character(desc_stats$n),
      check.names = FALSE
    )

  } else {
    # BLANK TABLE - empty template
    if (is.null(condition_labels)) {
      condition_labels <- c("Condition 1", "Condition 2")
    }

    data <- data.frame(
      Condition = condition_labels,
      Mean = rep("", length(condition_labels)),
      SD = rep("", length(condition_labels)),
      N = rep("", length(condition_labels)),
      check.names = FALSE
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


#' APA ANOVA Summary Table
#'
#' Creates an APA-formatted ANOVA summary table showing Source, SS, df, MS, F,
#' and p for a between-groups ANOVA. This provides the full ANOVA source table
#' rather than just descriptive statistics.
#'
#' @param anova_results_list Output from [bg_anova_answers()].
#' @param iv_name Character. Display name for the independent variable.
#' @param dv_name Character. Display name for the dependent variable.
#' @param Key Logical. If `TRUE` (default), fill with computed values.
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
                                      Key = TRUE,
                                      table_title = NULL) {

  if (Key && !is.null(anova_results_list)) {
    # Extract ANOVA statistics
    anova <- anova_results_list$ANOVA

    # Format p-value
    if (anova$p_value < 0.001) {
      p_text <- "< .001"
    } else {
      p_text <- sprintf("%.3f", anova$p_value)
    }

    # Create the source table data
    data <- data.frame(
      Source = c(paste("Between Groups (", iv_name, ")", sep = ""),
                 "Within Groups (Error)",
                 "Total"),
      df = c(as.character(anova$df_between),
             as.character(anova$df_within),
             as.character(anova$df_between + anova$df_within)),
      MS = c("", as.character(anova$mse), ""),
      F_val = c(as.character(anova$F), "", ""),
      p = c(p_text, "", ""),
      check.names = FALSE
    )

    names(data) <- c("Source", "df", "MS", "F", "p")

  } else {
    # BLANK TABLE
    data <- data.frame(
      Source = c(paste("Between Groups (", iv_name, ")", sep = ""),
                 "Within Groups (Error)",
                 "Total"),
      df = c("", "", ""),
      MS = c("", "", ""),
      F_val = c("", "", ""),
      p = c("", "", ""),
      check.names = FALSE
    )

    names(data) <- c("Source", "df", "MS", "F", "p")
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


#' Combined ANOVA Results and Descriptives Table
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
#' @param Key Logical. If `TRUE`, fill with values; if `FALSE`, blank.
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
                                           Key = TRUE) {

  if (Key && !is.null(anova_results_list)) {
    desc_stats <- anova_results_list$Descriptives
    anova <- anova_results_list$ANOVA

    if (is.null(iv_labels)) {
      iv_labels <- as.character(desc_stats$iv)
    }

    # Format p-value
    if (anova$p_value < 0.05) {
      decision <- "Reject H0"
    } else {
      decision <- "Retain H0"
    }

    p_formatted <- if (anova$p_value < 0.001) {
      "< .001"
    } else {
      sprintf("%.3f", anova$p_value)
    }

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
      Column5 = c(
        as.character(anova$mse),
        "",
        ""
      ),
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
      Column2 = c("", "", ""),
      Column3 = c("", "", ""),
      Column4 = c("", "", ""),
      Column5 = c("", "", ""),
      Column6 = c("", "", "")
    )
  }

  ft <- flextable::flextable(combined_data) |>
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
#'
#' @param anova_results_list Output from [wg_anova_answers()].
#' @param rh_name Character. Research hypothesis name/label.
#' @param dv_name Character. Display name for the DV.
#' @param condition_labels Character vector or `NULL`. Labels for conditions.
#' @param Key Logical. If `TRUE`, fill with values; if `FALSE`, blank.
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
                                           Key = TRUE) {

  if (Key && !is.null(anova_results_list)) {
    desc_stats <- anova_results_list$Descriptives
    anova <- anova_results_list$ANOVA

    if (is.null(condition_labels)) {
      condition_labels <- as.character(desc_stats$condition)
    }

    # Format p-value
    if (anova$p_value < 0.05) {
      decision <- "Reject H0"
    } else {
      decision <- "Retain H0"
    }

    p_formatted <- if (anova$p_value < 0.001) {
      "< .001"
    } else {
      sprintf("%.3f", anova$p_value)
    }

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
      Column5 = c(
        as.character(anova$mse),
        "",
        ""
      ),
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
      Column2 = c("", "", ""),
      Column3 = c("", "", ""),
      Column4 = c("", "", ""),
      Column5 = c("", "", ""),
      Column6 = c("", "", "")
    )
  }

  ft <- flextable::flextable(combined_data) |>
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
