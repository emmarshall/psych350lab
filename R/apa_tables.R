# ============================================================================
# apa_tables.R - APA-style table functions for psych350lab
# ============================================================================
# Table styling concepts adapted from apa7 package by W. Joel Schneider.
# Modifications are based on using table to create answer KEYs and for use in teaching context.
# ============================================================================

# Internal helper: apply APA styling to a flextable
apa_style_table <- function(ft) {
  ft |>
    flextable::theme_booktabs() |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::bold(bold = FALSE, part = "header") |>
    flextable::italic(italic = TRUE, part = "header") |>
    flextable::padding(padding.top = 3, padding.bottom = 3, part = "all")
}

# Internal helper: add an APA-style footer note to a flextable
add_apa_note <- function(ft, note_text, note_prefix = "Note. ") {
  ft |>
    flextable::add_footer_lines(paste0(note_prefix, note_text)) |>
    flextable::italic(italic = TRUE, part = "footer") |>
    flextable::align(align = "left", part = "footer")
}


# -----------------------------------------------------------------------------
# APA flextable styling
# -----------------------------------------------------------------------------

#' APA Descriptive Statistics Table
#'
#' Creates an APA-formatted descriptive statistics table suitable for
#' univariate descriptives, ANOVA group descriptives, and regression sample descriptions. Continuous
#' variables show M(SD) and n; categorical variables show level
#' frequencies and percentages. Produces either a filled answer KEY or
#' a blank student worksheet.
#'
#' @param data A data frame. Required when `KEY = TRUE` and `categorical`
#'   variables are included. Can be `NULL` for continuous-only tables.
#' @param continuous Character vector or `NULL`. Continuous variable names.
#'   These display as M(SD) with n.
#' @param categorical Character vector or `NULL`. Categorical variable names.
#'   These display level labels with n and percentage.
#' @param stats_data Output from [compute_summary_stats()]. Required when
#'   `KEY = TRUE`.
#' @param var_labels Named character vector or `NULL`. Display labels,
#'   e.g. `c(height_diff = "Height Difference")`.
#' @param show_sem Logical. If `TRUE`, adds a SEM column for continuous
#'   variables. Default `FALSE`.
#' @param KEY Logical. If `TRUE` (default), fill with computed values.
#'   If `FALSE`, create a blank worksheet template.
#' @param table_title Character or `NULL`. Optional caption.
#' @param footnote Character or `NULL`. Footer note. Defaults to APA-style
#'   M/SD/N explanation, extended with SEM if `show_sem = TRUE`.
#' @param digits Integer. Decimal places for M, SD, and SEM. Default 2.
#'
#' @return A [flextable::flextable()] object.
#'
#' @seealso [compute_summary_stats()]
#'
#' @examples
#' \dontrun{
#' data(superman, package = "psych350data")
#' my_data <- superman[!is.na(superman$height_diff),
#'   c("clark_height_in", "height_diff", "clark_grp", "height_gap")]
#' stats <- compute_summary_stats(my_data)
#'
#' # Without SEM (default)
#' create_descriptive_table(
#'   data        = my_data,
#'   continuous  = c("clark_height_in", "height_diff"),
#'   categorical = c("clark_grp", "height_gap"),
#'   stats_data  = stats
#' )
#'
#' # With SEM column
#' create_descriptive_table(
#'   data        = my_data,
#'   continuous  = c("clark_height_in", "height_diff"),
#'   categorical = c("clark_grp", "height_gap"),
#'   stats_data  = stats,
#'   show_sem    = TRUE
#' )
#'
#' # Blank worksheet
#' create_descriptive_table(
#'   data        = my_data,
#'   continuous  = c("clark_height_in", "height_diff"),
#'   categorical = c("clark_grp", "height_gap"),
#'   KEY = FALSE
#' )
#' }
#'
#' @export
create_descriptive_table <- function(data        = NULL,
                                     continuous  = NULL,
                                     categorical = NULL,
                                     stats_data  = NULL,
                                     var_labels  = NULL,
                                     show_sem    = FALSE,
                                     KEY         = TRUE,
                                     table_title = NULL,
                                     footnote    = NULL,
                                     digits      = 2) {

  if (KEY && is.null(data) && !is.null(categorical)) {
    stop("`data` is required when KEY = TRUE and categorical variables are included.")
  }
  if (KEY && is.null(stats_data)) {
    stop("`stats_data` is required when KEY = TRUE.")
  }

  if (is.null(footnote)) {
    footnote <- if (show_sem) {
      "Note. M = Mean; SD = Standard Deviation; SEM = Standard Error of the Mean; N = Sample Size."
    } else {
      "Note. M = Mean; SD = Standard Deviation; N = Sample Size."
    }
  }

  get_label <- function(v) {
    if (!is.null(var_labels) && v %in% names(var_labels)) unname(var_labels[v]) else v
  }

  fmt <- function(x) format(round(x, digits), nsmall = digits)

  table_rows <- list()

  if (KEY) {

    # Continuous: M(SD) | SEM (optional) | n
    if (!is.null(continuous)) {
      cont_stats <- stats_data |>
        dplyr::filter(.data$variable %in% continuous) |>
        dplyr::slice(match(continuous, .data$variable))

      for (i in seq_len(nrow(cont_stats))) {
        row <- data.frame(
          Variable = get_label(cont_stats$variable[i]),
          `M(SD)`  = paste0(fmt(cont_stats$mean[i]), " (", fmt(cont_stats$sd[i]), ")"),
          `N(%)`   = as.character(cont_stats$n[i]),
          check.names = FALSE, stringsAsFactors = FALSE
        )
        if (show_sem) {
          row$SEM <- fmt(cont_stats$sem[i])
        }
        table_rows[[length(table_rows) + 1]] <- row
      }
    }

    # Categorical: header row + one row per level with n(%)
    if (!is.null(categorical)) {
      for (var in categorical) {
        row <- data.frame(
          Variable = get_label(var), `M(SD)` = "", `N(%)` = "",
          check.names = FALSE, stringsAsFactors = FALSE
        )
        if (show_sem) row$SEM <- ""
        table_rows[[length(table_rows) + 1]] <- row

        freq_data <- data |>
          dplyr::count(.data[[var]], name = "freq") |>
          dplyr::filter(!is.na(.data[[var]])) |>
          dplyr::mutate(pct = round((freq / sum(freq)) * 100, 1))

        for (j in seq_len(nrow(freq_data))) {
          row <- data.frame(
            Variable = paste0("  ", freq_data[[var]][j]),
            `M(SD)`  = "",
            `N(%)`   = paste0(freq_data$freq[j], " (", freq_data$pct[j], "%)"),
            check.names = FALSE, stringsAsFactors = FALSE
          )
          if (show_sem) row$SEM <- ""
          table_rows[[length(table_rows) + 1]] <- row
        }
      }
    }

  } else {

    # Blank continuous rows
    if (!is.null(continuous)) {
      for (var in continuous) {
        row <- data.frame(
          Variable = get_label(var), `M(SD)` = "___", `N(%)` = "___",
          check.names = FALSE, stringsAsFactors = FALSE
        )
        if (show_sem) row$SEM <- "___"
        table_rows[[length(table_rows) + 1]] <- row
      }
    }

    # Blank categorical rows
    if (!is.null(categorical)) {
      for (var in categorical) {
        row <- data.frame(
          Variable = get_label(var), `M(SD)` = "", `N(%)` = "",
          check.names = FALSE, stringsAsFactors = FALSE
        )
        if (show_sem) row$SEM <- ""
        table_rows[[length(table_rows) + 1]] <- row

        if (!is.null(data) && var %in% names(data)) {
          levels_vec <- sort(unique(stats::na.omit(data[[var]])))
        } else {
          levels_vec <- paste("Level", 1:3)
        }

        for (lv in levels_vec) {
          row <- data.frame(
            Variable = paste0("  ", lv), `M(SD)` = "", `N(%)` = "___",
            check.names = FALSE, stringsAsFactors = FALSE
          )
          if (show_sem) row$SEM <- ""
          table_rows[[length(table_rows) + 1]] <- row
        }
      }
    }
  }

  table_data <- dplyr::bind_rows(table_rows)

  # Reorder columns so SEM sits between M(SD) and N(%)
  if (show_sem && "SEM" %in% names(table_data)) {
    table_data <- table_data |>
      dplyr::select("Variable", "M(SD)", "SEM", "N(%)")
  }

  n_numeric_cols <- if (show_sem) 2:4 else 2:3

  ft <- flextable::flextable(table_data) |>
    apa_style_table() |>
    flextable::align(j = n_numeric_cols, align = "center", part = "all") |>
    flextable::align(j = 1,             align = "left",   part = "all") |>
    flextable::width(j = 1, width = 2.5) |>
    flextable::add_footer_lines(footnote) |>
    flextable::italic(italic = TRUE, part = "footer") |>
    flextable::align(align = "left", part = "footer")

  if (!is.null(table_title)) {
    ft <- flextable::set_caption(ft, caption = table_title)
  }

  ft
}


# -----------------------------------------------------------------------------
# Chi-square crosstabs table
# -----------------------------------------------------------------------------

#' Create APA chi-square crosstabulation table
#'
#' @param chi_results_list A results list from `chi_square_answers()`, or NULL
#'   for blank template.
#' @param var1_name Display name for row variable.
#' @param var2_name Display name for column variable.
#' @param var1_labels Labels for row variable levels.
#' @param var2_labels Labels for column variable levels.
#' @param table_title Table title/caption.
#' @param table_number Table number for caption.
#' @param KEY If TRUE, show filled values; if FALSE, show blanks.
#' @return A flextable object.
#' @export
create_apa_chi_crosstabs_table <- function(chi_results_list = NULL,
                                           var1_name = "Row Variable",
                                           var2_name = "Column Variable",
                                           var1_labels = c("Level 1", "Level 2"),
                                           var2_labels = c("Group 1", "Group 2"),
                                           table_title = NULL,
                                           table_number = NULL,
                                           KEY = TRUE) {

  n_rows <- length(var1_labels)
  n_cols <- length(var2_labels)

  # Build table data
  if (KEY && !is.null(chi_results_list)) {
    # Extract counts from results
    ct <- chi_results_list$Crosstab

    # Build data frame with counts
    tbl_data <- data.frame(
      Variable = var1_labels,
      stringsAsFactors = FALSE
    )

    for (j in seq_len(n_cols)) {
      col_values <- vapply(seq_len(n_rows), function(i) {
        as.character(ct[i, j])
      }, character(1))
      tbl_data[[var2_labels[j]]] <- col_values
    }

    # Add row totals
    tbl_data$Total <- rowSums(ct)

    # Add column totals row
    col_totals <- c("Total", as.character(colSums(ct)), sum(ct))
    tbl_data <- rbind(tbl_data, col_totals)

  } else {
    # Blank template
    tbl_data <- data.frame(
      Variable = c(var1_labels, "Total"),
      stringsAsFactors = FALSE
    )

    for (j in seq_len(n_cols)) {
      tbl_data[[var2_labels[j]]] <- rep("______", n_rows + 1)
    }
    tbl_data$Total <- rep("______", n_rows + 1)
  }

  # Create header structure for spanning
  col_names <- c(var1_name, var2_labels, "Total")
  names(tbl_data) <- col_names

  # Create flextable
  ft <- flextable::flextable(tbl_data) |>
    apa_style_table() |>
    flextable::align(j = 2:(n_cols + 2), align = "center", part = "all")

  # Add column spanner for var2
  if (n_cols > 1) {
    ft <- flextable::add_header_row(
      ft,
      values = c("", var2_name, ""),
      colwidths = c(1, n_cols, 1),
      top = TRUE
    ) |>
      flextable::align(part = "header", align = "center")
  }

  # Add title
  if (!is.null(table_title)) {
    if (!is.null(table_number)) {
      full_title <- paste0("Table ", table_number, "\n\n", table_title)
    } else {
      full_title <- table_title
    }
    ft <- flextable::set_caption(ft, caption = full_title)
  }

  ft
}

# -----------------------------------------------------------------------------
# ANOVA tables
# -----------------------------------------------------------------------------

#' Create APA ANOVA source table
#'
#' @param anova_results A results list from `bg_anova_answers()` or similar.
#' @param table_title Table title.
#' @param KEY If TRUE, show filled values; if FALSE, show blanks.
#' @return A flextable object.
#' @export
create_apa_anova_source_table <- function(anova_results,
                                          table_title = NULL,
                                          KEY = TRUE) {

  if (KEY && !is.null(anova_results)) {
    anova_tbl <- anova_results$ANOVA

    tbl_data <- data.frame(
      Source = c("Between Groups", "Within Groups", "Total"),
      SS = c(
        format_stat(anova_tbl$SS_between),
        format_stat(anova_tbl$SS_within),
        format_stat(anova_tbl$SS_total)
      ),
      df = c(
        format_df(anova_tbl$df_between),
        format_df(anova_tbl$df_within),
        format_df(anova_tbl$df_total)
      ),
      MS = c(
        format_stat(anova_tbl$MS_between),
        format_stat(anova_tbl$MS_within),
        ""
      ),
      `F` = c(
        format_F(anova_tbl$F_value),
        "",
        ""
      ),
      p = c(
        format_p(anova_tbl$p_value),
        "",
        ""
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    tbl_data <- data.frame(
      Source = c("Between Groups", "Within Groups", "Total"),
      SS = rep("______", 3),
      df = rep("______", 3),
      MS = c("______", "______", ""),
      `F` = c("______", "", ""),
      p = c("______", "", ""),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  # Format header with italics
  ft <- flextable::flextable(tbl_data) |>
    apa_style_table() |>
    flextable::align(j = 2:6, align = "center", part = "all") |>
    flextable::set_header_labels(
      SS = "*SS*",
      df = "*df*",
      MS = "*MS*",
      `F` = "*F*",
      p = "*p*"
    ) |>
    ftExtra::colformat_md(part = "header")

  if (!is.null(table_title)) {
    ft <- flextable::set_caption(ft, caption = table_title)
  }

  ft
}

#' Create APA descriptives table for ANOVA
#'
#' @param anova_results A results list from `bg_anova_answers()`.
#' @param group_labels Labels for the groups.
#' @param dv_label Label for the dependent variable.
#' @param KEY If TRUE, show filled values; if FALSE, show blanks.
#' @return A flextable object.
#' @export
create_apa_anova_descriptives_table <- function(anova_results,
                                                group_labels = NULL,
                                                dv_label = "Dependent Variable",
                                                KEY = TRUE) {

  if (KEY && !is.null(anova_results)) {
    desc <- anova_results$Descriptives

    if (is.null(group_labels)) {
      group_labels <- desc$group
    }

    tbl_data <- data.frame(
      Group = group_labels,
      n = format_df(desc$n),
      M = format_mean(desc$mean),
      SD = format_sd(desc$sd),
      stringsAsFactors = FALSE
    )
  } else {
    n_groups <- if (!is.null(group_labels)) length(group_labels) else 3
    if (is.null(group_labels)) group_labels <- paste("Group", 1:n_groups)

    tbl_data <- data.frame(
      Group = group_labels,
      n = rep("______", n_groups),
      M = rep("______", n_groups),
      SD = rep("______", n_groups),
      stringsAsFactors = FALSE
    )
  }

  ft <- flextable::flextable(tbl_data) |>
    apa_style_table() |>
    flextable::align(j = 2:4, align = "center", part = "all") |>
    flextable::set_header_labels(
      n = "*n*",
      M = "*M*",
      SD = "*SD*"
    ) |>
    ftExtra::colformat_md(part = "header")

  ft
}

# -----------------------------------------------------------------------------
# Regression tables
# -----------------------------------------------------------------------------

#' Create APA regression coefficients table
#'
#' @param regression_results A results list from `regression_answers()`.
#' @param predictor_labels Optional labels for predictors.
#' @param show_beta Show standardized coefficients.
#' @param show_ci Show confidence intervals.
#' @param KEY If TRUE, show filled values; if FALSE, show blanks.
#' @return A flextable object.
#' @export
create_apa_regression_table <- function(regression_results,
                                        predictor_labels = NULL,
                                        show_beta = TRUE,
                                        show_ci = FALSE,
                                        KEY = TRUE) {

  if (KEY && !is.null(regression_results)) {
    weights <- regression_results$Regression_Weights

    if (is.null(predictor_labels)) {
      predictor_labels <- weights$predictor
    }

    tbl_data <- data.frame(
      Predictor = predictor_labels,
      B = format_stat(weights$b),
      SE = format_stat(weights$se),
      stringsAsFactors = FALSE
    )

    if (show_beta) {
      tbl_data[["beta"]] <- format_stat(weights$beta, remove_leading_zero = TRUE)
    }

    tbl_data$t <- format_t(weights$t)
    tbl_data$p <- format_p(weights$p)

    if (show_ci) {
      tbl_data$`95% CI` <- mapply(
        format_ci,
        weights$ci_low,
        weights$ci_high,
        USE.NAMES = FALSE
      )
    }

  } else {
    n_pred <- if (!is.null(predictor_labels)) length(predictor_labels) else 3
    if (is.null(predictor_labels)) predictor_labels <- paste("Predictor", 1:n_pred)

    tbl_data <- data.frame(
      Predictor = predictor_labels,
      B = rep("______", n_pred),
      SE = rep("______", n_pred),
      stringsAsFactors = FALSE
    )

    if (show_beta) {
      tbl_data[["beta"]] <- rep("______", n_pred)
    }

    tbl_data$t <- rep("______", n_pred)
    tbl_data$p <- rep("______", n_pred)

    if (show_ci) {
      tbl_data$`95% CI` <- rep("______", n_pred)
    }
  }

  ft <- flextable::flextable(tbl_data) |>
    apa_style_table() |>
    flextable::align(j = 2:ncol(tbl_data), align = "center", part = "all") |>
    flextable::set_header_labels(
      B = "*B*",
      SE = "*SE*",
      t = "*t*",
      p = "*p*"
    )

  if (show_beta) {
    ft <- flextable::set_header_labels(ft, beta = paste0("*", intToUtf8(0x03B2), "*"))
  }

  ft <- ftExtra::colformat_md(ft, part = "header")

  ft
}

# -----------------------------------------------------------------------------
# Utility: Blank vs. filled table generation
# -----------------------------------------------------------------------------

#' Create worksheet value (blank or filled)
#'
#' Helper function to return either a formatted value or a blank for worksheets.
#'
#' @param value The value to format.
#' @param format_fn The formatting function to apply.
#' @param KEY If TRUE, return formatted value; if FALSE, return blank.
#' @param blank_text Text to use for blanks (default "______").
#' @param ... Additional arguments passed to format_fn.
#' @return Character string.
#' @export
worksheet_value <- function(value, format_fn, KEY = TRUE, blank_text = "______", ...) {
  if (KEY) {
    format_fn(value, ...)
  } else {
    blank_text
  }
}
