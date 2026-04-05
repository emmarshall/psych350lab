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

#' APA Univariate Statistics Table
#'
#' Creates an APA-formatted univariate statistics table suitable for
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
#' @param stats_data Output from [univariate_stats_answers()]. Required when
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
#' @seealso [univariate_stats_answers()]
#'
#' @examples
#' \dontrun{
#' data(superman, package = "psych350data")
#' my_data <- superman[!is.na(superman$height_diff),
#'   c("clark_height_in", "height_diff", "clark_grp", "height_gap")]
#' stats <- univariate_stats_answers(my_data)
#'
#' # Without SEM (default)
#' create_apa_univariates_table(
#'   data        = my_data,
#'   continuous  = c("clark_height_in", "height_diff"),
#'   categorical = c("clark_grp", "height_gap"),
#'   stats_data  = stats
#' )
#'
#' # With SEM column
#' create_apa_univariates_table(
#'   data        = my_data,
#'   continuous  = c("clark_height_in", "height_diff"),
#'   categorical = c("clark_grp", "height_gap"),
#'   stats_data  = stats,
#'   show_sem    = TRUE
#' )
#'
#' # Blank worksheet
#' create_apa_univariates_table(
#'   data        = my_data,
#'   continuous  = c("clark_height_in", "height_diff"),
#'   categorical = c("clark_grp", "height_gap"),
#'   KEY = FALSE
#' )
#' }
#'
#' @export
create_apa_univariates_table <- function(data        = NULL,
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


#' APA 7th-Style Chi-Square Crosstabulation Table
#'
#' Creates an APA 7th-formatted crosstabulation table with counts,
#' percentages, and chi-square results in footer. Works with both
#' 2x2 tables and tables with 3+ row levels.
#'
#' @param chi_results_list Output from [chi_square_answers()] (for 2x2 tables)
#'   or [chi_square_kgroup_answers()] (for 3+ row levels), or NULL for blank table.
#' @param var1_name Character. Display name for the row variable.
#' @param var2_name Character. Display name for the column variable.
#' @param var1_labels Character vector or NULL. Labels for row levels.
#'   If NULL and KEY = TRUE, labels are extracted from chi_results_list.
#' @param var2_labels Character vector of length 2. Labels for column levels.
#' @param KEY Logical. If TRUE (default), fill with values; if FALSE, blank.
#' @param include_percentages Logical. If TRUE (default), include row
#'   percentages alongside counts (e.g., "15 (62.5%)").
#' @param table_title Character or NULL. Optional table caption (italicized).
#' @param table_number Integer or NULL. Optional table number (bold).
#'
#' @return A [flextable::flextable()] object.
#'
#' @seealso [chi_square_answers()], [chi_square_kgroup_answers()]
#'
#' @export
create_apa_chi_crosstabs_table <- function(chi_results_list = NULL,
                                           var1_name = "Variable 1",
                                           var2_name = "Variable 2",
                                           var1_labels = NULL,
                                           var2_labels = c("Group 1", "Group 2"),
                                           KEY = TRUE,
                                           include_percentages = TRUE,
                                           table_title = NULL,
                                           table_number = NULL) {

  if (KEY && !is.null(chi_results_list)) {
    if (is.null(var1_labels)) {
      if (!is.null(chi_results_list$Var1_Descriptives$level_label)) {
        var1_labels <- chi_results_list$Var1_Descriptives$level_label
      } else {
        var1_labels <- rownames(chi_results_list$ContingencyTable)
        if (is.null(var1_labels)) {
          var1_labels <- paste("Level", 1:nrow(chi_results_list$ContingencyTable))
        }
      }
    }

    cont_table <- chi_results_list$ContingencyTable
    n_groups <- nrow(cont_table)

    row_totals <- rowSums(cont_table)
    col_totals <- colSums(cont_table)
    grand_total <- sum(cont_table)

    if (include_percentages) {
      format_cell <- function(count, row_total) {
        pct <- round((count / row_total) * 100, 1)
        paste0(count, " (", pct, "%)")
      }

      col1_vals <- sapply(1:n_groups, function(i) format_cell(cont_table[i, 1], row_totals[i]))
      col2_vals <- sapply(1:n_groups, function(i) format_cell(cont_table[i, 2], row_totals[i]))
      total_vals <- sapply(1:n_groups, function(i) paste0(row_totals[i], " (100%)"))

    } else {
      col1_vals <- sapply(1:n_groups, function(i) as.character(cont_table[i, 1]))
      col2_vals <- sapply(1:n_groups, function(i) as.character(cont_table[i, 2]))
      total_vals <- sapply(1:n_groups, function(i) as.character(row_totals[i]))
    }

    data <- data.frame(
      " " = c(var1_labels, "Total"),
      "Group1" = c(col1_vals, as.character(col_totals[1])),
      "Group2" = c(col2_vals, as.character(col_totals[2])),
      "Total" = c(total_vals, as.character(grand_total)),
      check.names = FALSE
    )

    names(data) <- c(" ", var2_labels[1], var2_labels[2], "Total")

    chi_sq <- chi_results_list$ChiSquare$chi_sq
    p_value <- chi_results_list$ChiSquare$p_value
    df <- chi_results_list$ChiSquare$df

    p_text <- format_p_value(p_value, include_p = TRUE)

    footer_text <- paste0("Note. \u03C7\u00B2(", format_df(df), ") = ", format_chi2(chi_sq), ", ", p_text, ".")

  } else {
    if (is.null(var1_labels)) {
      var1_labels <- c("Level 1", "Level 2")
    }

    n_groups <- length(var1_labels)

    data <- data.frame(
      " " = c(var1_labels, "Total"),
      "Group1" = rep("", n_groups + 1),
      "Group2" = rep("", n_groups + 1),
      "Total" = rep("", n_groups + 1),
      check.names = FALSE
    )

    names(data) <- c(" ", var2_labels[1], var2_labels[2], "Total")

    footer_text <- NULL
  }

  # Build base flextable
  apa_table <- data |>
    flextable::flextable() |>
    flextable::set_table_properties(layout = "autofit", align = "left") |>
    flextable::set_header_labels(" " = var1_name)

  # Add spanning header for column variable (var2_name)
  apa_table <- apa_table |>
    flextable::add_header_row(
      values = c("", var2_name, ""),
      colwidths = c(1, 2, 1),
      top = TRUE
    )

  # Count header rows before adding title/number
  # Currently: row 1 = var2_name spanning, row 2 = column headers

  # Determine which rows are which after adding title info
  has_number <- !is.null(table_number)
  has_title <- !is.null(table_title)

  # Add title row (italicized) - this goes ABOVE the line
  if (has_title) {
    apa_table <- apa_table |>
      flextable::add_header_lines(values = table_title, top = TRUE)
  }

  # Add table number row (bold) - this goes at the very top, ABOVE the line
  if (has_number) {
    apa_table <- apa_table |>
      flextable::add_header_lines(values = paste0("Table ", table_number), top = TRUE)
  }

  # Calculate header row positions
  n_header_rows <- flextable::nrow_part(apa_table, part = "header")

  # Number of "above the line" rows (table number + title)
  above_line_rows <- sum(c(has_number, has_title))

  # The first row inside the table (below the top border) is the var2_name spanning row
  # which is at position (above_line_rows + 1)

  # Apply borders
  apa_table <- apa_table |>
    flextable::border_remove()

  # Top border - goes AFTER the title rows (above var2_name spanning header)
  if (above_line_rows > 0) {
    apa_table <- apa_table |>
      flextable::hline(i = above_line_rows, part = "header", border = officer::fp_border(width = 2))
  } else {
    apa_table <- apa_table |>
      flextable::hline_top(part = "header", border = officer::fp_border(width = 2))
  }

  # Border below var2_name spanning row (separates spanning header from column headers)
  apa_table <- apa_table |>
    flextable::hline(i = n_header_rows - 1, part = "header", border = officer::fp_border(width = 1))

  # Border at bottom of header (below column headers)
  apa_table <- apa_table |>
    flextable::hline_bottom(part = "header", border = officer::fp_border(width = 1))

  # Bottom border of table body
  apa_table <- apa_table |>
    flextable::hline_bottom(part = "body", border = officer::fp_border(width = 2))

  # Apply formatting to title rows
  if (has_number && has_title) {
    # Row 1 = Table number (bold), Row 2 = Title (italic)
    apa_table <- apa_table |>
      flextable::bold(i = 1, part = "header") |>
      flextable::italic(i = 2, part = "header")
  } else if (has_number) {
    # Row 1 = Table number (bold)
    apa_table <- apa_table |>
      flextable::bold(i = 1, part = "header")
  } else if (has_title) {
    # Row 1 = Title (italic)
    apa_table <- apa_table |>
      flextable::italic(i = 1, part = "header")
  }

  # Alignment
  apa_table <- apa_table |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::align(j = 1, align = "left", part = "header")

  # Font sizes
  apa_table <- apa_table |>
    flextable::fontsize(size = 11, part = "all")

  # Add footer if present
  if (!is.null(footer_text)) {
    apa_table <- apa_table |>
      flextable::add_footer_lines(footer_text) |>
      flextable::fontsize(part = "footer", size = 10) |>
      flextable::align(part = "footer", align = "left")
  }

  return(apa_table)
}

# -----------------------------------------------------------------------------
# ONEWAY ANOVA tables
# -----------------------------------------------------------------------------

#' Create APA ANOVA source table
#'
#' @param anova_results A results list from `bg_anova_answers()` or similar.
#' @param table_title Table title.
#' @param KEY If TRUE, show filled values; if FALSE, show blanks.
#' @return A flextable object.
#' @export
create_apa_anova_stats_table <- function(anova_results,
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
      n = vapply(desc$n, format_df, character(1)),
      M = vapply(desc$mean, format_mean, character(1)),
      SD = vapply(desc$sd, format_sd, character(1)),
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
#' @param regression_results A results list from `linear_reg_answers()`.
#' @param predictor_labels Optional labels for predictors.
#' @param show_beta Show standardized coefficients.
#' @param show_ci Show confidence intervals.
#' @param KEY If TRUE, show filled values; if FALSE, show blanks.
#' @return A flextable object.
#' @importFrom stats model.matrix
#' @export
create_apa_regression_table <- function(regression_results,
                                        predictor_labels = NULL,
                                        show_beta = TRUE,
                                        show_ci = FALSE,
                                        KEY = TRUE) {
  if (KEY && !is.null(regression_results)) {
    weights <- regression_results$Regression_Weights

    if (is.null(predictor_labels)) {
      predictor_labels <- vapply(weights, \(x) x$label, character(1))
    }

    b_vals <- vapply(weights, \(x) x$b, numeric(1))
    se_vals <- vapply(weights, \(x) x$se, numeric(1))
    t_vals <- vapply(weights, \(x) x$t, numeric(1))
    p_vals <- vapply(weights, \(x) x$p_value, numeric(1))

    tbl_data <- data.frame(
      Predictor = predictor_labels,
      B = vapply(b_vals, format_stat, character(1)),
      SE = vapply(se_vals, format_stat, character(1)),
      stringsAsFactors = FALSE
    )

    if (show_beta) {
      raw_model <- regression_results$Raw_Model
      if (!is.null(raw_model)) {
        X <- model.matrix(raw_model)[, -1, drop = FALSE]
        y <- raw_model$model[[1]]
        X_scaled <- scale(X)
        y_scaled <- scale(y)
        beta_model <- stats::lm(y_scaled ~ X_scaled - 1)
        beta_vals <- unname(stats::coefficients(beta_model))
        tbl_data[["beta"]] <- vapply(
          beta_vals, \(x) format_stat(x, remove_leading_zero = TRUE), character(1)
        )
      }
    }

    tbl_data$t <- vapply(t_vals, format_t, character(1))
    tbl_data$p <- vapply(p_vals, format_p, character(1))

    if (show_ci) {
      ci <- stats::confint(regression_results$Raw_Model)
      # Remove intercept row
      ci <- ci[-1, , drop = FALSE]
      tbl_data$`95% CI` <- mapply(
        format_ci, ci[, 1], ci[, 2], USE.NAMES = FALSE
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
  if (show_beta && "beta" %in% names(tbl_data)) {
    ft <- flextable::set_header_labels(
      ft, beta = paste0("*", intToUtf8(0x03B2), "*")
    )
  }
  ft <- ftExtra::colformat_md(ft, part = "header")
  ft
}

# Deprecated wrapper for backwards compatibility
#' @rdname create_apa_univariates_table
#' @param ... Arguments passed to [create_apa_univariates_table()].
#' @export
create_apa_descriptives_table <- function(...) {
  .Deprecated("create_apa_univariates_table")
  create_apa_univariates_table(...)
}

