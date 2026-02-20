# =============================================================================
# spss_typst.R
# SPSS-Style Tables for Typst Output (Quarto PDF)
# =============================================================================
# Creates publication-ready tables that mimic SPSS output appearance.
# Outputs raw Typst markup for use in Quarto documents with format: typst.
# Requires Typst template with spss_table() and spss_table_complex() functions.
# =============================================================================

# -----------------------------------------------------------------------------
# INTERNAL HELPERS
# -----------------------------------------------------------------------------

#' Output raw Typst code block
#' @noRd
.typst_raw <- function(...) {
  cat("\n```{=typst}\n")
  cat(...)
  cat("\n```\n\n")
}

#' Format numeric for Typst
#' @noRd
.typst_fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
}

#' Format p-value for Typst (no leading zero)
#' @noRd
.typst_fmt_p <- function(p, digits = 3) {
  if (is.na(p)) return("")
  if (p < 0.001) return("<.001")
  sub("^0\\.", ".", formatC(p, digits = digits, format = "f"))
}

#' Escape special Typst characters
#' @noRd
.typst_esc <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub('"', '\\\\"', x)
  x <- gsub("#", "\\\\#", x)
  x <- gsub("\\$", "\\\\$", x)
  x <- gsub("@", "\\\\@", x)
  x <- gsub("<", "\\\\<", x)
  x <- gsub(">", "\\\\>", x)
  x
}

#' Create Typst table cell
#' @noRd
.typst_cell <- function(x) {
  if (is.na(x) || x == "") return("[]")
  paste0("[", .typst_esc(as.character(x)), "]")
}

#' Create Typst table row
#' @noRd
.typst_row <- function(vals) {
  paste0("    ", paste(sapply(vals, .typst_cell), collapse = ", "), ",")
}

#' Get variable label from haven-imported data
#' @noRd
.typst_get_var_label <- function(data_raw, var) {
  if (is.null(data_raw)) return(var)
  lbl <- attr(data_raw[[var]], "label")
  if (!is.null(lbl)) lbl else var
}

#' Get value labels from haven-imported data
#' @noRd
.typst_get_val_labels <- function(data_raw, var) {
  if (is.null(data_raw)) return(NULL)
  lbls <- attr(data_raw[[var]], "labels")
  if (!is.null(lbls)) {
    stats::setNames(names(lbls), as.character(lbls))
  } else {
    NULL
  }
}


# =============================================================================
# DESCRIPTIVES TABLE (Typst)
# =============================================================================

#' SPSS-Style Descriptives Table (Typst)
#'
#' Outputs a Typst table displaying descriptive statistics in SPSS format.
#' For use in Quarto documents with `format: typst`.
#'
#' @param data A data frame.
#' @param variables Character vector of variable names to summarize.
#' @param digits Integer. Number of decimal places. Default 2.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @examples
#' \dontrun{
#' # In a Quarto chunk with results: asis
#' data(superman)
#' typst_descriptives(superman, c("clark_height_in", "rt_critics_score"))
#' }
#'
#' @export
typst_descriptives <- function(data, variables, digits = 2) {

  row_strings <- c()
  valid_n <- nrow(data[stats::complete.cases(data[, variables, drop = FALSE]), , drop = FALSE])

  for (var in variables) {
    x <- data[[var]]
    x <- x[!is.na(x)]
    n <- length(x)
    m <- mean(x)
    s <- stats::sd(x)
    mn <- min(x)
    mx <- max(x)
    se <- s / sqrt(n)

    row_strings <- c(row_strings, .typst_row(c(
      var, n, .typst_fmt(m, digits), .typst_fmt(s, digits),
      .typst_fmt(se, digits), .typst_fmt(mn, digits), .typst_fmt(mx, digits)
    )))
  }

  row_strings <- c(row_strings,
                   paste0("    [Valid N (listwise)], [", valid_n, "], [], [], [], [], [],"))

  .typst_raw(paste0(
    "#spss_table(\n",
    "  title: \"Descriptive Statistics\",\n",
    "  columns: (\"\", \"N\", \"Mean\", \"Std. Deviation\", \"Std. Error\", \"Minimum\", \"Maximum\"),\n",
    "  rows: (\n",
    paste(row_strings, collapse = "\n"), "\n",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


# =============================================================================
# DESCRIPTIVES BY GROUP (Typst)
# =============================================================================

#' SPSS-Style Grouped Descriptives Table (Typst)
#'
#' Outputs a Typst table displaying descriptive statistics by group.
#'
#' @param data A data frame.
#' @param dv Character. Name of the dependent variable.
#' @param iv Character. Name of the independent (grouping) variable.
#' @param data_raw Optional. Original haven-imported data for value labels.
#' @param digits Integer. Decimal places. Default 2.
#' @param conf.level Numeric. Confidence level for CI. Default 0.95.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_descriptives_by_group <- function(data, dv, iv, data_raw = NULL,
                                        digits = 2, conf.level = 0.95) {

  dv_vals <- data[[dv]]
  iv_vals <- data[[iv]]
  groups <- split(dv_vals, iv_vals)
  val_labels <- .typst_get_val_labels(data_raw, iv)

  row_strings <- c()

  for (g in names(groups)) {
    x <- groups[[g]]
    x <- x[!is.na(x)]
    n <- length(x)
    if (n == 0) next

    m <- mean(x)
    s <- if (n > 1) stats::sd(x) else NA
    se <- if (n > 1) s / sqrt(n) else NA
    ci <- if (n > 1) {
      tryCatch(stats::t.test(x, conf.level = conf.level)$conf.int,
               error = function(e) c(NA, NA))
    } else {
      c(NA, NA)
    }

    label <- if (!is.null(val_labels) && g %in% names(val_labels)) {
      val_labels[[g]]
    } else {
      g
    }

    row_strings <- c(row_strings, .typst_row(c(
      label, n, .typst_fmt(m, digits), .typst_fmt(s, digits), .typst_fmt(se, digits),
      .typst_fmt(ci[1], digits), .typst_fmt(ci[2], digits),
      .typst_fmt(min(x), digits), .typst_fmt(max(x), digits)
    )))
  }

  # Total row
  x_all <- dv_vals[!is.na(dv_vals)]
  n_all <- length(x_all)
  ci_all <- tryCatch(
    stats::t.test(x_all, conf.level = conf.level)$conf.int,
    error = function(e) c(NA, NA)
  )

  row_strings <- c(row_strings, .typst_row(c(
    "Total", n_all, .typst_fmt(mean(x_all), digits), .typst_fmt(stats::sd(x_all), digits),
    .typst_fmt(stats::sd(x_all) / sqrt(n_all), digits),
    .typst_fmt(ci_all[1], digits), .typst_fmt(ci_all[2], digits),
    .typst_fmt(min(x_all), digits), .typst_fmt(max(x_all), digits)
  )))

  ci_pct <- paste0(round(conf.level * 100), "% Confidence Interval for Mean")

  .typst_raw(paste0(
    "#spss_table_complex(\n",
    "  title: \"Descriptives\",\n",
    "  subtitle: \"", .typst_esc(dv), "\",\n",
    "  ncols: 9,\n",
    "  header_rows: (\n",
    "    ([], [], [], [], [], [", ci_pct, "], [], [], []),\n",
    "    ([], [N], [Mean], [Std. Deviation], [Std. Error],\n",
    "     [Lower Bound], [Upper Bound], [Minimum], [Maximum]),\n",
    "  ),\n",
    "  rows: (\n",
    paste(row_strings, collapse = "\n"), "\n",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


# =============================================================================
# LEVENE'S TEST (Typst)
# =============================================================================

#' SPSS-Style Levene's Test Table (Typst)
#'
#' Outputs a Typst table for Levene's test of homogeneity of variances.
#' Requires the \pkg{car} package (listed in Suggests).
#'
#' @param data A data frame.
#' @param dv Character. Dependent variable name.
#' @param iv Character. Independent variable name.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_levene <- function(data, dv, iv, digits = 3) {

  if (!requireNamespace("car", quietly = TRUE)) {
    message("Install 'car' package for Levene's test: install.packages('car')")
    return(invisible(NULL))
  }

  formula <- stats::as.formula(paste(dv, "~", "factor(", iv, ")"))
  lev <- car::leveneTest(formula, data = data)

  f_val <- lev$`F value`[1]
  df1 <- lev$Df[1]
  df2 <- lev$Df[2]
  p_val <- lev$`Pr(>F)`[1]

  .typst_raw(paste0(
    "#spss_table(\n",
    "  title: \"Test of Homogeneity of Variances\",\n",
    "  subtitle: \"", .typst_esc(dv), "\",\n",
    "  columns: (\"Levene Statistic\", \"df1\", \"df2\", \"Sig.\"),\n",
    "  label_cols: 0,\n",
    "  rows: (\n",
    .typst_row(c(.typst_fmt(f_val, digits), df1, df2, .typst_fmt_p(p_val, digits))), "\n",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


# =============================================================================
# ONE-WAY ANOVA (Typst)
# =============================================================================

#' SPSS-Style One-Way ANOVA Table (Typst)
#'
#' Outputs a Typst ANOVA table.
#'
#' @param data A data frame.
#' @param dv Character. Dependent variable name.
#' @param iv Character. Independent variable name.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_anova_oneway <- function(data, dv, iv, digits = 3) {

  formula <- stats::as.formula(paste(dv, "~", "factor(", iv, ")"))
  fit <- stats::aov(formula, data = data)
  s <- summary(fit)[[1]]

  ss_b <- s$`Sum Sq`[1]
  ss_w <- s$`Sum Sq`[2]
  ss_t <- sum(s$`Sum Sq`)
  df_b <- s$Df[1]
  df_w <- s$Df[2]
  ms_b <- s$`Mean Sq`[1]
  ms_w <- s$`Mean Sq`[2]
  f_val <- s$`F value`[1]
  p_val <- s$`Pr(>F)`[1]

  .typst_raw(paste0(
    "#spss_table(\n",
    "  title: \"ANOVA\",\n",
    "  subtitle: \"", .typst_esc(dv), "\",\n",
    "  columns: (\"\", \"Sum of Squares\", \"df\", \"Mean Square\", \"F\", \"Sig.\"),\n",
    "  rows: (\n",
    .typst_row(c("Between Groups", .typst_fmt(ss_b, digits), df_b,
                 .typst_fmt(ms_b, digits), .typst_fmt(f_val, digits),
                 .typst_fmt_p(p_val, digits))), "\n",
    .typst_row(c("Within Groups", .typst_fmt(ss_w, digits), df_w,
                 .typst_fmt(ms_w, digits), "", "")), "\n",
    .typst_row(c("Total", .typst_fmt(ss_t, digits), df_b + df_w,
                 "", "", "")), "\n",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


# =============================================================================
# POST-HOC TESTS (Typst)
# =============================================================================

#' SPSS-Style Post-Hoc Comparisons Table (Typst)
#'
#' Outputs Typst tables for Tukey HSD and/or LSD post-hoc comparisons.
#'
#' @param data A data frame.
#' @param dv Character. Dependent variable name.
#' @param iv Character. Independent variable name.
#' @param methods Character vector. One or both of "tukey" and "lsd".
#' @param data_raw Optional. Original haven-imported data for value labels.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_posthoc <- function(data, dv, iv, methods = c("tukey", "lsd"),
                          data_raw = NULL, digits = 3) {

  data$iv_factor <- factor(data[[iv]])
  formula <- stats::as.formula(paste(dv, "~ iv_factor"))
  fit <- stats::aov(formula, data = data)
  val_labels <- .typst_get_val_labels(data_raw, iv)

  .label <- function(g) {
    if (!is.null(val_labels) && g %in% names(val_labels)) {
      val_labels[[g]]
    } else {
      g
    }
  }

  # Tukey HSD
  if ("tukey" %in% methods) {
    tk <- stats::TukeyHSD(fit)$iv_factor
    row_strings <- c()

    ngroups <- length(levels(data$iv_factor))
    q_crit <- stats::qtukey(0.95, ngroups, fit$df.residual) / sqrt(2)

    for (i in seq_len(nrow(tk))) {
      pair <- strsplit(rownames(tk)[i], "-")[[1]]
      md <- tk[i, "diff"]
      p <- tk[i, "p adj"]
      se_approx <- (tk[i, "upr"] - tk[i, "lwr"]) / (2 * q_crit)

      row_strings <- c(row_strings, .typst_row(c(
        .label(pair[2]), .label(pair[1]),
        .typst_fmt(md, digits), .typst_fmt(se_approx, digits),
        .typst_fmt_p(p, digits),
        .typst_fmt(tk[i, "lwr"], digits), .typst_fmt(tk[i, "upr"], digits)
      )))

      row_strings <- c(row_strings, .typst_row(c(
        .label(pair[1]), .label(pair[2]),
        .typst_fmt(-md, digits), .typst_fmt(se_approx, digits),
        .typst_fmt_p(p, digits),
        .typst_fmt(-tk[i, "upr"], digits), .typst_fmt(-tk[i, "lwr"], digits)
      )))
    }

    .typst_raw(paste0(
      "#spss_table_complex(\n",
      "  title: \"Multiple Comparisons\",\n",
      "  subtitle: \"Dependent Variable: ", .typst_esc(dv), " \\u{2014} Tukey HSD\",\n",
      "  ncols: 7,\n",
      "  label_cols: 2,\n",
      "  header_rows: (\n",
      "    ([], [], [], [], [], [95% Confidence Interval], []),\n",
      "    ([(I) ", .typst_esc(iv), "], [(J) ", .typst_esc(iv),
      "], [Mean Difference (I-J)], [Std. Error], [Sig.],\n",
      "     [Lower Bound], [Upper Bound]),\n",
      "  ),\n",
      "  rows: (\n",
      paste(row_strings, collapse = "\n"), "\n",
      "  ),\n",
      ")\n"
    ))
  }

  # LSD
  if ("lsd" %in% methods) {
    pw <- stats::pairwise.t.test(data[[dv]], data$iv_factor, p.adjust.method = "none")
    groups <- levels(data$iv_factor)
    mse <- sum(stats::residuals(fit)^2) / fit$df.residual
    row_strings <- c()

    for (i in seq_along(groups)) {
      for (j in seq_along(groups)) {
        if (i != j) {
          g1 <- groups[i]
          g2 <- groups[j]
          x1 <- data[[dv]][data$iv_factor == g1]
          x2 <- data[[dv]][data$iv_factor == g2]
          md <- mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE)
          se <- sqrt(mse * (1 / sum(!is.na(x1)) + 1 / sum(!is.na(x2))))
          p <- if (i > j) pw$p.value[i - 1, j] else pw$p.value[j - 1, i]
          t_crit <- stats::qt(0.975, fit$df.residual)

          row_strings <- c(row_strings, .typst_row(c(
            .label(g1), .label(g2),
            .typst_fmt(md, digits), .typst_fmt(se, digits),
            .typst_fmt_p(p, digits),
            .typst_fmt(md - t_crit * se, digits),
            .typst_fmt(md + t_crit * se, digits)
          )))
        }
      }
    }

    .typst_raw(paste0(
      "#spss_table_complex(\n",
      "  title: \"Multiple Comparisons\",\n",
      "  subtitle: \"Dependent Variable: ", .typst_esc(dv), " \\u{2014} LSD\",\n",
      "  ncols: 7,\n",
      "  label_cols: 2,\n",
      "  header_rows: (\n",
      "    ([], [], [], [], [], [95% Confidence Interval], []),\n",
      "    ([(I) ", .typst_esc(iv), "], [(J) ", .typst_esc(iv),
      "], [Mean Difference (I-J)], [Std. Error], [Sig.],\n",
      "     [Lower Bound], [Upper Bound]),\n",
      "  ),\n",
      "  rows: (\n",
      paste(row_strings, collapse = "\n"), "\n",
      "  ),\n",
      ")\n"
    ))
  }

  invisible(NULL)
}


# =============================================================================
# CHI-SQUARE TEST (Typst)
# =============================================================================

#' SPSS-Style Chi-Square Test Table (Typst)
#'
#' Outputs a Typst chi-square test table.
#'
#' @param data A data frame.
#' @param var1 Character. First variable name.
#' @param var2 Character. Second variable name.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_chisq <- function(data, var1, var2, digits = 3) {

  tab <- table(data[[var1]], data[[var2]])
  n <- sum(tab)

  chi <- stats::chisq.test(tab, correct = FALSE)
  is_2x2 <- nrow(tab) == 2 && ncol(tab) == 2

  low_expected <- sum(chi$expected < 5)
  pct_low <- 100 * low_expected / length(chi$expected)
  min_expected <- min(chi$expected)

  row_strings <- c()

  # Pearson Chi-Square
  row_strings <- c(row_strings, .typst_row(c(
    "Pearson Chi-Square",
    .typst_fmt(chi$statistic, digits),
    chi$parameter,
    .typst_fmt_p(chi$p.value, digits)
  )))

  # Continuity Correction (2x2 only)
  if (is_2x2) {
    chi_corrected <- stats::chisq.test(tab, correct = TRUE)
    row_strings <- c(row_strings, .typst_row(c(
      "Continuity Correction",
      .typst_fmt(chi_corrected$statistic, digits),
      chi_corrected$parameter,
      .typst_fmt_p(chi_corrected$p.value, digits)
    )))
  }

  # N of Valid Cases
  row_strings <- c(row_strings, .typst_row(c("N of Valid Cases", n, "", "")))

  .typst_raw(paste0(
    "#spss_table(\n",
    "  title: \"Chi-Square Tests\",\n",
    "  columns: (\"\", \"Value\", \"df\",",
    " \"Asymptotic Significance (2-sided)\"),\n",
    "  rows: (\n",
    paste(row_strings, collapse = "\n"), "\n",
    "  ),\n",
    "  footnotes: (\n",
    "    [", low_expected, " cells (",
    .typst_fmt(pct_low, 1),
    "%) have expected count less than 5. The minimum expected count is ",
    .typst_fmt(min_expected, 2), ".],\n",
    if (is_2x2) "    [Computed only for a 2x2 table.],\n" else "",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


#' SPSS-Style Crosstabulation Table (Typst)
#'
#' Outputs a Typst crosstabulation table with observed and expected counts.
#'
#' @param data A data frame.
#' @param var1 Character. Row variable name.
#' @param var2 Character. Column variable name.
#' @param data_raw Optional. Original haven-imported data for labels.
#' @param digits Integer. Decimal places for expected counts. Default 1.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_crosstab <- function(data, var1, var2, data_raw = NULL, digits = 1) {

  tab <- table(data[[var1]], data[[var2]])
  chi <- stats::chisq.test(tab, correct = FALSE)
  nr <- nrow(tab)

  rows1_labels <- .typst_get_val_labels(data_raw, var1)
  rows2_labels <- .typst_get_val_labels(data_raw, var2)

  .rl <- function(val, labels) {
    if (!is.null(labels) && val %in% names(labels)) labels[[val]] else val
  }

  row_strings <- c()
  for (i in seq_len(nr)) {
    row_label <- .rl(rownames(tab)[i], rows1_labels)
    counts <- c(row_label, "Count", as.character(tab[i, ]), sum(tab[i, ]))
    row_strings <- c(row_strings, .typst_row(counts))
    expected <- c("", "Expected Count",
                  sapply(chi$expected[i, ], function(x) .typst_fmt(x, digits)),
                  .typst_fmt(sum(chi$expected[i, ]), digits))
    row_strings <- c(row_strings, .typst_row(expected))
  }

  # Total row
  col_totals <- colSums(tab)
  row_strings <- c(row_strings, .typst_row(c(
    "Total", "Count", as.character(col_totals), sum(col_totals))))
  exp_totals <- colSums(chi$expected)
  row_strings <- c(row_strings, .typst_row(c(
    "", "Expected Count",
    sapply(exp_totals, function(x) .typst_fmt(x, digits)),
    .typst_fmt(sum(exp_totals), digits))))

  col_hdrs <- c(.typst_esc(var1), "",
                sapply(colnames(tab), function(x) .rl(x, rows2_labels)), "Total")

  .typst_raw(paste0(
    "#spss_table(\n",
    "  title: \"", .typst_esc(var1), " * ", .typst_esc(var2), " Crosstabulation\",\n",
    "  label_cols: 2,\n",
    "  columns: (", paste(sapply(col_hdrs, .typst_cell), collapse = ", "), "),\n",
    "  rows: (\n",
    paste(row_strings, collapse = "\n"), "\n",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


# =============================================================================
# CORRELATIONS (Typst)
# =============================================================================

#' SPSS-Style Correlation Matrix Table (Typst)
#'
#' Outputs a Typst correlation matrix with significance stars.
#'
#' @param data A data frame.
#' @param variables Character vector of variable names.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_correlations <- function(data, variables, digits = 3) {

  complete <- data[stats::complete.cases(data[, variables, drop = FALSE]), variables, drop = FALSE]
  n <- nrow(complete)
  nvars <- length(variables)

  cor_mat <- stats::cor(complete)
  p_mat <- matrix(NA, nvars, nvars)
  for (i in seq_len(nvars)) {
    for (j in seq_len(nvars)) {
      if (i != j) {
        p_mat[i, j] <- stats::cor.test(complete[[i]], complete[[j]])$p.value
      }
    }
  }

  row_strings <- c()
  for (i in seq_len(nvars)) {
    # Pearson Correlation row
    vals <- c(variables[i], "Pearson Correlation")
    for (j in seq_len(nvars)) {
      r <- cor_mat[i, j]
      star <- ""
      if (i != j && !is.na(p_mat[i, j])) {
        if (p_mat[i, j] < 0.01) {
          star <- "\\*\\*"
        } else if (p_mat[i, j] < 0.05) {
          star <- "\\*"
        }
      }
      vals <- c(vals, paste0(.typst_fmt(r, digits), star))
    }
    row_strings <- c(row_strings, .typst_row(vals))

    # Sig. row
    vals_p <- c("", "Sig. (2-tailed)")
    for (j in seq_len(nvars)) {
      if (i == j) {
        vals_p <- c(vals_p, "")
      } else {
        vals_p <- c(vals_p, .typst_fmt_p(p_mat[i, j], digits))
      }
    }
    row_strings <- c(row_strings, .typst_row(vals_p))

    # N row
    vals_n <- c("", "N")
    for (j in seq_len(nvars)) {
      vals_n <- c(vals_n, as.character(n))
    }
    row_strings <- c(row_strings, .typst_row(vals_n))
  }

  col_hdrs <- c("", "", variables)

  .typst_raw(paste0(
    "#spss_table(\n",
    "  title: \"Correlations\",\n",
    "  label_cols: 2,\n",
    "  columns: (", paste(sapply(col_hdrs, .typst_cell), collapse = ", "), "),\n",
    "  rows: (\n",
    paste(row_strings, collapse = "\n"), "\n",
    "  ),\n",
    "  footnotes: (\n",
    "    [\\*\\*. Correlation is significant at the 0.01 level (2-tailed).],\n",
    "    [\\*. Correlation is significant at the 0.05 level (2-tailed).],\n",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


# =============================================================================
# REGRESSION (Typst)
# =============================================================================

#' SPSS-Style Regression Tables (Typst)
#'
#' Outputs Typst tables for linear regression (Model Summary, ANOVA, Coefficients).
#'
#' @param data A data frame.
#' @param formula_str Character. Regression formula as string (e.g., "y ~ x1 + x2").
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_regression <- function(data, formula_str, digits = 3) {

  fm <- stats::as.formula(formula_str)
  fit <- stats::lm(fm, data = data)
  s <- summary(fit)

  r <- sqrt(s$r.squared)
  r2 <- s$r.squared
  adj_r2 <- s$adj.r.squared
  se_est <- s$sigma

  # Model Summary
  .typst_raw(paste0(
    "#spss_table(\n",
    "  title: \"Model Summary\",\n",
    "  columns: (\"Model\", \"R\", \"R Square\", \"Adjusted R Square\",",
    " \"Std. Error of the Estimate\"),\n",
    "  rows: (\n",
    .typst_row(c("1", .typst_fmt(r, digits), .typst_fmt(r2, digits),
                 .typst_fmt(adj_r2, digits), .typst_fmt(se_est, digits))), "\n",
    "  ),\n",
    ")\n"
  ))

  # ANOVA table
  aov_tbl <- stats::anova(fit)
  n_terms <- nrow(aov_tbl)
  ss_reg <- sum(aov_tbl$`Sum Sq`[-n_terms])
  ss_res <- aov_tbl$`Sum Sq`[n_terms]
  df_reg <- sum(aov_tbl$Df[-n_terms])
  df_res <- aov_tbl$Df[n_terms]
  ms_reg <- ss_reg / df_reg
  ms_res <- ss_res / df_res
  f_val <- ms_reg / ms_res
  p_val <- stats::pf(f_val, df_reg, df_res, lower.tail = FALSE)

  .typst_raw(paste0(
    "#spss_table(\n",
    "  title: \"ANOVA\",\n",
    "  columns: (\"Model\", \"Sum of Squares\", \"df\",",
    " \"Mean Square\", \"F\", \"Sig.\"),\n",
    "  rows: (\n",
    .typst_row(c("Regression", .typst_fmt(ss_reg, digits), df_reg,
                 .typst_fmt(ms_reg, digits), .typst_fmt(f_val, digits),
                 .typst_fmt_p(p_val, digits))), "\n",
    .typst_row(c("Residual", .typst_fmt(ss_res, digits), df_res,
                 .typst_fmt(ms_res, digits), "", "")), "\n",
    .typst_row(c("Total", .typst_fmt(ss_reg + ss_res, digits),
                 df_reg + df_res, "", "", "")), "\n",
    "  ),\n",
    ")\n"
  ))

  # Coefficients
  coefs <- s$coefficients
  row_strings <- c()

  for (i in seq_len(nrow(coefs))) {
    name <- rownames(coefs)[i]
    if (name == "(Intercept)") name <- "(Constant)"

    row_strings <- c(row_strings, .typst_row(c(
      name,
      .typst_fmt(coefs[i, "Estimate"], digits),
      .typst_fmt(coefs[i, "Std. Error"], digits),
      "",
      .typst_fmt(coefs[i, "t value"], digits),
      .typst_fmt_p(coefs[i, "Pr(>|t|)"], digits)
    )))
  }

  .typst_raw(paste0(
    "#spss_table_complex(\n",
    "  title: \"Coefficients\",\n",
    "  ncols: 6,\n",
    "  label_cols: 1,\n",
    "  header_rows: (\n",
    "    ([], [Unstandardized Coefficients], [],",
    " [Standardized Coefficients], [], []),\n",
    "    ([Model], [B], [Std. Error], [Beta], [t], [Sig.]),\n",
    "  ),\n",
    "  rows: (\n",
    paste(row_strings, collapse = "\n"), "\n",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


# =============================================================================
# T-TESTS (Typst)
# =============================================================================

#' SPSS-Style Group Statistics Table (Typst)
#'
#' Outputs a Typst table of group statistics for t-tests.
#'
#' @param data A data frame.
#' @param dv Character. Dependent variable name.
#' @param iv Character. Independent (grouping) variable name.
#' @param data_raw Optional. Original haven-imported data for labels.
#' @param digits Integer. Decimal places. Default 4.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_group_stats <- function(data, dv, iv, data_raw = NULL, digits = 4) {

  groups <- split(data[[dv]], data[[iv]])
  val_labels <- .typst_get_val_labels(data_raw, iv)

  row_strings <- c()
  first <- TRUE

  for (g in names(groups)) {
    x <- groups[[g]][!is.na(groups[[g]])]
    label <- if (!is.null(val_labels) && g %in% names(val_labels)) {
      val_labels[[g]]
    } else {
      g
    }
    row_strings <- c(row_strings, .typst_row(c(
      if (first) dv else "",
      label, length(x), .typst_fmt(mean(x), digits),
      .typst_fmt(stats::sd(x), digits), .typst_fmt(stats::sd(x) / sqrt(length(x)), digits)
    )))
    first <- FALSE
  }

  .typst_raw(paste0(
    "#spss_table(\n",
    "  title: \"Group Statistics\",\n",
    "  label_cols: 2,\n",
    "  columns: (\"\", \"", .typst_esc(iv), "\", \"N\", \"Mean\",",
    " \"Std. Deviation\", \"Std. Error Mean\"),\n",
    "  rows: (\n",
    paste(row_strings, collapse = "\n"), "\n",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


#' SPSS-Style Independent Samples T-Test Table (Typst)
#'
#' Outputs a Typst table for independent samples t-test.
#'
#' @param data A data frame.
#' @param dv Character. Dependent variable name.
#' @param iv Character. Independent variable name (must have 2 levels).
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_ttest_independent <- function(data, dv, iv, digits = 3) {

  groups <- split(data[[dv]], data[[iv]])
  if (length(groups) != 2) {
    stop("Independent t-test requires exactly 2 groups in '", iv, "'")
  }

  g1 <- groups[[1]][!is.na(groups[[1]])]
  g2 <- groups[[2]][!is.na(groups[[2]])]

  lev <- stats::var.test(g1, g2)
  t_eq <- stats::t.test(g1, g2, var.equal = TRUE)
  t_welch <- stats::t.test(g1, g2, var.equal = FALSE)
  md <- mean(g1) - mean(g2)

  .typst_raw(paste0(
    "#spss_table_complex(\n",
    "  title: \"Independent Samples Test\",\n",
    "  ncols: 10,\n",
    "  label_cols: 2,\n",
    "  header_rows: (\n",
    "    ([], [], [Levene's Test], [],\n",
    "     [t-test for Equality of Means], [], [], [], [], []),\n",
    "    ([], [], [F], [Sig.], [t], [df], [Sig. (2-tailed)],\n",
    "     [Mean Difference], [Std. Error Difference],\n",
    "     [95% CI]),\n",
    "  ),\n",
    "  rows: (\n",
    "    ([", .typst_esc(dv), "], [Equal variances assumed], [",
    .typst_fmt(lev$statistic, digits), "], [",
    .typst_fmt_p(lev$p.value, digits), "], [",
    .typst_fmt(t_eq$statistic, digits), "], [",
    t_eq$parameter, "], [",
    .typst_fmt_p(t_eq$p.value, digits), "], [",
    .typst_fmt(md, digits), "], [",
    .typst_fmt(t_eq$stderr, digits), "], [",
    .typst_fmt(t_eq$conf.int[1], digits), " to ",
    .typst_fmt(t_eq$conf.int[2], digits), "]),\n",
    "    ([], [Equal variances not assumed], [], [], [",
    .typst_fmt(t_welch$statistic, digits), "], [",
    .typst_fmt(t_welch$parameter, digits), "], [",
    .typst_fmt_p(t_welch$p.value, digits), "], [",
    .typst_fmt(md, digits), "], [",
    .typst_fmt(t_welch$stderr, digits), "], [",
    .typst_fmt(t_welch$conf.int[1], digits), " to ",
    .typst_fmt(t_welch$conf.int[2], digits), "]),\n",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


#' SPSS-Style Paired Samples T-Test Table (Typst)
#'
#' Outputs a Typst table for paired samples t-test.
#'
#' @param data A data frame.
#' @param var1 Character. First variable name.
#' @param var2 Character. Second variable name.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_ttest_paired <- function(data, var1, var2, digits = 3) {

  x1 <- data[[var1]]
  x2 <- data[[var2]]
  complete <- stats::complete.cases(x1, x2)
  x1 <- x1[complete]
  x2 <- x2[complete]
  d <- x1 - x2
  tobj <- stats::t.test(x1, x2, paired = TRUE)

  .typst_raw(paste0(
    "#spss_table(\n",
    "  title: \"Paired Samples Test\",\n",
    "  columns: (\"\", \"Mean\", \"Std. Deviation\", \"Std. Error Mean\",",
    " \"t\", \"df\", \"Sig. (2-tailed)\"),\n",
    "  rows: (\n",
    .typst_row(c(
      paste0(var1, " - ", var2),
      .typst_fmt(mean(d), digits), .typst_fmt(stats::sd(d), digits),
      .typst_fmt(stats::sd(d) / sqrt(length(d)), digits),
      .typst_fmt(tobj$statistic, digits), tobj$parameter,
      .typst_fmt_p(tobj$p.value, digits)
    )), "\n",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


#' SPSS-Style One-Sample T-Test Table (Typst)
#'
#' Outputs a Typst table for one-sample t-test.
#'
#' @param data A data frame.
#' @param variable Character. Variable name.
#' @param mu Numeric. Test value (hypothesized mean). Default 0.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_ttest_one <- function(data, variable, mu = 0, digits = 3) {

  x <- data[[variable]][!is.na(data[[variable]])]
  tobj <- stats::t.test(x, mu = mu)

  .typst_raw(paste0(
    "#spss_table_complex(\n",
    "  title: \"One-Sample Test\",\n",
    "  subtitle: \"Test Value = ", mu, "\",\n",
    "  ncols: 6,\n",
    "  label_cols: 1,\n",
    "  header_rows: (\n",
    "    ([], [], [], [], [95% Confidence Interval of the Difference], []),\n",
    "    ([], [t], [df], [Sig. (2-tailed)], [Lower], [Upper]),\n",
    "  ),\n",
    "  rows: (\n",
    .typst_row(c(
      variable, .typst_fmt(tobj$statistic, digits), tobj$parameter,
      .typst_fmt_p(tobj$p.value, digits),
      .typst_fmt(tobj$conf.int[1] - mu, digits),
      .typst_fmt(tobj$conf.int[2] - mu, digits)
    )), "\n",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


# =============================================================================
# KRUSKAL-WALLIS TEST (Typst)
# =============================================================================

#' SPSS-Style Kruskal-Wallis Test Table (Typst)
#'
#' Outputs a Typst table for Kruskal-Wallis test.
#'
#' @param data A data frame.
#' @param dv Character. Dependent variable name.
#' @param iv Character. Independent variable name.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @export
typst_kruskal <- function(data, dv, iv, digits = 3) {

  kt <- stats::kruskal.test(
    stats::as.formula(paste(dv, "~ factor(", iv, ")")),
    data = data
  )

  .typst_raw(paste0(
    "#spss_table(\n",
    "  title: \"Test Statistics\",\n",
    "  subtitle: \"Kruskal-Wallis Test\",\n",
    "  label_cols: 1,\n",
    "  columns: (\"\", \"", .typst_esc(dv), "\"),\n",
    "  rows: (\n",
    .typst_row(c("Kruskal-Wallis H", .typst_fmt(kt$statistic, digits))), "\n",
    .typst_row(c("df", kt$parameter)), "\n",
    .typst_row(c("Asymp. Sig.", .typst_fmt_p(kt$p.value, digits))), "\n",
    "  ),\n",
    ")\n"
  ))

  invisible(NULL)
}


# =============================================================================
# STYLED BOX WRAPPER (Typst)
# =============================================================================

#' Output Text in Styled Typst Box
#'
#' Outputs text inside a styled Typst box (writeup, tip, warning, etc.).
#'
#' @param text Character. Text to display.
#' @param box_type Character. One of "writeup", "tip", "warning", "important", "note".
#' @param side Character. For margin notes: "left" or "right". Default "right".
#'
#' @return NULL (outputs Typst markup via cat()).
#'
#' @examples
#' \dontrun{
#' typst_box("This is my APA writeup text.", "writeup")
#' typst_box("Remember this tip!", "tip", side = "left")
#' }
#'
#' @export
typst_box <- function(text, box_type = "writeup", side = "right") {

  valid_types <- c("writeup", "tip", "warning", "important", "note")
  if (!box_type %in% valid_types) {
    stop("box_type must be one of: ", paste(valid_types, collapse = ", "))
  }

  escaped_text <- .typst_esc(text)

  if (box_type == "writeup") {
    .typst_raw(paste0("#writeup[\n", escaped_text, "\n]"))
  } else {
    .typst_raw(paste0("#", box_type, "(side: ", side, ")[\n", escaped_text, "\n]"))
  }

  invisible(NULL)
}
