# =============================================================================
# spss_tables.R
# SPSS-Style Tables for HTML and PNG Output
# =============================================================================

# Declare global variables used in ggplot2 aes() to avoid R CMD check notes
utils::globalVariables(c("label", "count", "x", "density"))

# -----------------------------------------------------------------------------
# INTERNAL HELPERS
# -----------------------------------------------------------------------------

#' Format numeric value for SPSS-style display
#' @noRd
.spss_fmt <- function(x, digits = 3) {
  if (is.na(x) || is.null(x)) return("")

  formatC(x, digits = digits, format = "f")
}

#' Format p-value SPSS-style (no leading zero)
#' @noRd
.spss_fmt_p <- function(p, digits = 3) {
  if (is.na(p) || is.null(p)) return("")
  if (p < 0.001) return("<.001")
  sub("^0\\.", ".", formatC(p, digits = digits, format = "f"))
}

#' Get variable label from haven-imported data
#' @noRd
.get_var_label <- function(data_raw, var) {
  if (is.null(data_raw)) return(var)
  lbl <- attr(data_raw[[var]], "label")
  if (!is.null(lbl)) lbl else var
}

#' Get value labels from haven-imported data
#' @noRd
.get_val_labels <- function(data_raw, var) {
  if (is.null(data_raw)) return(NULL)
  lbls <- attr(data_raw[[var]], "labels")
  if (!is.null(lbls)) {
    stats::setNames(names(lbls), as.character(lbls))
  } else {
    NULL
  }
}

#' SPSS table CSS styles - matches spss-tables.typ exactly
#' @noRd
.spss_table_css <- function() {
  '
  <style>
  .spss-container {
    font-family: Arial, "Helvetica Neue", sans-serif;
    font-size: 9pt;
    margin: 16px 0;
  }
  .spss-title {
    font-weight: bold;
    font-size: 10pt;
    color: black;
    margin-bottom: 4px;
    text-align: center;
  }
  .spss-subtitle {
    font-size: 9pt;
    color: black;
    margin-bottom: 4px;
    text-align: center;
  }
  .spss-table {
    border-collapse: collapse;
    border-spacing: 0;
    border-top: 1px solid black;
    border-bottom: 1px solid black;
  }
  /* Header cells - gray background, dark blue text */
  .spss-table thead th {
    background-color: #e4e4e4;
    color: #264a60;
    font-weight: bold;
    font-size: 8pt;
    padding: 3px 8px;
    text-align: right;
    vertical-align: middle;
    border-bottom: 1px solid black;
    border-right: 1px solid #aeaeae;
  }
  .spss-table thead th:last-child {
    border-right: none;
  }
  /* Data cells - very light gray background */
  .spss-table tbody td {
    padding: 3px 8px;
    text-align: right;
    font-size: 9pt;
    color: #1a1a1a;
    vertical-align: middle;
    background-color: #f9f9fb;
    border-bottom: 1px solid #aeaeae;
    border-right: 1px solid #aeaeae;
  }
  .spss-table tbody td:last-child {
    border-right: none;
  }
  .spss-table tbody tr:last-child td {
    border-bottom: none;
  }
  /* Row label cells - gray background, left aligned */
  .spss-table tbody td.spss-rowlabel {
    text-align: left;
    background-color: #e4e4e4;
  }
  .spss-table tbody td.spss-rowlabel-sub {
    text-align: left;
    background-color: #e4e4e4;
  }
  /* Footnotes */
  .spss-footnotes {
    font-size: 8pt;
    color: #555;
    margin-top: 4px;
  }
  .spss-footnotes p {
    margin: 2px 0;
  }

  /* === SIMPLE TABLE STYLE (for crosstabs etc) === */
  .spss-table-simple {
    border-collapse: collapse;
    border-spacing: 0;
    border-top: 1px solid black;
    border-bottom: 1px solid black;
  }
  .spss-table-simple thead th {
    background-color: #e4e4e4;
    color: #264a60;
    font-weight: bold;
    font-size: 8pt;
    padding: 3px 8px;
    text-align: right;
    vertical-align: middle;
    border-bottom: 1px solid black;
    border-right: 1px solid #aeaeae;
  }
  .spss-table-simple thead th:last-child {
    border-right: none;
  }
  .spss-table-simple thead th:first-child,
  .spss-table-simple thead th:nth-child(2) {
    text-align: left;
  }
  .spss-table-simple tbody td {
    padding: 3px 8px;
    text-align: right;
    font-size: 9pt;
    color: #1a1a1a;
    vertical-align: middle;
    background-color: #f9f9fb;
    border-bottom: 1px solid #aeaeae;
    border-right: 1px solid #aeaeae;
  }
  .spss-table-simple tbody td:last-child {
    border-right: none;
  }
  .spss-table-simple tbody tr:last-child td {
    border-bottom: none;
  }
  .spss-table-simple tbody td:first-child,
  .spss-table-simple tbody td:nth-child(2) {
    text-align: left;
    background-color: #e4e4e4;
  }
  </style>
  '
}

# =============================================================================
# DESCRIPTIVES TABLE
# =============================================================================

#' SPSS-Style Descriptives Table (HTML)
#'
#' Creates an HTML table displaying descriptive statistics in SPSS format.
#'
#' @param data A data frame.
#' @param variables Character vector of variable names to summarize.
#' @param digits Integer. Number of decimal places. Default 2.
#'
#' @return HTML string (invisibly). Prints to console with `cat()`.
#'
#' @examples
#' data(superman)
#' spss_descriptives_html(superman, c("clark_height_in", "rt_critics_score"))
#'
#' @export
spss_descriptives_html <- function(data, variables, digits = 2) {
  valid_n <- nrow(data[stats::complete.cases(data[, variables, drop = FALSE]), , drop = FALSE])
  rows_html <- ""
  for (var in variables) {
    x <- data[[var]]
    x <- x[!is.na(x)]
    n <- length(x)
    m <- mean(x)
    s <- stats::sd(x)
    se <- s / sqrt(n)
    mn <- min(x)
    mx <- max(x)
    rows_html <- paste0(rows_html, sprintf(
      '<tr><td class="spss-rowlabel">%s</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n',
      htmltools::htmlEscape(var), n, .spss_fmt(m, digits), .spss_fmt(s, digits),
      .spss_fmt(se, digits), .spss_fmt(mn, digits), .spss_fmt(mx, digits)
    ))
  }
  rows_html <- paste0(rows_html, sprintf(
    '<tr><td class="spss-rowlabel">Valid N (listwise)</td><td>%d</td><td></td><td></td><td></td><td></td><td></td></tr>\n',
    valid_n
  ))
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">Descriptive Statistics</div>\n',
    '<table class="spss-table">\n',
    '<thead>\n',
    '<tr ><th></th><th>N</th><th>Mean</th><th>Std. Deviation</th><th>Std. Error</th><th>Minimum</th><th>Maximum</th></tr>\n',
    '</thead>\n',
    '<tbody>\n',
    rows_html,
    '</tbody>\n',
    '</table>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

#' SPSS-Style Descriptives Table (PNG)
#'
#' Saves an SPSS-style descriptives table as a PNG image.
#' Requires the \pkg{gt} package (listed in Suggests).
#'
#' @inheritParams spss_descriptives_html
#' @param filename Character. Output filename (should end in .png).
#' @param width Numeric. Image width in inches. Default 7.
#' @param height Numeric. Image height in inches. Default NULL (auto).
#' @param dpi Numeric. Resolution. Default 150.
#'
#' @return The filename (invisibly).
#'
#' @export
spss_descriptives_png <- function(data, variables, filename = "descriptives.png",
                                  digits = 2, width = 7, height = NULL, dpi = 150) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required for PNG output. Install with: install.packages('gt')")
  }
  valid_n <- nrow(data[stats::complete.cases(data[, variables, drop = FALSE]), , drop = FALSE])
  tbl_data <- data.frame(
    Variable = character(), N = integer(), Mean = numeric(),
    `Std. Deviation` = numeric(), `Std. Error` = numeric(),
    Minimum = numeric(), Maximum = numeric(),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  for (var in variables) {
    x <- data[[var]]
    x <- x[!is.na(x)]
    tbl_data <- rbind(tbl_data, data.frame(
      Variable = var, N = length(x), Mean = mean(x),
      `Std. Deviation` = stats::sd(x), `Std. Error` = stats::sd(x) / sqrt(length(x)),
      Minimum = min(x), Maximum = max(x),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }
  tbl_data <- rbind(tbl_data, data.frame(
    Variable = "Valid N (listwise)", N = valid_n, Mean = NA,
    `Std. Deviation` = NA, `Std. Error` = NA, Minimum = NA, Maximum = NA,
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  gt_tbl <- gt::gt(tbl_data) |>
    gt::tab_header(title = "Descriptive Statistics") |>
    gt::fmt_number(columns = c("Mean", "Std. Deviation", "Std. Error", "Minimum", "Maximum"), decimals = digits) |>
    gt::sub_missing(missing_text = "") |>
    gt::tab_style(style = list(gt::cell_fill(color = "#4A7A91"), gt::cell_text(color = "white")),
                  locations = gt::cells_column_labels()) |>
    gt::tab_style(style = gt::cell_fill(color = "#F5F5ED"), locations = gt::cells_body(columns = "Variable")) |>
    gt::tab_options(table.font.names = "Arial", table.font.size = gt::px(11),
                    table.border.top.style = "hidden", table.border.bottom.width = gt::px(2),
                    table.border.bottom.color = "#4A7A91", column_labels.border.bottom.color = "#4A7A91")
  if (is.null(height)) height <- 1 + 0.3 * nrow(tbl_data)
  gt::gtsave(gt_tbl, filename = filename, vwidth = width * dpi, vheight = height * dpi)
  invisible(filename)
}

# =============================================================================
# DESCRIPTIVES BY GROUP
# =============================================================================

#' SPSS-Style Grouped Descriptives Table (HTML)
#'
#' Creates an HTML table displaying descriptive statistics by group.
#'
#' @param data A data frame.
#' @param dv Character. Name of the dependent variable.
#' @param iv Character. Name of the independent (grouping) variable.
#' @param data_raw Optional. Original haven-imported data for value labels.
#' @param digits Integer. Decimal places. Default 2.
#' @param conf.level Numeric. Confidence level for CI. Default 0.95.
#'
#' @return HTML string (invisibly).
#'
#' @examples
#' data(superman)
#' spss_descriptives_by_group_html(superman, "rt_critics_score", "clark_grp")
#'
#' @export
spss_descriptives_by_group_html <- function(data, dv, iv, data_raw = NULL,
                                            digits = 2, conf.level = 0.95) {
  dv_vals <- data[[dv]]
  iv_vals <- data[[iv]]
  groups <- split(dv_vals, iv_vals)
  val_labels <- .get_val_labels(data_raw, iv)
  rows_html <- ""
  for (g in names(groups)) {
    x <- groups[[g]]
    x <- x[!is.na(x)]
    n <- length(x)
    if (n == 0) next
    m <- mean(x)
    s <- if (n > 1) stats::sd(x) else NA
    se <- if (n > 1) s / sqrt(n) else NA
    ci <- if (n > 1) tryCatch(stats::t.test(x, conf.level = conf.level)$conf.int, error = function(e) c(NA, NA)) else c(NA, NA)
    label <- if (!is.null(val_labels) && g %in% names(val_labels)) val_labels[[g]] else g
    rows_html <- paste0(rows_html, sprintf(
      '<tr><td class="spss-rowlabel">%s</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n',
      htmltools::htmlEscape(label), n, .spss_fmt(m, digits), .spss_fmt(s, digits),
      .spss_fmt(se, digits), .spss_fmt(ci[1], digits), .spss_fmt(ci[2], digits),
      .spss_fmt(min(x), digits), .spss_fmt(max(x), digits)
    ))
  }
  x_all <- dv_vals[!is.na(dv_vals)]
  n_all <- length(x_all)
  ci_all <- tryCatch(stats::t.test(x_all, conf.level = conf.level)$conf.int, error = function(e) c(NA, NA))
  rows_html <- paste0(rows_html, sprintf(
    '<tr><td class="spss-rowlabel">Total</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n',
    n_all, .spss_fmt(mean(x_all), digits), .spss_fmt(stats::sd(x_all), digits),
    .spss_fmt(stats::sd(x_all) / sqrt(n_all), digits),
    .spss_fmt(ci_all[1], digits), .spss_fmt(ci_all[2], digits),
    .spss_fmt(min(x_all), digits), .spss_fmt(max(x_all), digits)
  ))
  ci_pct <- paste0(round(conf.level * 100), "% Confidence Interval for Mean")
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">Descriptives</div>\n',
    '<div class="spss-subtitle">', htmltools::htmlEscape(dv), '</div>\n',
    '<table class="spss-table">\n',
    '<thead>\n',
    '<tr ><th rowspan="2"></th><th rowspan="2">N</th><th rowspan="2">Mean</th><th rowspan="2">Std. Deviation</th><th rowspan="2">Std. Error</th><th colspan="2">', ci_pct, '</th><th rowspan="2">Minimum</th><th rowspan="2">Maximum</th></tr>\n',
    '<tr ><th>Lower Bound</th><th>Upper Bound</th></tr>\n',
    '</thead>\n',
    '<tbody>\n',
    rows_html,
    '</tbody>\n',
    '</table>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

# =============================================================================
# ONE-WAY ANOVA TABLE
# =============================================================================

#' SPSS-Style One-Way ANOVA Table (HTML)
#'
#' Creates an HTML ANOVA table from bg_anova_answers() output or raw data.
#'
#' @param data A data frame, OR output from [bg_anova_answers()].
#' @param dv Character. DV name (ignored if data is anova results).
#' @param iv Character. IV name (ignored if data is anova results).
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return HTML string (invisibly).
#'
#' @examples
#' data(superman)
#' spss_anova_html(superman, "rt_critics_score", "clark_grp")
#'
#' @export
spss_anova_html <- function(data, dv = NULL, iv = NULL, digits = 3) {
  if (is.list(data) && "ANOVA" %in% names(data)) {
    anova_results <- data
    if (is.null(dv)) dv <- "Dependent Variable"
  } else {
    if (is.null(dv) || is.null(iv)) stop("Must provide 'dv' and 'iv' when passing raw data")
    formula <- stats::as.formula(paste(dv, "~ factor(", iv, ")"))
    fit <- stats::aov(formula, data = data)
    s <- summary(fit)[[1]]
    anova_results <- list(ANOVA = list(
      F = s$`F value`[1], p_value = s$`Pr(>F)`[1], df_between = s$Df[1], df_within = s$Df[2],
      ss_between = s$`Sum Sq`[1], ss_within = s$`Sum Sq`[2], ms_between = s$`Mean Sq`[1], ms_within = s$`Mean Sq`[2]
    ))
  }
  a <- anova_results$ANOVA
  ss_b <- if (!is.null(a$ss_between)) a$ss_between else NA
  ss_w <- if (!is.null(a$ss_within)) a$ss_within else NA
  ms_b <- if (!is.null(a$ms_between)) a$ms_between else NA
  ms_w <- if (!is.null(a$ms_within)) a$ms_within else a$mse
  if (is.na(ss_b) && !is.null(ms_b) && !is.null(a$df_between)) ss_b <- ms_b * a$df_between
  if (is.na(ss_w) && !is.null(ms_w) && !is.null(a$df_within)) ss_w <- ms_w * a$df_within
  ss_t <- if (!is.na(ss_b) && !is.na(ss_w)) ss_b + ss_w else NA
  df_t <- a$df_between + a$df_within
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">ANOVA</div>\n',
    '<div class="spss-subtitle">', htmltools::htmlEscape(dv), '</div>\n',
    '<table class="spss-table">\n',
    '<thead>\n',
    '<tr ><th></th><th>Sum of Squares</th><th>df</th><th>Mean Square</th><th>F</th><th>Sig.</th></tr>\n',
    '</thead>\n',
    '<tbody>\n',
    sprintf('<tr><td class="spss-rowlabel">Between Groups</td><td>%s</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td></tr>\n',
            .spss_fmt(ss_b, digits), a$df_between, .spss_fmt(ms_b, digits), .spss_fmt(a$F, digits), .spss_fmt_p(a$p_value, digits)),
    sprintf('<tr><td class="spss-rowlabel">Within Groups</td><td>%s</td><td>%d</td><td>%s</td><td></td><td></td></tr>\n',
            .spss_fmt(ss_w, digits), a$df_within, .spss_fmt(ms_w, digits)),
    sprintf('<tr><td class="spss-rowlabel">Total</td><td>%s</td><td>%d</td><td></td><td></td><td></td></tr>\n',
            .spss_fmt(ss_t, digits), df_t),
    '</tbody>\n',
    '</table>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

# =============================================================================
# LEVENE'S TEST
# =============================================================================

#' SPSS-Style Levene's Test Table (HTML)
#'
#' Creates an HTML table for Levene's test of homogeneity of variances.
#' Requires the \pkg{car} package (listed in Suggests).
#'
#' @param data A data frame.
#' @param dv Character. Dependent variable name.
#' @param iv Character. Independent variable name.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return HTML string (invisibly).
#'
#' @export
spss_levene_html <- function(data, dv, iv, digits = 3) {
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Package 'car' is required for Levene's test. Install with: install.packages('car')")
  }
  formula <- stats::as.formula(paste(dv, "~", "factor(", iv, ")"))
  lev <- car::leveneTest(formula, data = data)
  f_val <- lev$`F value`[1]
  df1 <- lev$Df[1]
  df2 <- lev$Df[2]
  p_val <- lev$`Pr(>F)`[1]
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">Test of Homogeneity of Variances</div>\n',
    '<div class="spss-subtitle">', htmltools::htmlEscape(dv), '</div>\n',
    '<table class="spss-table">\n',
    '<thead>\n',
    '<tr ><th>Levene Statistic</th><th>df1</th><th>df2</th><th>Sig.</th></tr>\n',
    '</thead>\n',
    '<tbody>\n',
    sprintf('<tr><td>%s</td><td>%d</td><td>%d</td><td>%s</td></tr>\n',
            .spss_fmt(f_val, digits), df1, df2, .spss_fmt_p(p_val, digits)),
    '</tbody>\n',
    '</table>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

# =============================================================================
# CHI-SQUARE TEST
# =============================================================================

#' SPSS-Style Chi-Square Test Table (HTML)
#'
#' Creates an HTML chi-square test table from chi_square_answers() output or raw data.
#'
#' @param data A data frame, OR output from [chi_square_answers()].
#' @param var1 Character. First variable name (ignored if data is chi results).
#' @param var2 Character. Second variable name (ignored if data is chi results).
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return HTML string (invisibly).
#'
#' @examples
#' data(superman)
#' spss_chisq_html(superman, "clark_grp", "tomatometer")
#'
#' @export
spss_chisq_html <- function(data, var1 = NULL, var2 = NULL, digits = 3) {
  if (is.list(data) && "ChiSquare" %in% names(data)) {
    chi_results <- data
    tab <- chi_results$ContingencyTable
    chi_stat <- chi_results$ChiSquare$chi_sq
    p_val <- chi_results$ChiSquare$p_value_raw
    df <- chi_results$ChiSquare$df
    n <- chi_results$Sample_Size
    expected <- chi_results$Expected
  } else {
    if (is.null(var1) || is.null(var2)) stop("Must provide 'var1' and 'var2' when passing raw data")
    tab <- table(data[[var1]], data[[var2]])
    chi <- stats::chisq.test(tab, correct = FALSE)
    chi_stat <- chi$statistic
    p_val <- chi$p.value
    df <- chi$parameter
    n <- sum(tab)
    expected <- chi$expected
  }
  is_2x2 <- nrow(tab) == 2 && ncol(tab) == 2
  low_expected <- sum(expected < 5)
  pct_low <- 100 * low_expected / length(expected)
  min_expected <- min(expected)
  rows_html <- sprintf(
    '<tr><td class="spss-rowlabel">Pearson Chi-Square</td><td>%s</td><td>%d</td><td>%s</td></tr>\n',
    .spss_fmt(chi_stat, digits), df, .spss_fmt_p(p_val, digits)
  )
  if (is_2x2) {
    chi_corrected <- stats::chisq.test(tab, correct = TRUE)
    rows_html <- paste0(rows_html, sprintf(
      '<tr><td class="spss-rowlabel">Continuity Correction<sup>b</sup></td><td>%s</td><td>%d</td><td>%s</td></tr>\n',
      .spss_fmt(chi_corrected$statistic, digits), chi_corrected$parameter, .spss_fmt_p(chi_corrected$p.value, digits)
    ))
  }
  rows_html <- paste0(rows_html, sprintf('<tr><td class="spss-rowlabel">N of Valid Cases</td><td>%d</td><td></td><td></td></tr>\n', n))
  footnote1 <- sprintf("a. %d cells (%.1f%%) have expected count less than 5. The minimum expected count is %.2f.", low_expected, pct_low, min_expected)
  footnote2 <- if (is_2x2) "b. Computed only for a 2x2 table." else ""
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">Chi-Square Tests</div>\n',
    '<table class="spss-table">\n',
    '<thead>\n',
    '<tr ><th></th><th>Value</th><th>df</th><th>Asymptotic Significance (2-sided)</th></tr>\n',
    '</thead>\n',
    '<tbody>\n',
    rows_html,
    '</tbody>\n',
    '</table>\n',
    '<div class="spss-footnotes">\n',
    '<p>', footnote1, '</p>\n',
    if (nchar(footnote2) > 0) paste0('<p>', footnote2, '</p>\n') else "",
    '</div>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

# =============================================================================
# CROSSTABULATION
# =============================================================================

#' SPSS-Style Crosstabulation Table (HTML)
#'
#' Creates an HTML crosstabulation table with observed and expected counts.
#'
#' @param data A data frame, OR output from [chi_square_answers()].
#' @param var1 Character. Row variable name.
#' @param var2 Character. Column variable name.
#' @param data_raw Optional. Original haven-imported data for labels.
#' @param digits Integer. Decimal places for expected counts. Default 1.
#'
#' @return HTML string (invisibly).
#'
#' @export
spss_crosstab_html <- function(data, var1 = NULL, var2 = NULL, data_raw = NULL, digits = 1) {
  if (is.list(data) && "ChiSquare" %in% names(data)) {
    chi_results <- data
    tab <- chi_results$ContingencyTable
    expected <- as.matrix(chi_results$Expected)
    if (is.null(var1)) var1 <- "Variable 1"
    if (is.null(var2)) var2 <- "Variable 2"
  } else {
    if (is.null(var1) || is.null(var2)) stop("Must provide 'var1' and 'var2' when passing raw data")
    tab <- table(data[[var1]], data[[var2]])
    chi <- stats::chisq.test(tab, correct = FALSE)
    expected <- chi$expected
  }
  nr <- nrow(tab)
  nc <- ncol(tab)
  rows1_labels <- .get_val_labels(data_raw, var1)
  rows2_labels <- .get_val_labels(data_raw, var2)
  .rl <- function(val, labels) if (!is.null(labels) && val %in% names(labels)) labels[[val]] else val
  
  # Build header with column variable values
  col_hdrs <- sapply(colnames(tab), function(x) .rl(x, rows2_labels))
  header_html <- paste0(
    '<tr><th>', htmltools::htmlEscape(var1), '</th><th></th>',
    paste(sapply(col_hdrs, function(h) paste0('<th>', htmltools::htmlEscape(h), '</th>')), collapse = ""),
    '<th>Total</th></tr>\n'
  )
  
  rows_html <- ""
  for (i in seq_len(nr)) {
    row_label <- .rl(rownames(tab)[i], rows1_labels)
    # Count row
    rows_html <- paste0(rows_html, '<tr><td>', htmltools::htmlEscape(row_label), '</td><td>Count</td>')
    for (j in seq_len(nc)) rows_html <- paste0(rows_html, sprintf('<td>%d</td>', tab[i, j]))
    rows_html <- paste0(rows_html, sprintf('<td>%d</td></tr>\n', sum(tab[i, ])))
    # Expected row
    rows_html <- paste0(rows_html, '<tr><td></td><td>Expected Count</td>')
    for (j in seq_len(nc)) rows_html <- paste0(rows_html, sprintf('<td>%s</td>', .spss_fmt(expected[i, j], digits)))
    rows_html <- paste0(rows_html, sprintf('<td>%s</td></tr>\n', .spss_fmt(sum(expected[i, ]), digits)))
  }
  # Total rows
  col_totals <- colSums(tab)
  exp_totals <- colSums(expected)
  rows_html <- paste0(rows_html, '<tr><td>Total</td><td>Count</td>')
  for (j in seq_len(nc)) rows_html <- paste0(rows_html, sprintf('<td>%d</td>', col_totals[j]))
  rows_html <- paste0(rows_html, sprintf('<td>%d</td></tr>\n', sum(col_totals)))
  rows_html <- paste0(rows_html, '<tr><td></td><td>Expected Count</td>')
  for (j in seq_len(nc)) rows_html <- paste0(rows_html, sprintf('<td>%s</td>', .spss_fmt(exp_totals[j], digits)))
  rows_html <- paste0(rows_html, sprintf('<td>%s</td></tr>\n', .spss_fmt(sum(exp_totals), digits)))
  
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">', htmltools::htmlEscape(var1), ' * ', htmltools::htmlEscape(var2), ' Crosstabulation</div>\n',
    '<table class="spss-table-simple">\n',
    '<thead>\n',
    header_html,
    '</thead>\n',
    '<tbody>\n',
    rows_html,
    '</tbody>\n',
    '</table>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

# =============================================================================
# CORRELATIONS
# =============================================================================

#' SPSS-Style Correlation Matrix Table (HTML)
#'
#' Creates an HTML correlation matrix with significance stars.
#'
#' @param data A data frame.
#' @param variables Character vector of variable names.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return HTML string (invisibly).
#'
#' @examples
#' data(superman)
#' spss_correlations_html(superman, c("clark_height_in", "rt_critics_score", "rt_audience_score"))
#'
#' @export
spss_correlations_html <- function(data, variables, digits = 3) {
  complete <- data[stats::complete.cases(data[, variables, drop = FALSE]), variables, drop = FALSE]
  n <- nrow(complete)
  nvars <- length(variables)
  cor_mat <- stats::cor(complete)
  p_mat <- matrix(NA, nvars, nvars)
  for (i in seq_len(nvars)) {
    for (j in seq_len(nvars)) {
      if (i != j) p_mat[i, j] <- stats::cor.test(complete[[i]], complete[[j]])$p.value
    }
  }
  header_html <- paste0('<tr ><th colspan="2"></th>',
                        paste(sapply(variables, function(v) paste0('<th>', htmltools::htmlEscape(v), '</th>')), collapse = ""), '</tr>\n')
  rows_html <- ""
  for (i in seq_len(nvars)) {
    rows_html <- paste0(rows_html, sprintf('<tr><td class="spss-rowlabel" rowspan="3">%s</td><td class="spss-rowlabel-sub">Pearson Correlation</td>', htmltools::htmlEscape(variables[i])))
    for (j in seq_len(nvars)) {
      r <- cor_mat[i, j]
      star <- ""
      if (i != j && !is.na(p_mat[i, j])) {
        if (p_mat[i, j] < 0.01) star <- "**"
        else if (p_mat[i, j] < 0.05) star <- "*"
      }
      val_str <- if (i == j) "1" else paste0(.spss_fmt(r, digits), star)
      rows_html <- paste0(rows_html, sprintf('<td>%s</td>', val_str))
    }
    rows_html <- paste0(rows_html, '</tr>\n')
    rows_html <- paste0(rows_html, '<tr><td class="spss-rowlabel-sub">Sig. (2-tailed)</td>')
    for (j in seq_len(nvars)) {
      if (i == j) rows_html <- paste0(rows_html, '<td></td>')
      else rows_html <- paste0(rows_html, sprintf('<td>%s</td>', .spss_fmt_p(p_mat[i, j], digits)))
    }
    rows_html <- paste0(rows_html, '</tr>\n')
    rows_html <- paste0(rows_html, '<tr><td class="spss-rowlabel-sub">N</td>')
    for (j in seq_len(nvars)) rows_html <- paste0(rows_html, sprintf('<td>%d</td>', n))
    rows_html <- paste0(rows_html, '</tr>\n')
  }
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">Correlations</div>\n',
    '<table class="spss-table">\n',
    '<thead>\n',
    header_html,
    '</thead>\n',
    '<tbody>\n',
    rows_html,
    '</tbody>\n',
    '</table>\n',
    '<div class="spss-footnotes">\n',
    '<p>**. Correlation is significant at the 0.01 level (2-tailed).</p>\n',
    '<p>*. Correlation is significant at the 0.05 level (2-tailed).</p>\n',
    '</div>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

# =============================================================================
# REGRESSION TABLES
# =============================================================================

#' SPSS-Style Regression Model Summary Table (HTML)
#'
#' Creates an HTML model summary table from regression_answers() output.
#'
#' @param reg_results Output from [regression_answers()].
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return HTML string (invisibly).
#'
#' @export
spss_regression_model_html <- function(reg_results, digits = 3) {
  m <- reg_results$Model
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">Model Summary</div>\n',
    '<table class="spss-table">\n',
    '<thead>\n',
    '<tr ><th>Model</th><th>R</th><th>R Square</th><th>Adjusted R Square</th><th>Std. Error of the Estimate</th></tr>\n',
    '</thead>\n',
    '<tbody>\n',
    sprintf('<tr><td class="spss-rowlabel">1</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n',
            .spss_fmt(m$R, digits), .spss_fmt(m$R_squared, digits), .spss_fmt(m$Adj_R_squared, digits),
            if (!is.null(m$se_estimate)) .spss_fmt(m$se_estimate, digits) else ""),
    '</tbody>\n',
    '</table>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

#' SPSS-Style Regression ANOVA Table (HTML)
#'
#' Creates an HTML ANOVA table for regression from regression_answers() output.
#'
#' @param reg_results Output from [regression_answers()].
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return HTML string (invisibly).
#'
#' @export
spss_regression_anova_html <- function(reg_results, digits = 3) {
  m <- reg_results$Model
  raw_model <- reg_results$Raw_Model
  aov_tbl <- stats::anova(raw_model)
  n_terms <- nrow(aov_tbl)
  ss_reg <- sum(aov_tbl$`Sum Sq`[-n_terms])
  ss_res <- aov_tbl$`Sum Sq`[n_terms]
  df_reg <- m$df1
  df_res <- m$df2
  ms_reg <- ss_reg / df_reg
  ms_res <- ss_res / df_res
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">ANOVA<sup>a</sup></div>\n',
    '<table class="spss-table">\n',
    '<thead>\n',
    '<tr ><th colspan="2">Model</th><th>Sum of Squares</th><th>df</th><th>Mean Square</th><th>F</th><th>Sig.</th></tr>\n',
    '</thead>\n',
    '<tbody>\n',
    sprintf('<tr><td class="spss-rowlabel" rowspan="3">1</td><td class="spss-rowlabel-sub">Regression</td><td>%s</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td></tr>\n',
            .spss_fmt(ss_reg, digits), df_reg, .spss_fmt(ms_reg, digits), .spss_fmt(m$F, digits), .spss_fmt_p(m$p_value, digits)),
    sprintf('<tr><td class="spss-rowlabel-sub">Residual</td><td>%s</td><td>%d</td><td>%s</td><td></td><td></td></tr>\n',
            .spss_fmt(ss_res, digits), df_res, .spss_fmt(ms_res, digits)),
    sprintf('<tr><td class="spss-rowlabel-sub">Total</td><td>%s</td><td>%d</td><td></td><td></td><td></td></tr>\n',
            .spss_fmt(ss_reg + ss_res, digits), df_reg + df_res),
    '</tbody>\n',
    '</table>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

#' SPSS-Style Regression Coefficients Table (HTML)
#'
#' Creates an HTML coefficients table from regression_answers() output.
#'
#' @param reg_results Output from [regression_answers()].
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return HTML string (invisibly).
#'
#' @export
spss_regression_coefficients_html <- function(reg_results, digits = 3) {
  raw_model <- reg_results$Raw_Model
  s <- summary(raw_model)
  coefs <- s$coefficients
  rows_html <- ""
  for (i in seq_len(nrow(coefs))) {
    name <- rownames(coefs)[i]
    if (name == "(Intercept)") name <- "(Constant)"
    rows_html <- paste0(rows_html, sprintf(
      '<tr><td class="spss-rowlabel-sub">%s</td><td>%s</td><td>%s</td><td></td><td>%s</td><td>%s</td></tr>\n',
      htmltools::htmlEscape(name), .spss_fmt(coefs[i, "Estimate"], digits), .spss_fmt(coefs[i, "Std. Error"], digits),
      .spss_fmt(coefs[i, "t value"], digits), .spss_fmt_p(coefs[i, "Pr(>|t|)"], digits)
    ))
  }
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">Coefficients<sup>a</sup></div>\n',
    '<table class="spss-table">\n',
    '<thead>\n',
    '<tr ><th rowspan="2">Model</th><th colspan="2">Unstandardized Coefficients</th><th>Standardized Coefficients</th><th rowspan="2">t</th><th rowspan="2">Sig.</th></tr>\n',
    '<tr ><th>B</th><th>Std. Error</th><th>Beta</th></tr>\n',
    '</thead>\n',
    '<tbody>\n',
    rows_html,
    '</tbody>\n',
    '</table>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

# =============================================================================
# T-TEST TABLES
# =============================================================================

#' SPSS-Style Independent Samples T-Test Table (HTML)
#'
#' Creates an HTML table for independent samples t-test results.
#'
#' @param data A data frame.
#' @param dv Character. Dependent variable name.
#' @param iv Character. Independent (grouping) variable name (must have 2 levels).
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return HTML string (invisibly).
#'
#' @export
spss_ttest_independent_html <- function(data, dv, iv, digits = 3) {
  groups <- split(data[[dv]], data[[iv]])
  if (length(groups) != 2) stop("Independent t-test requires exactly 2 groups in '", iv, "'")
  g1 <- groups[[1]][!is.na(groups[[1]])]
  g2 <- groups[[2]][!is.na(groups[[2]])]
  lev <- stats::var.test(g1, g2)
  t_eq <- stats::t.test(g1, g2, var.equal = TRUE)
  t_welch <- stats::t.test(g1, g2, var.equal = FALSE)
  md <- mean(g1) - mean(g2)
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">Independent Samples Test</div>\n',
    '<table class="spss-table">\n',
    '<thead>\n',
    '<tr>',
    '<th colspan="2"></th>',
    '<th colspan="2">Levene\'s Test</th>',
    '<th colspan="7">t-test for Equality of Means</th>',
    '</tr>\n',
    '<tr>',
    '<th colspan="2"></th>',
    '<th>F</th><th>Sig.</th>',
    '<th>t</th><th>df</th><th>Sig. (2-tailed)</th>',
    '<th>Mean Diff</th><th>SE Diff</th><th>Lower</th><th>Upper</th>',
    '</tr>\n',
    '</thead>\n',
    '<tbody>\n',
    sprintf('<tr><td class="spss-rowlabel" rowspan="2">%s</td><td class="spss-rowlabel-sub">Equal variances assumed</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n',
            htmltools::htmlEscape(dv), .spss_fmt(lev$statistic, digits), .spss_fmt_p(lev$p.value, digits),
            .spss_fmt(t_eq$statistic, digits), .spss_fmt(t_eq$parameter, 0), .spss_fmt_p(t_eq$p.value, digits),
            .spss_fmt(md, digits), .spss_fmt(t_eq$stderr, digits), .spss_fmt(t_eq$conf.int[1], digits), .spss_fmt(t_eq$conf.int[2], digits)),
    sprintf('<tr><td class="spss-rowlabel-sub">Equal variances not assumed</td><td></td><td></td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n',
            .spss_fmt(t_welch$statistic, digits), .spss_fmt(t_welch$parameter, digits), .spss_fmt_p(t_welch$p.value, digits),
            .spss_fmt(md, digits), .spss_fmt(t_welch$stderr, digits), .spss_fmt(t_welch$conf.int[1], digits), .spss_fmt(t_welch$conf.int[2], digits)),
    '</tbody>\n',
    '</table>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

#' SPSS-Style Paired Samples T-Test Table (HTML)
#'
#' Creates an HTML table for paired samples t-test results.
#'
#' @param data A data frame.
#' @param var1 Character. First variable name.
#' @param var2 Character. Second variable name.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return HTML string (invisibly).
#'
#' @export
spss_ttest_paired_html <- function(data, var1, var2, digits = 3) {
  x1 <- data[[var1]]
  x2 <- data[[var2]]
  complete <- stats::complete.cases(x1, x2)
  x1 <- x1[complete]
  x2 <- x2[complete]
  d <- x1 - x2
  tobj <- stats::t.test(x1, x2, paired = TRUE)
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">Paired Samples Test</div>\n',
    '<table class="spss-table">\n',
    '<thead>\n',
    '<tr >',
    '<th colspan="2"></th>',
    '<th colspan="3">Paired Differences</th>',
    '<th rowspan="2">t</th>',
    '<th rowspan="2">df</th>',
    '<th rowspan="2">Sig. (2-tailed)</th>',
    '</tr>\n',
    '<tr >',
    '<th colspan="2"></th>',
    '<th>Mean</th>',
    '<th>Std. Dev</th>',
    '<th>SE Mean</th>',
    '</tr>\n',
    '</thead>\n',
    '<tbody>\n',
    sprintf('<tr><td class="spss-rowlabel">Pair 1</td><td class="spss-rowlabel-sub">%s - %s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%d</td><td>%s</td></tr>\n',
            htmltools::htmlEscape(var1), htmltools::htmlEscape(var2), .spss_fmt(mean(d), digits),
            .spss_fmt(stats::sd(d), digits), .spss_fmt(stats::sd(d) / sqrt(length(d)), digits),
            .spss_fmt(tobj$statistic, digits), tobj$parameter, .spss_fmt_p(tobj$p.value, digits)),
    '</tbody>\n',
    '</table>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

#' SPSS-Style One-Sample T-Test Table (HTML)
#'
#' Creates an HTML table for one-sample t-test results.
#'
#' @param data A data frame.
#' @param variable Character. Variable name.
#' @param mu Numeric. Test value (hypothesized mean). Default 0.
#' @param digits Integer. Decimal places. Default 3.
#'
#' @return HTML string (invisibly).
#'
#' @export
spss_ttest_one_html <- function(data, variable, mu = 0, digits = 3) {
  x <- data[[variable]][!is.na(data[[variable]])]
  tobj <- stats::t.test(x, mu = mu)
  html <- paste0(
    .spss_table_css(),
    '<div class="spss-container">\n',
    '<div class="spss-title">One-Sample Test</div>\n',
    '<table class="spss-table">\n',
    '<thead>\n',
    '<tr ><th rowspan="2"></th><th colspan="6">Test Value = ', mu, '</th></tr>\n',
    '<tr ><th>t</th><th>df</th><th>Sig. (2-tailed)</th><th>Mean Diff</th><th>Lower</th><th>Upper</th></tr>\n',
    '</thead>\n',
    '<tbody>\n',
    sprintf('<tr><td class="spss-rowlabel">%s</td><td>%s</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n',
            htmltools::htmlEscape(variable), .spss_fmt(tobj$statistic, digits), tobj$parameter,
            .spss_fmt_p(tobj$p.value, digits), .spss_fmt(mean(x) - mu, digits),
            .spss_fmt(tobj$conf.int[1] - mu, digits), .spss_fmt(tobj$conf.int[2] - mu, digits)),
    '</tbody>\n',
    '</table>\n',
    '</div>\n'
  )
  cat(html)
  invisible(html)
}

# =============================================================================
# SPSS-STYLE PLOTS
# =============================================================================

#' SPSS-Style Histogram
#'
#' Creates a histogram with SPSS styling.
#'
#' @param data A data frame.
#' @param variable Character. Variable name to plot.
#' @param data_raw Optional. Original haven-imported data for variable label.
#' @param bins Integer. Number of bins. Default 10.
#' @param show_normal Logical. Overlay normal curve. Default TRUE.
#' @param show_stats Logical. Show mean, SD, N annotation. Default TRUE.
#' @param fill_color Character. Bar fill color. Default "#1192E8".
#' @param version Character. "modern" or "legacy" SPSS style.
#'
#' @return A ggplot2 object (invisibly). Also prints the plot.
#'
#' @examples
#' data(superman)
#' spss_histogram(superman, "clark_height_in")
#'
#' @export
spss_histogram <- function(data, variable, data_raw = NULL, bins = 10, show_normal = TRUE,
                           show_stats = TRUE, fill_color = "#1192E8", version = "modern") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  var_label <- .get_var_label(data_raw, variable)
  x <- data[[variable]]
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  s <- stats::sd(x)
  p <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = bins,
                            fill = fill_color, color = "black", linewidth = 0.3, alpha = 1)
  if (show_normal) {
    p <- p + ggplot2::stat_function(fun = stats::dnorm, args = list(mean = m, sd = s), color = "black", linewidth = 0.8)
  }
  p <- p + theme_SPSS(version = version, scales = "continuous") + ggplot2::labs(x = var_label, y = "Frequency")
  if (show_stats) {
    x_pos <- max(x) - (max(x) - min(x)) * 0.02
    y_max <- max(ggplot2::ggplot_build(p)$data[[1]]$density, na.rm = TRUE)
    y_pos <- y_max * 0.95
    stats_label <- paste0("Mean = ", formatC(m, digits = 2, format = "f"), "\n",
                          "Std. Dev. = ", formatC(s, digits = 3, format = "f"), "\n", "N = ", n)
    p <- p + ggplot2::annotate("label", x = x_pos, y = y_pos, label = stats_label, hjust = 1, vjust = 1,
                               size = 3, family = "sans", fill = "white", label.size = 0.3,
                               label.padding = ggplot2::unit(0.4, "lines"))
  }
  print(p)
  invisible(p)
}

#' SPSS-Style Bar Chart
#'
#' Creates a bar chart for categorical variables with SPSS styling.
#'
#' @param data A data frame.
#' @param variable Character. Variable name to plot.
#' @param data_raw Optional. Original haven-imported data for labels.
#' @param show_counts Logical. Show count labels on bars. Default TRUE.
#' @param fill_color Character. Bar fill color. Default "#1192E8".
#' @param version Character. "modern" or "legacy" SPSS style.
#'
#' @return A ggplot2 object (invisibly). Also prints the plot.
#'
#' @export
spss_barchart <- function(data, variable, data_raw = NULL, show_counts = TRUE,
                          fill_color = "#1192E8", version = "modern") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  var_label <- .get_var_label(data_raw, variable)
  val_labels <- .get_val_labels(data_raw, variable)
  x <- data[[variable]]
  x <- x[!is.na(x)]
  freq_df <- as.data.frame(table(x))
  names(freq_df) <- c("category", "count")
  if (!is.null(val_labels)) {
    freq_df$label <- sapply(as.character(freq_df$category), function(v) if (v %in% names(val_labels)) val_labels[[v]] else v)
  } else {
    freq_df$label <- as.character(freq_df$category)
  }
  p <- ggplot2::ggplot(freq_df, ggplot2::aes(x = label, y = count)) +
    ggplot2::geom_bar(stat = "identity", fill = fill_color, color = "black", linewidth = 0.3, width = 0.7)
  if (show_counts) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.5, size = 3, family = "sans")
  }
  p <- p + theme_SPSS(version = version, scale.x = "discrete", scale.y = "continuous") +
    ggplot2::labs(x = var_label, y = "Frequency") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))
  print(p)
  invisible(p)
}

#' Save SPSS-Style Plot as PNG
#'
#' Saves a ggplot2 plot as PNG.
#'
#' @param plot A ggplot2 object.
#' @param filename Character. Output filename.
#' @param width Numeric. Width in inches. Default 6.
#' @param height Numeric. Height in inches. Default 4.5.
#' @param dpi Numeric. Resolution. Default 150.
#'
#' @return The filename (invisibly).
#'
#' @export
spss_save_plot <- function(plot, filename, width = 6, height = 4.5, dpi = 150) {
  ggplot2::ggsave(filename, plot, width = width, height = height, dpi = dpi)
  invisible(filename)
}
