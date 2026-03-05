# ============================================================================
# format_results.R - Format results lists for display in worksheets
# ============================================================================

# -----------------------------------------------------------------------------
# Correlation results formatting
# -----------------------------------------------------------------------------

#' Format correlation results for fill-in-the-blank output
#'
#' Creates a markdown-formatted results section for correlation analysis,
#' with either filled answers (KEY=TRUE) or blanks for student worksheets.
#'
#' @param rh_name Name/number of research hypothesis (e.g., "RH1").
#' @param vars Character vector of two variable names.
#' @param corr_results_list A results list from [corr_answers()].
#' @param Key If TRUE, show answers; if FALSE, show blanks.
#' @param digits Number of decimal places for statistics (default 2).
#' @param p_digits Number of decimal places for p-values (default 3).
#' @return Character string of formatted results (markdown).
#' @export
#' @examples
#' \dontrun{
#' result <- corr_answers(data, "var1", "var2")
#' cat(format_corr_results("RH1", c("var1", "var2"), result, Key = TRUE))
#' }
format_corr_results <- function(rh_name, vars, corr_results_list, Key = TRUE,
                                digits = 2, p_digits = 3) {

  blank <- "______"

  # Extract values from results list
  r <- corr_results_list$Correlation$r
  p <- corr_results_list$Correlation$p_value
  n <- corr_results_list$Sample_Size
  df <- corr_results_list$Correlation$df
  desc <- corr_results_list$Descriptives

  # Get descriptives for each variable
  desc1 <- desc[desc$variable == vars[1], ]
  desc2 <- desc[desc$variable == vars[2], ]

  # Handle case where variable names don't match exactly
  if (nrow(desc1) == 0) desc1 <- desc[1, ]
  if (nrow(desc2) == 0) desc2 <- desc[2, ]

  # Format values based on Key
  if (Key) {
    r_fmt <- format_r(r, digits = digits)
    p_fmt <- format_p_value(p, digits = p_digits, include_p = FALSE)
    n_fmt <- as.character(n)
    df_fmt <- format_df(df)
    m1_fmt <- format_mean(desc1$mean, digits = digits)
    sd1_fmt <- format_sd(desc1$sd, digits = digits)
    n1_fmt <- as.character(desc1$n)
    m2_fmt <- format_mean(desc2$mean, digits = digits)
    sd2_fmt <- format_sd(desc2$sd, digits = digits)
    n2_fmt <- as.character(desc2$n)
  } else {
    r_fmt <- p_fmt <- n_fmt <- df_fmt <- blank
    m1_fmt <- sd1_fmt <- n1_fmt <- blank
    m2_fmt <- sd2_fmt <- n2_fmt <- blank
  }

  # Build output using glue
  glue::glue("
**{rh_name} Results**

**Descriptive Statistics:**

| Variable | *N* | *M* | *SD* |
|:---------|:---:|:---:|:----:|
| {vars[1]} | {n1_fmt} | {m1_fmt} | {sd1_fmt} |
| {vars[2]} | {n2_fmt} | {m2_fmt} | {sd2_fmt} |

**Correlations:**

| | *r* | *df* | *p* |
|:--|:---:|:---:|:---:|
| {vars[1]} & {vars[2]} | {r_fmt} | {df_fmt} | {p_fmt} |

*N* = {n_fmt} complete pairs
")
}

# -----------------------------------------------------------------------------
# Chi-square results formatting
# -----------------------------------------------------------------------------

#' Format chi-square results for fill-in-the-blank output
#'
#' Creates a markdown-formatted results section for chi-square analysis.
#'
#' @param rh_name Name/number of research hypothesis.
#' @param vars Character vector of two variable names.
#' @param chi_results_list A results list from [chi_square_answers()].
#' @param var1_labels Labels for var1 levels.
#' @param var2_labels Labels for var2 levels.
#' @param KEY If TRUE, show answers; if FALSE, show blanks.
#' @param digits Number of decimal places (default 2).
#' @return Character string of formatted results.
#' @export
format_chi2_results <- function(rh_name,
                                vars,
                                chi_results_list,
                                var1_labels,
                                var2_labels,
                                KEY = TRUE,
                                digits = 2) {

  blank <- "______"

  # Handle different naming conventions in results
  if (!is.null(chi_results_list$ChiSquare)) {
    chi2 <- chi_results_list$ChiSquare$chi_sq
    df <- chi_results_list$ChiSquare$df
    p <- chi_results_list$ChiSquare$p_value
    ct <- chi_results_list$ContingencyTable
    n <- sum(ct)
  } else if (!is.null(chi_results_list$Chi_Square)) {
    chi2 <- chi_results_list$Chi_Square$chi_square
    df <- chi_results_list$Chi_Square$df
    p <- chi_results_list$Chi_Square$p_value
    ct <- chi_results_list$Crosstab
    n <- chi_results_list$Sample_Size
  } else {
    stop("Unrecognized chi-square results structure")
  }

  if (KEY) {
    chi2_fmt <- format_chi2(chi2, digits = digits)
    df_fmt <- format_df(df)
    p_fmt <- format_p_value(p, include_p = FALSE)
    n_fmt <- as.character(n)

    # Format crosstab cells
    ct_formatted <- apply(ct, c(1, 2), as.character)
  } else {
    chi2_fmt <- df_fmt <- p_fmt <- n_fmt <- blank
    ct_formatted <- matrix(blank, nrow = nrow(ct), ncol = ncol(ct))
  }

  # Build crosstab markdown table
  header <- paste0("| | ", paste(var2_labels, collapse = " | "), " |")
  sep <- paste0("|:--|", paste(rep(":---:", length(var2_labels)), collapse = "|"), "|")

  rows <- vapply(seq_len(nrow(ct)), function(i) {
    paste0("| ", var1_labels[i], " | ", paste(ct_formatted[i, ], collapse = " | "), " |")
  }, character(1))

  crosstab_md <- paste(c(header, sep, rows), collapse = "\n")

  glue::glue("
**{rh_name} Results**

**Crosstabulation:**

{crosstab_md}

**Chi-Square Test:**

| {.chi_sq_symbol()} | *df* | *p* | *N* |
|:---:|:---:|:---:|:---:|
| {chi2_fmt} | {df_fmt} | {p_fmt} | {n_fmt} |
")
}

# -----------------------------------------------------------------------------
# ANOVA results formatting
# -----------------------------------------------------------------------------

#' Format between-groups ANOVA results for fill-in-the-blank output
#'
#' @param rh_name Name/number of research hypothesis.
#' @param vars Character vector: c(iv_name, dv_name).
#' @param anova_results_list A results list from [bg_anova_answers()].
#' @param iv_labels Labels for IV levels.
#' @param KEY If TRUE, show answers; if FALSE, show blanks.
#' @param digits Number of decimal places (default 2).
#' @return Character string of formatted results.
#' @export
format_bg_anova_results <- function(rh_name,
                                    vars,
                                    anova_results_list,
                                    iv_labels = NULL,
                                    KEY = TRUE,
                                    digits = 2) {

  blank <- "______"

  # Extract ANOVA values
  f_val <- anova_results_list$ANOVA$F
  p_val <- anova_results_list$ANOVA$p_value
  df_between <- anova_results_list$ANOVA$df_between
  df_within <- anova_results_list$ANOVA$df_within
  mse <- anova_results_list$ANOVA$mse

  # Extract descriptives
  desc <- anova_results_list$Descriptives
  if (is.null(iv_labels)) {
    iv_labels <- as.character(desc$group_label)
  }

  if (KEY) {
    f_fmt <- format_F(f_val, digits = digits)
    p_fmt <- format_p_value(p_val, include_p = FALSE)
    df_b_fmt <- format_df(df_between)
    df_w_fmt <- format_df(df_within)
    mse_fmt <- format_mse(mse, digits = digits)

    # Format group descriptives
    desc_rows <- vapply(seq_len(nrow(desc)), function(i) {
      paste0("| ", iv_labels[i], " | ",
             desc$n[i], " | ",
             format_mean(desc$mean[i], digits), " | ",
             format_sd(desc$sd[i], digits), " |")
    }, character(1))
  } else {
    f_fmt <- p_fmt <- df_b_fmt <- df_w_fmt <- mse_fmt <- blank
    desc_rows <- vapply(seq_along(iv_labels), function(i) {
      paste0("| ", iv_labels[i], " | ", blank, " | ", blank, " | ", blank, " |")
    }, character(1))
  }

  desc_table <- paste(desc_rows, collapse = "\n")

  glue::glue("
**{rh_name} Results**

**Descriptive Statistics:**

| {vars[1]} | *n* | *M* | *SD* |
|:----------|:---:|:---:|:----:|
{desc_table}

**ANOVA:**

| *F* | *df* | *MSE* | *p* |
|:---:|:----:|:-----:|:---:|
| {f_fmt} | {df_b_fmt}, {df_w_fmt} | {mse_fmt} | {p_fmt} |
")
}

#' Format within-groups ANOVA results for fill-in-the-blank output
#'
#' @param rh_name Name/number of research hypothesis.
#' @param dv_name Display name for the DV/measure.
#' @param anova_results_list A results list from [wg_anova_answers()].
#' @param condition_labels Labels for conditions/time points.
#' @param KEY If TRUE, show answers; if FALSE, show blanks.
#' @param digits Number of decimal places (default 2).
#' @return Character string of formatted results.
#' @export
format_wg_anova_results <- function(rh_name,
                                    dv_name,
                                    anova_results_list,
                                    condition_labels = NULL,
                                    KEY = TRUE,
                                    digits = 2) {
  blank <- "______"

  f_val     <- anova_results_list$ANOVA$F
  p_val     <- anova_results_list$ANOVA$p_value
  df_effect <- anova_results_list$ANOVA$df_effect
  df_error  <- anova_results_list$ANOVA$df_error
  mse       <- anova_results_list$ANOVA$mse

  desc <- anova_results_list$Descriptives
  if (is.null(condition_labels)) {
    condition_labels <- as.character(desc$condition)
  }

  if (KEY) {
    f_fmt      <- format_F(f_val, digits = digits)
    p_fmt      <- format_p_value(p_val, include_p = FALSE)
    df_eff_fmt <- format_df(df_effect)
    df_err_fmt <- format_df(df_error)
    mse_fmt    <- format_mse(mse, digits = digits)
    desc_rows  <- vapply(seq_len(nrow(desc)), function(i) {
      paste0("| ", condition_labels[i],
             " | ", format_mean(desc$mean[i], digits),
             " | ", format_sd(desc$sd[i], digits),
             " | ", desc$n[i], " |")
    }, character(1))
  } else {
    f_fmt <- p_fmt <- df_eff_fmt <- df_err_fmt <- mse_fmt <- blank
    desc_rows <- vapply(seq_along(condition_labels), function(i) {
      paste0("| ", condition_labels[i],
             " | ", blank, " | ", blank, " | ", blank, " |")
    }, character(1))
  }  # <-- this closing brace was missing

  desc_table <- paste(desc_rows, collapse = "\n")

  glue::glue("
**{rh_name} Results**

**Descriptive Statistics:**

| Condition | *M* | *SD* | *n* |
|:----------|:---:|:----:|:---:|
{desc_table}

**Repeated Measures ANOVA:**

| *F* | *df* | *MSE* | *p* |
|:---:|:----:|:-----:|:---:|
| {f_fmt} | {df_eff_fmt}, {df_err_fmt} | {mse_fmt} | {p_fmt} |
")
}
