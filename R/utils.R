# =============================================================================
# Data Import, Formatting Helpers, and Internal Utilities
# =============================================================================

#' Read SPSS Data File
#'
#' Reads an SPSS `.sav` file, converts user-missing values to `NA`,
#' and removes all labels.
#'
#' @param file_path Character string. Path to the `.sav` file.
#' @param check_filter Logical. If `TRUE`, checks for and applies any active
#'   SPSS filter variable. Default is `TRUE`.
#' @param verbose Logical. If `TRUE`, prints diagnostic information.
#'
#' @return A tibble with numeric values and no SPSS labels.
#'
#' @export
get_spss_data <- function(file_path, check_filter = TRUE, verbose = FALSE) {
  data <- haven::read_sav(here::here(file_path))

  if (verbose) {
    cat("\n=== SPSS DATA IMPORT DIAGNOSTICS ===\n")
    cat("Total rows in raw SPSS file:", nrow(data), "\n")
  }

  if (check_filter) {
    filter_var <- attr(data, "filter")
    if (!is.null(filter_var)) {
      if (verbose) {
        warning("SPSS filter detected on variable: ", filter_var)
        warning("Applying filter to match SPSS...")
      }
      data <- data[data[[filter_var]] != 0 & !is.na(data[[filter_var]]), ]
    }
  }

  data <- data |>
    haven::zap_missing() |>
    haven::zap_labels() |>
    haven::zap_label() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(haven::is.labelled),   # catch any remaining haven_labelled cols
        haven::as_factor             # convert to plain factor
      )
    )

  if (verbose) {
    cat("\nMissing data after zap_missing():\n")
    missing_after <- colSums(is.na(data))
    print(missing_after[missing_after > 0])
    cat("\nFinal dataset:", nrow(data), "rows\n")
    cat("=====================================\n\n")
  }

  data
}

# =============================================================================
# CATEGORICAL DATA HELPERS
# =============================================================================

#' Convert variable to factor for categorical analysis
#'
#' @param x A vector (numeric, character, or factor)
#' @param remove_missing_codes Logical. If TRUE, converts -99 to NA
#'
#' @return A factor
#' @noRd
.prepare_categorical <- function(x, remove_missing_codes = TRUE) {
  if (is.factor(x)) return(x)
  if (is.numeric(x)) {
    if (remove_missing_codes) x[x == -99] <- NA
    return(as.factor(x))
  }
  if (is.character(x)) return(as.factor(x))
  as.factor(x)
}


# =============================================================================
# UNICODE SYMBOL HELPERS
# =============================================================================

#' Create chi-square symbol
#' @keywords internal
#' @noRd
.chi_sq_symbol <- function() {
  paste0(intToUtf8(0x03C7), intToUtf8(0x00B2))
}

#' Create eta-squared symbol
#' @keywords internal
#' @noRd
.eta_sq_symbol <- function() {
  paste0(intToUtf8(0x03B7), intToUtf8(0x00B2))
}

#' Create en-dash
#' @keywords internal
#' @noRd
.en_dash <- function() {
  intToUtf8(0x2013)
}

#' Create R-squared symbol
#' @keywords internal
#' @noRd
.r_sq_symbol <- function() {
  paste0("R", intToUtf8(0x00B2))
}

#' Create beta symbol
#' @keywords internal
#' @noRd
.beta_symbol <- function() {

  intToUtf8(0x03B2)
}

#' Create em-dash
#' @keywords internal
#' @noRd
.em_dash <- function() {
  intToUtf8(0x2014)
}


# =============================================================================
# FORMATTING HELPERS - EXPORTED
# =============================================================================
# These are the primary functions for formatting in tables and displays.
# All *_answers() functions store raw values; formatting happens here.
# =============================================================================

#' Format a p-value for APA style
#'
#' Formats a p-value following APA conventions: values less than .001 are
#' displayed as "< .001", and leading zeros are removed.
#'
#' @param p Numeric. The p-value to format.
#' @param digits Integer. Number of decimal places. Default is 3.
#' @param include_p Logical. If TRUE, prefix with "p = " or "p < ". Default FALSE.
#'
#' @return A character string with the formatted p-value.
#'
#' @examples
#' format_p_value(0.023)
#' format_p_value(0.0004)
#' format_p_value(0.150, include_p = TRUE)
#'
#' @export
format_p_value <- function(p, digits = 3, include_p = FALSE) {
  result <- dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "< .001",
    .default = sub("^0\\.", ".", sprintf(paste0("%.", digits, "f"), p))
  )

  if (include_p && !is.na(result)) {
    prefix <- if (grepl("^<", result)) "p " else "p = "
    result <- paste0(prefix, result)
  }

  result
}


#' Format a statistic for display
#'
#' Formats numeric statistics with appropriate decimal places and leading zeros.
#' Use for means, SDs, F, t, chi-square, MSE, b coefficients, etc.
#'
#' @param x Numeric value to format.
#' @param digits Integer. Number of decimal places. Default is 2.
#' @param remove_leading_zero Logical. If TRUE, removes leading zero for
#'   values between -1 and 1. Use for r, R², effect sizes. Default is FALSE.
#'
#' @return A character string.
#'
#' @examples
#' format_stat(3.456)
#' format_stat(0.234, remove_leading_zero = TRUE)
#' format_stat(-0.567, remove_leading_zero = TRUE)
#'
#' @export
format_stat <- function(x, digits = 2, remove_leading_zero = FALSE) {
  if (is.na(x) || is.null(x)) return("NA")

  formatted <- sprintf(paste0("%.", digits, "f"), x)

  if (remove_leading_zero && abs(x) < 1) {
    formatted <- sub("^0", "", formatted)
    formatted <- sub("^-0", "-", formatted)
  }

  formatted
}


#' Format an integer for display
#'
#' Formats numeric values as integers (no decimal places).
#' Use for n, N, df, k, and other count variables.
#'
#' @param x Numeric value to format as integer.
#'
#' @return A character string.
#'
#' @examples
#' format_int(25)
#' format_int(25.4)
#'
#' @export
format_int <- function(x) {
  if (is.na(x) || is.null(x)) return("NA")
  as.character(as.integer(round(x)))
}


#' Convert p-value to significance stars
#'
#' @param p Numeric p-value.
#'
#' @return Character: "***" (p < .001), "**" (p < .01), "*" (p < .05), or "ns".
#'
#' @examples
#' p_to_stars(0.0001)
#' p_to_stars(0.03)
#' p_to_stars(0.15)
#'
#' @export
p_to_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    .default = "ns"
  )
}


# =============================================================================
# FORMATTING HELPERS - INTERNAL
# =============================================================================
# These are convenience wrappers for internal use in checker functions.
# They provide a consistent interface for the .safe_format() function.
# =============================================================================

#' Format a numeric value for APA style (internal)
#' @param x Numeric value.
#' @param digits Number of decimal places.
#' @keywords internal
#' @noRd
.format_apa <- function(x, digits = 2) {
  if (is.na(x) || is.null(x)) return("NA")
  formatted <- sprintf(paste0("%.", digits, "f"), x)
  if (abs(x) < 1) {
    formatted <- sub("^0", "", formatted)
    formatted <- sub("^-0", "-", formatted)
  }
  formatted
}


#' Format a p-value for APA style (internal)
#' @param p Numeric p-value.
#' @keywords internal
#' @noRd
.format_p_apa <- function(p) {
  if (is.na(p) || is.null(p)) return("NA
")
  if (p < 0.001) return(".001")
  .format_apa(p, digits = 3)
}


#' Format statistic for display (internal)
#' @param x Numeric value.
#' @param bounded Logical. TRUE for r, R², effect sizes.
#' @keywords internal
#' @noRd
.format_stat <- function(x, bounded = FALSE) {
  if (is.null(x) || length(x) == 0) return("NA")
  if (length(x) > 1) x <- x[[1]]
  if (is.na(x)) return("NA")
  formatted <- sprintf("%.2f", x)
  if (bounded && abs(x) < 1) {
    formatted <- sub("^0", "", formatted)
    formatted <- sub("^-0", "-", formatted)
  }
  formatted
}


#' Format integer for display (internal)
#' @param x Numeric value.
#' @keywords internal
#' @noRd
.format_int <- function(x) {
  if (is.na(x) || is.null(x)) return("NA")
  as.character(as.integer(round(x)))
}


#' Safely extract and format scalar value
#'
#' @param value Value to format (may be vector, list element, or scalar).
#' @param type One of "stat", "bounded", "p", "int".
#' @param default Default value if input is NA or empty.
#' @keywords internal
#' @noRd
.safe_format <- function(value, type = "stat", default = "NA") {
  if (is.null(value) || length(value) == 0) return(default)
  val <- value[[1]]
  if (is.na(val)) return(default)

  switch(type,
         "stat"    = .format_stat(val, bounded = FALSE),
         "bounded" = .format_stat(val, bounded = TRUE),
         "p"       = .format_p_apa(val),
         "int"     = .format_int(val),
         as.character(val)
  )
}


# =============================================================================
# DECISION HELPERS - INTERNAL
# =============================================================================

#' Determine H0 decision based on p-value
#' @param p_value Numeric p-value.
#' @param alpha Significance level. Default 0.05.
#' @keywords internal
#' @noRd
.get_h0_decision <- function(p_value, alpha = 0.05) {
  if (is.na(p_value)) return("Cannot determine")
  if (p_value < alpha) "Reject H0" else "Retain H0"
}


#' Determine research hypothesis support
#' @param p_value Numeric p-value.
#' @param alpha Significance level. Default 0.05.
#' @keywords internal
#' @noRd
.get_rh_support <- function(p_value, alpha = 0.05) {
  if (is.na(p_value)) return("Cannot determine")
  if (p_value < alpha) "Yes" else "No"
}

#' Format correlation coefficient (r)
#'
#' Convenience wrapper for formatting correlations with leading zero removed.
#'
#' @param r Numeric correlation value.
#' @param digits Number of decimal places. Default is 2.
#'
#' @return Character string with leading zero removed.
#'
#' @examples
#' format_r(0.456)
#' format_r(-0.234)
#'
#' @export
format_r <- function(r, digits = 2) {
  format_stat(r, digits = digits, remove_leading_zero = TRUE)
}


#' Format effect size
#'
#' Convenience wrapper for formatting bounded effect sizes (r, d, eta-squared, R²)
#' with leading zero removed.
#'
#' @param es Numeric effect size value.
#' @param digits Number of decimal places. Default is 2.
#'
#' @return Character string with leading zero removed for values between -1 and 1.
#'
#' @examples
#' format_effect(0.35)
#' format_effect(-0.12)
#'
#' @export
format_effect <- function(es, digits = 2) {
  format_stat(es, digits = digits, remove_leading_zero = TRUE)
}


#' Format mean or standard deviation
#'
#' @param x Numeric value (mean or SD).
#' @param digits Number of decimal places. Default is 2.
#'
#' @return Character string.
#'
#' @examples
#' format_mean(3.456)
#' format_sd(1.234)
#'
#' @export
format_mean <- function(x, digits = 2) {
  format_stat(x, digits = digits, remove_leading_zero = FALSE)
}

#' @rdname format_mean
#' @export
format_sd <- function(x, digits = 2) {
  format_stat(x, digits = digits, remove_leading_zero = FALSE)
}


#' Format F-statistic
#'
#' @param f Numeric F value.
#' @param digits Number of decimal places. Default is 2.
#'
#' @return Character string.
#'
#' @export
format_F <- function(f, digits = 2) {
  format_stat(f, digits = digits, remove_leading_zero = FALSE)
}


#' Format t-statistic
#'
#' @param t Numeric t value.
#' @param digits Number of decimal places. Default is 2.
#'
#' @return Character string.
#'
#' @export
format_t <- function(t, digits = 2) {
  format_stat(t, digits = digits, remove_leading_zero = FALSE)
}


#' Format chi-square statistic
#'
#' @param chi2 Numeric chi-square value.
#' @param digits Number of decimal places. Default is 2.
#'
#' @return Character string.
#'
#' @export
format_chi2 <- function(chi2, digits = 2) {
  format_stat(chi2, digits = digits, remove_leading_zero = FALSE)
}


#' Format MSE (Mean Square Error)
#'
#' @param mse Numeric MSE value.
#' @param digits Number of decimal places. Default is 2.
#'
#' @return Character string.
#'
#' @export
format_mse <- function(mse, digits = 2) {
  format_stat(mse, digits = digits, remove_leading_zero = FALSE)
}


#' Format degrees of freedom
#'
#' @param df Numeric degrees of freedom.
#'
#' @return Character string (integer format).
#'
#' @export
format_df <- function(df) {
  format_int(df)
}


#' Format sample size
#'
#' @param n Numeric sample size.
#'
#' @return Character string (integer format).
#'
#' @export
format_n <- function(n) {
  format_int(n)
}


# =============================================================================
# INLINE APA STATISTICS
# =============================================================================
# These extract values from results lists and format for inline text reporting.
# =============================================================================

#' Format correlation for inline APA reporting
#'
#' Extracts values from [corr_answers()] result and formats for inline text.
#'
#' @param corr_results Output from [corr_answers()].
#'
#' @return Character string like "*r*(28) = .45, *p* = .012".
#'
#' @examples
#' \dontrun{
#' result <- corr_answers(data, "var1", "var2")
#' cat(apa_inline_r(result))
#' }
#'
#' @export
apa_inline_r <- function(corr_results) {
  r <- corr_results$Correlation$r
  p <- corr_results$Correlation$p_value
  df <- corr_results$Correlation$df

  r_text <- format_r(r)
  p_text <- format_p_value(p, include_p = TRUE)

  paste0("*r*(", format_df(df), ") = ", r_text, ", ", p_text)
}


#' Format chi-square for inline APA reporting
#'
#' Extracts values from [chi_square_answers()] result and formats for inline text.
#'
#' @param chi_results Output from [chi_square_answers()].
#'
#' @return Character string like "χ²(1) = 4.52, *p* = .034".
#'
#' @export
apa_inline_chi2 <- function(chi_results) {
  # Handle both naming conventions
  if (!is.null(chi_results$ChiSquare)) {
    chi_sq <- chi_results$ChiSquare$chi_sq
    df <- chi_results$ChiSquare$df
    p <- chi_results$ChiSquare$p_value
  } else if (!is.null(chi_results$Chi_Square)) {
    chi_sq <- chi_results$Chi_Square$chi_square
    df <- chi_results$Chi_Square$df
    p <- chi_results$Chi_Square$p_value
  } else {
    stop("Unrecognized chi-square results structure")
  }

  chi2_text <- format_chi2(chi_sq)
  p_text <- format_p_value(p, include_p = TRUE)

  paste0(.chi_sq_symbol(), "(", format_df(df), ") = ", chi2_text, ", ", p_text)
}


#' Format t-test for inline APA reporting
#'
#' @param t_value t-statistic.
#' @param df Degrees of freedom.
#' @param p_value p-value.
#'
#' @return Character string like "*t*(28) = 2.34, *p* = .026".
#'
#' @export
apa_inline_t <- function(t_value, df, p_value) {
  t_text <- format_t(t_value)
  p_text <- format_p_value(p_value, include_p = TRUE)

  paste0("*t*(", format_df(df), ") = ", t_text, ", ", p_text)
}


#' Format F-test for inline APA reporting
#'
#' @param f_value F-statistic.
#' @param df_between Numerator (between) degrees of freedom.
#' @param df_within Denominator (within/error) degrees of freedom.
#' @param p_value p-value.
#' @param mse Mean square error (optional).
#'
#' @return Character string like "*F*(2, 47) = 5.67, *p* = .006".
#'
#' @export
apa_inline_F <- function(f_value, df_between, df_within, p_value, mse = NULL) {
  f_text <- format_F(f_value)
  p_text <- format_p_value(p_value, include_p = TRUE)

  result <- paste0("*F*(", format_df(df_between), ", ", format_df(df_within), ") = ", f_text)

  if (!is.null(mse)) {
    result <- paste0(result, ", *MSE* = ", format_mse(mse))
  }

  paste0(result, ", ", p_text)
}


#' Format ANOVA for inline APA reporting
#'
#' Extracts values from [bg_anova_answers()] or [wg_anova_answers()] and formats.
#'
#' @param anova_results Output from anova answer function.
#' @param include_mse Include MSE in output. Default TRUE.
#'
#' @return Character string with formatted F-test.
#'
#' @export
apa_inline_anova <- function(anova_results, include_mse = TRUE) {
  f_val <- anova_results$ANOVA$F
  p_val <- anova_results$ANOVA$p_value
  mse <- anova_results$ANOVA$mse

  # Handle different df naming conventions
  if (!is.null(anova_results$ANOVA$df_between)) {
    df1 <- anova_results$ANOVA$df_between
    df2 <- anova_results$ANOVA$df_within
  } else if (!is.null(anova_results$ANOVA$df_effect)) {
    df1 <- anova_results$ANOVA$df_effect
    df2 <- anova_results$ANOVA$df_error
  } else {
    stop("Unrecognized ANOVA results structure")
  }

  if (include_mse) {
    apa_inline_F(f_val, df1, df2, p_val, mse)
  } else {
    apa_inline_F(f_val, df1, df2, p_val)
  }
}


# =============================================================================
# KEY/BLANK WORKSHEET HELPERS
# =============================================================================

#' Return formatted value or blank based on KEY flag
#'
#' Helper for creating fill-in-the-blank worksheets vs answer keys.
#'
#' @param value The value to potentially display.
#' @param KEY If TRUE, return formatted value; if FALSE, return blank.
#' @param format_fn Formatting function to apply (e.g., format_r, format_stat).
#' @param blank Character string for blank. Default "______".
#' @param ... Additional arguments passed to format_fn.
#'
#' @return Formatted value or blank string.
#'
#' @examples
#' key_or_blank(0.45, KEY = TRUE, format_fn = format_r)
#' key_or_blank(0.45, KEY = FALSE)
#' key_or_blank(3.45, KEY = TRUE, format_fn = format_stat, digits = 3)
#'
#' @export
key_or_blank <- function(value, KEY = TRUE, format_fn = NULL, blank = "______", ...) {
  if (KEY) {
    if (!is.null(format_fn)) {
      format_fn(value, ...)
    } else {
      as.character(value)
    }
  } else {
    blank
  }
}


#' Apply highlight style for Quarto/Word output
#'
#' Wraps text in Quarto custom style span for highlighting in Word documents.
#'
#' @param text Text to highlight.
#' @param style Custom style name. Default "highlight-yellow".
#' @param apply If FALSE, return text unchanged.
#'
#' @return Character string with Quarto custom style span.
#'
#' @examples
#' highlight_text("Answer here", apply = TRUE)
#' highlight_text("Answer here", style = "highlight-blue")
#'
#' @export
highlight_text <- function(text, style = "highlight-yellow", apply = TRUE) {
  if (apply) {
    paste0("[", text, "]{custom-style=\"", style, "\"}")
  } else {
    as.character(text)
  }
}


# =============================================================================
# MARKDOWN HELPERS
# =============================================================================

#' Wrap text in markdown formatting
#'
#' @param x Character vector.
#'
#' @return Character vector with markdown markers.
#'
#' @name md_format
#' @examples
#' md_bold("important")
#' md_italic("emphasized")
#'
#' @export
md_bold <- function(x) {
  ifelse(is.na(x) | x == "", x, paste0("**", x, "**"))
}

#' @rdname md_format
#' @export
md_italic <- function(x) {
  ifelse(is.na(x) | x == "", x, paste0("*", x, "*"))
}

#' @rdname md_format
#' @export
md_super <- function(x) {
  ifelse(is.na(x) | x == "", x, paste0("^", x, "^"))
}

#' @rdname md_format
#' @export
md_sub <- function(x) {
  ifelse(is.na(x) | x == "", x, paste0("~", x, "~"))
}

#' Safely format value for fitb (fill-in-the-blank)
#' @param x Value to format
#' @keywords internal
#' @noRd
.safe_fitb_value <- function(x) {

  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) {
    return("NA")
  }
  as.character(x[[1]])
}

#' Format confidence interval
#' @param lower Lower bound
#' @param upper Upper bound
#' @param digits Number of decimal places
#' @keywords internal
#' @noRd
format_ci <- function(lower, upper, digits = 2) {
  paste0("[", format_stat(lower, digits), ", ", format_stat(upper, digits), "]")
}

#' Interpret eta-squared effect size
#' @param eta_sq Eta-squared value
#' @return Character description: "small", "medium", or "large"
#' @keywords internal
#' @noRd
interpret_eta_sq <- function(eta_sq) {
  dplyr::case_when(
    is.na(eta_sq) ~ NA_character_,
    eta_sq < 0.06 ~ "small",
    eta_sq < 0.14 ~ "medium",
    .default ~ "large"
  )
}


#' Convert p-value to significance stars (extended)
#'
#' @param p Numeric p-value.
#' @param alpha Numeric vector of significance thresholds (default c(0.05, 0.01, 0.001)).
#' @param superscript Logical. If TRUE, wrap stars in markdown superscript.
#'
#' @return Character string with stars.
#'
#' @keywords internal
#' @noRd
p2stars <- function(p, alpha = c(0.05, 0.01, 0.001), superscript = FALSE) {
  if (is.na(p)) return("")

  # Sort alpha in descending order to check from least to most significant
  alpha <- sort(alpha, decreasing = TRUE)

  n_stars <- sum(p < alpha)

  if (n_stars == 0) return("")

  stars <- paste(rep("*", n_stars), collapse = "")

  if (superscript) {
    stars <- paste0("^", stars, "^")
  }

  stars
}


# =============================================================================
# Create ALIASES so I don't have to find where the other uses are and I can keep updating
# =============================================================================

#' @keywords internal
#' @noRd
.format_p_anova <- .format_p_apa

#' @keywords internal
#' @noRd
format_p <- format_p_value

#' @keywords internal
#' @noRd
format_num <- format_stat

#' @keywords internal
#' @noRd
format_f <- format_F
# =============================================================================
# PACKAGE CHECKS
# =============================================================================

#' Check and load required packages
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


#' @import psych350data
NULL
