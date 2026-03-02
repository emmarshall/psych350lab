#' Read SPSS Data File
#'
#' Reads an SPSS `.sav` file, converts user-missing values to `NA`,
#' and removes all labels.
#'
#' @param file_path Character string. Path to the `.sav` file, relative to
#'   the project root (resolved via [here::here()]).
#' @param check_filter Logical. If `TRUE`, checks for and applies any active
#'   SPSS filter variable. Default is `TRUE`.
#' @param verbose Logical. If `TRUE`, prints diagnostic information about
#'   missing data and filtering. Default is `FALSE`.
#'
#' @return A tibble with numeric values and no SPSS labels.
#'
#' @examples
#' \dontrun{
#' data <- get_spss_data("data/superman.sav")
#' }
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
    haven::zap_missing()

  if (verbose) {
    cat("\nMissing data after zap_missing():\n")
    missing_after <- colSums(is.na(data))
    print(missing_after[missing_after > 0])
    cat("\nFinal dataset:", nrow(data), "rows\n")
    cat("=====================================\n\n")
  }

  data_clean <- data |>
    haven::zap_labels() |>
    haven::zap_label()

  return(tibble::as_tibble(data_clean))
}

#' Convert variable to factor for categorical analysis
#'
#' Internal helper that handles numeric codes, character strings, and factors
#' consistently for chi-square and other categorical analyses.
#'
#' @param x A vector (numeric, character, or factor)
#' @param remove_missing_codes Logical. If TRUE, converts -99 to NA for numeric input
#'
#' @return A factor
#' @noRd
.prepare_categorical <- function(x, remove_missing_codes = TRUE) {
  # Already a factor - return as-is
  if (is.factor(x)) {
    return(x)
  }

  # Numeric input (legacy format with codes like 1, 2, 3)
  if (is.numeric(x)) {
    if (remove_missing_codes) {
      x[x == -99] <- NA
    }
    return(as.factor(x))
  }

  # Character input (e.g., "Rotten", "Fresh")
  if (is.character(x)) {
    return(as.factor(x))
  }

  # Fallback
  as.factor(x)
}


#' Format a p-value for APA style
#'
#' Formats a p-value following APA conventions: values less than .001 are
#' displayed as "< .001", and leading zeros are removed.
#'
#' @param p Numeric. The p-value to format.
#' @param digits Integer. Number of decimal places. Default is 3.
#'
#' @return A character string with the formatted p-value.
#'
#' @examples
#' format_p_value(0.023)
#' format_p_value(0.0004)
#' format_p_value(0.150)
#'
#' @export
format_p_value <- function(p, digits = 3) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "< .001",
    .default = sub("^0\\.", ".", sprintf(paste0("%.", digits, "f"), p))
  )
}

# -----------------------------------------------------------------------------
# INTERNAL HELPERS
# -----------------------------------------------------------------------------

#' Create chi-square symbol using unicode
#' @keywords internal
#' @noRd
.chi_sq_symbol <- function() {

  paste0(intToUtf8(0x03C7), intToUtf8(0x00B2))
}

#' Create eta-squared symbol using unicode
#' @keywords internal
#' @noRd
.eta_sq_symbol <- function() {

  paste0(intToUtf8(0x03B7), intToUtf8(0x00B2))
}

#' Create en-dash using unicode
#' @keywords internal
#' @noRd
.en_dash <- function() {
  intToUtf8(0x2013)
}

#' Create R-squared symbol using unicode
#' @keywords internal
#' @noRd
.r_sq_symbol <- function() {
  paste0("R", intToUtf8(0x00B2))
}
