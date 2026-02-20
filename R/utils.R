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
  if (is.na(p)) {
    return(NA_character_)
  } else if (p < 0.001) {
    return("< .001")
  } else {
    formatted <- sprintf(paste0("%.", digits, "f"), p)
    formatted <- sub("^0\\.", ".", formatted)
    return(formatted)
  }
}
