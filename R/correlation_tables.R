# =============================================================================
# correlation_tables.R
# Correlation Tables for Word/PDF Output (Flextable)
# =============================================================================
# Updated for dplyr 1.2.0+ with modern tidyverse patterns
# =============================================================================

# -----------------------------------------------------------------------------
# Internal helper: Format correlation cell with significance stars
# -----------------------------------------------------------------------------

#' Format correlation cell with significance stars (internal)
#' @param r Numeric correlation coefficient
#' @param p Numeric p-value
#' @return Character string with formatted correlation and stars
#' @noRd
.format_cor_cell <- function(r, p) {
  if (is.na(r)) return("\u2014")

  stars <- dplyr::case_when(
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    .default = ""
  )

  paste0(sprintf("%.2f", r), stars)
}


# =============================================================================
# CORRELATION RESULTS TABLE
# =============================================================================

#' Correlation Results Summary Table
#'
#' Creates a flextable showing correlation results alongside descriptive
#' statistics in a compact format suitable for quick review.
#'
#' @param rh_name Character string. Name/label for the research hypothesis.
#' @param vars Character vector of length 2. Variable names.
#' @param corr_results_list Output from [corr_answers()].
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman, package = "psych350data")
#' result <- corr_answers(superman, "clark_height_in", "rt_critics_score")
#' ft <- create_corr_table("RH1", c("clark_height_in", "rt_critics_score"), result)
#' ft
#'
#' @export
create_corr_table <- function(rh_name, vars, corr_results_list) {

  desc_stats <- corr_results_list$Descriptives
  var1_stats <- desc_stats[desc_stats$variable == vars[1], ]
  var2_stats <- desc_stats[desc_stats$variable == vars[2], ]

  p_value <- corr_results_list$Correlation$p_value

  decision <- dplyr::if_else(
    p_value < 0.05,
    "Reject the H0 null hypothesis",
    "Retain the H0 null hypothesis"
  )

  combined_data <- tibble::tibble(
    ` ` = c(
      paste("Correlation:", rh_name),
      paste("Variable 1:", vars[1]),
      paste("Variable 2:", vars[2])
    ),
    Column2 = c(
      format_r(corr_results_list$Correlation$r),
      format_mean(var1_stats$mean),
      format_mean(var2_stats$mean)
    ),
    Column3 = c(
      format_p_value(p_value),
      format_sd(var1_stats$sd),
      format_sd(var2_stats$sd)
    ),
    Column4 = c(
      format_df(corr_results_list$Correlation$df),
      format_n(var1_stats$n),
      format_n(var2_stats$n)
    ),
    Column5 = c(decision, "", "")
  )

  ft <- flextable::flextable(combined_data) |>
    flextable::set_header_labels(
      ` ` = " ",
      Column2 = "r / Mean",
      Column3 = "p / SD",
      Column4 = "df / N",
      Column5 = "Reject or Retain?"
    ) |>
    flextable::theme_box() |>
    flextable::autofit()

  return(ft)
}




# =============================================================================
# APA CORRELATION TABLE (BLANK OR FILLED)
# =============================================================================

#' APA Correlation Table (Blank or Filled)
#'
#' Creates a correlation matrix table that can be either filled with computed
#' values (answer KEY) or blank (student worksheet). Supports a `KEY` toggle
#' and can produce empty templates.
#'
#' @param data A data frame or `NULL` (for blank table when `KEY = FALSE`).
#' @param vars Character vector or `NULL`. Variable names to include.
#' @param var_labels Character vector or `NULL`. Display labels for variables.
#'   If `NULL`, labels are auto-generated.
#' @param KEY Logical. If `TRUE` and data/vars are provided, compute and fill
#'   values. If `FALSE`, create blank template.
#' @param table_title Character. Table caption text.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman, package = "psych350data")
#'
#' # Filled answer KEY table
#' ft_KEY <- create_apa_corr_table(
#'   data = superman,
#'   vars = c("clark_height_in", "rt_critics_score", "rt_audience_score"),
#'   var_labels = c("1. Clark Height", "2. Critics Score", "3. Audience Score"),
#'   KEY = TRUE
#' )
#' ft_KEY
#'
#' # Blank student worksheet table
#' ft_blank <- create_apa_corr_table(
#'   var_labels = c("1. Clark Height", "2. Critics Score", "3. Audience Score"),
#'   KEY = FALSE
#' )
#' ft_blank
#'
#' @export
create_apa_corr_table <- function(data = NULL, vars = NULL, var_labels = NULL,
                                  KEY = TRUE,
                                  table_title = "Means, Standard Deviations, and Correlations") {

  # Determine number of variables
  n_vars <- if (!is.null(vars)) {
    length(vars)
  } else if (!is.null(var_labels)) {
    length(var_labels)
  } else {
    3L # Default
  }

  # Create variable labels if not provided
  if (is.null(var_labels)) {
    var_labels <- if (!is.null(vars)) {
      paste0(seq_len(n_vars), ". ", vars)
    } else {
      paste0(seq_len(n_vars), ". Variable ", seq_len(n_vars))
    }
  }

  if (KEY && !is.null(data) && !is.null(vars)) {
    # =============================================
    # FILLED TABLE - Calculate correlations and descriptives
    # =============================================

    # Extract and convert to numeric, replace -99 with NA
    cor_data <- data |>
      dplyr::select(dplyr::all_of(vars)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(x) dplyr::na_if(x, -99)))

    # Calculate correlation matrix using psych
    cor_matrix <- psych::corr.test(cor_data, use = "pairwise.complete.obs")
    r_vals <- cor_matrix$r
    p_vals <- cor_matrix$p

    # Calculate descriptives for each variable
    desc_stats <- cor_data |>
      dplyr::summarise(dplyr::across(dplyr::everything(), list(
        mean = \(x) mean(x, na.rm = TRUE),
        sd   = \(x) stats::sd(x, na.rm = TRUE),
        n    = \(x) sum(!is.na(x))
      )))

    # Extract means, SDs, and ns
    means <- desc_stats |>
      dplyr::select(dplyr::ends_with("_mean")) |>
      unlist() |>
      purrr::map_chr(format_mean)

    sds <- desc_stats |>
      dplyr::select(dplyr::ends_with("_sd")) |>
      unlist() |>
      purrr::map_chr(format_sd)

    ns <- desc_stats |>
      dplyr::select(dplyr::ends_with("_n")) |>
      unlist()

    # Build correlation columns (upper triangle, shown below diagonal)
    cor_cols <- list()
    for (i in seq_len(n_vars - 1)) {
      cor_col <- purrr::map_chr(seq_len(n_vars), \(j) {
        if (j > i) {
          .format_cor_cell(r_vals[i, j], p_vals[i, j])
        } else {
          "\u2014"
        }
      })
      cor_cols[[paste0(i)]] <- cor_col
    }

    # Create the data frame
    table_data <- data.frame(
      Variable = var_labels,
      M = unname(means),
      SD = unname(sds),
      n = as.character(ns),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Add correlation columns
    for (i in seq_len(n_vars - 1)) {
      table_data[[as.character(i)]] <- cor_cols[[i]]
    }

  } else {
    # =============================================
    # BLANK TABLE - Empty template for student worksheets
    # =============================================

    # Create empty correlation columns with dashes on/below diagonal
    cor_cols <- list()
    for (i in seq_len(n_vars - 1)) {
      cor_col <- purrr::map_chr(seq_len(n_vars), \(j) {
        if (j <= i) "\u2014" else ""
      })
      cor_cols[[paste0(i)]] <- cor_col
    }

    # Create the data frame with empty cells
    table_data <- data.frame(
      Variable = var_labels,
      M = rep("", n_vars),
      SD = rep("", n_vars),
      n = rep("", n_vars),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Add empty correlation columns
    for (i in seq_len(n_vars - 1)) {
      table_data[[as.character(i)]] <- cor_cols[[i]]
    }
  }

  # =============================================
  # BUILD THE FLEXTABLE
  # =============================================

  apa_table <- table_data |>
    flextable::flextable() |>
    flextable::set_caption(caption = table_title) |>
    flextable::set_table_properties(layout = "autofit", align = "left", width = 0.8) |>
    flextable::set_header_labels(
      Variable = "Variable",
      M = "M",
      SD = "SD",
      n = "n"
    ) |>
    flextable::add_header_row(
      values = c("", "Descriptive Statistics", "Correlations"),
      colwidths = c(1, 3, n_vars - 1)
    ) |>
    flextable::border_remove() |>
    flextable::hline_top(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline(i = 1, part = "header", border = officer::fp_border(width = 1)) |>
    flextable::hline_bottom(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "body", border = officer::fp_border(width = 2)) |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = "Variable", align = "left", part = "all") |>
    flextable::fontsize(size = 11, part = "all") |>
    flextable::add_footer_lines("Note. *p < .05. **p < .01. ***p < .001.") |>
    flextable::fontsize(part = "footer", size = 10) |>
    flextable::align(part = "footer", align = "left")

  return(apa_table)
}
