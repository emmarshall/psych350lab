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
#' data(superman)
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
      as.character(corr_results_list$Correlation$r),
      as.character(var1_stats$mean),
      as.character(var2_stats$mean)
    ),
    Column3 = c(
      as.character(p_value),
      as.character(var1_stats$sd),
      as.character(var2_stats$sd)
    ),
    Column4 = c(
      as.character(corr_results_list$Correlation$df),
      as.character(var1_stats$n),
      as.character(var2_stats$n)
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
# FORMAT CORRELATION RESULTS (TEXT)
# =============================================================================

#' Format Correlation Results for Fill-in-the-Blank
#'
#' Returns a formatted text string of correlation results, either filled
#' in (answer key) or blank (student worksheet).
#'
#' @param rh_name Character. Research hypothesis name.
#' @param vars Character vector of length 2. Variable names.
#' @param corr_results_list Output from [corr_answers()].
#' @param Key Logical. If `TRUE` (default), show answers; if `FALSE`, show blanks.
#'
#' @return A character string.
#'
#' @examples
#' data(superman)
#' result <- corr_answers(superman, "clark_height_in", "rt_critics_score")
#'
#' # Answer key version
#' cat(format_corr_results("RH1", c("clark_height_in", "rt_critics_score"), result))
#'
#' # Blank student version
#' cat(format_corr_results("RH1", c("clark_height_in", "rt_critics_score"),
#'   result, Key = FALSE))
#'
#' @export
format_corr_results <- function(rh_name, vars, corr_results_list, Key = TRUE) {

  desc_stats <- corr_results_list$Descriptives
  var1_stats <- desc_stats[desc_stats$variable == vars[1], ]
  var2_stats <- desc_stats[desc_stats$variable == vars[2], ]

  r <- corr_results_list$Correlation$r
  p_value <- corr_results_list$Correlation$p_value
  df <- corr_results_list$Correlation$df

  h0_decision <- dplyr::if_else(p_value < 0.05, "Reject H0", "Retain H0")
  rh_support <- dplyr::if_else(p_value < 0.05, "Yes", "No")

  if (Key) {
    output <- paste0(
      "For the ", vars[1], ":\n",
      "  Mean = ", var1_stats$mean, "\n",
      "  SD = ", var1_stats$sd, "\n",
      "  N = ", var1_stats$n, "\n\n",
      "For the ", vars[2], ":\n",
      "  Mean = ", var2_stats$mean, "\n",
      "  SD = ", var2_stats$sd, "\n",
      "  N = ", var2_stats$n, "\n\n",
      "Correlation results:\n",
      "  r = ", r, "\n",
      "  df = ", df, "\n",
      "  p = ", p_value, "\n\n",
      "State the H0: There is no correlation between ", vars[1], " and ", vars[2], "\n\n",
      "Retain or reject H0? ", h0_decision, "\n\n",
      "Support research hypothesis? ", rh_support, "\n\n"
    )
  } else {
    output <- paste0(
      "For the ", vars[1], ":\n",
      "  Mean = ____\n",
      "  SD = ____\n",
      "  N = ____\n\n",
      "For the ", vars[2], ":\n",
      "  Mean = ____\n",
      "  SD = ____\n",
      "  N = ____\n\n",
      "Correlation results:\n",
      "  r = ____\n",
      "  df = ____\n",
      "  p = ____\n\n",
      "State the H0: _______\n\n",
      "Retain or reject H0? ______\n\n",
      "Support research hypothesis? ______"
    )
  }

  return(output)
}


# =============================================================================
# APA CORRELATION MATRIX TABLE
# =============================================================================

#' APA Correlation Matrix Table (from raw data)
#'
#' Creates an APA-formatted correlation matrix table with means, SDs,
#' sample sizes, and significance stars using [psych::corr.test()].
#' This function always computes correlations from the raw data.
#'
#' @param data A data frame.
#' @param vars Character vector. Variable names to include in the matrix.
#' @param var_labels Character vector or `NULL`. Display labels for variables.
#'   If `NULL`, labels are auto-generated as numbered variable names.
#' @param table_title Character. Table caption text.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman)
#' sm <- superman[, c("clark_height_in", "rt_critics_score", "rt_audience_score")]
#' sm <- sm[stats::complete.cases(sm), ]
#' ft <- create_apa_corr_table(sm,
#'   vars = c("clark_height_in", "rt_critics_score", "rt_audience_score"),
#'   var_labels = c("1. Clark Height", "2. Critics Score", "3. Audience Score")
#' )
#' ft
#'
#' @export
create_apa_corr_table <- function(data, vars, var_labels = NULL,
                                  table_title = "Means, Standard Deviations, and Correlations") {

  # Select only specified variables
  cor_vars <- data |> dplyr::select(dplyr::all_of(vars))

  # If no labels provided, use variable names with numbers
  if (is.null(var_labels)) {
    var_labels <- paste0(seq_along(vars), ". ", vars)
  }

  # Calculate correlation matrix (r and p values)
  cor_matrix <- psych::corr.test(cor_vars, use = "pairwise.complete.obs")
  r_vals <- cor_matrix$r
  p_vals <- cor_matrix$p

  # Calculate descriptive statistics
  desc_stats <- psych::describe(cor_vars)

  # Build the data frame
  table_df <- data.frame(
    Variable = var_labels,
    M = round(desc_stats$mean, 2),
    SD = round(desc_stats$sd, 2),
    n = desc_stats$n
  )

  # Add correlation columns (lower triangle only, with dash on diagonal and above)
  for (i in seq_along(var_labels)) {
    new_col <- purrr::map_chr(seq_along(var_labels), \(j) {
      if (j >= i) {
        "\u2014"
      } else {
        .format_cor_cell(r_vals[j, i], p_vals[j, i])
      }
    })
    table_df[[paste0(i)]] <- new_col
  }

  # Create the flextable
  apa_table <- table_df |>
    flextable::flextable() |>
    flextable::set_caption(caption = paste("Table 1:", table_title)) |>
    flextable::set_table_properties(layout = "autofit") |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::set_header_labels(
      Variable = "Variable",
      M = "M",
      SD = "SD",
      n = "n"
    ) |>
    flextable::add_header_row(
      values = c("", "Descriptive Statistics", "Correlations"),
      colwidths = c(1, 3, length(var_labels))
    ) |>
    flextable::border_remove() |>
    flextable::hline_top(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "body", border = officer::fp_border(width = 2)) |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = "Variable", align = "left", part = "all") |>
    flextable::fontsize(size = 10.5, part = "all") |>
    flextable::add_footer_lines("Note. *p < .05. **p < .01. ***p < .001.") |>
    flextable::fontsize(part = "footer", size = 10) |>
    flextable::align(part = "footer", align = "left")

  return(apa_table)
}


# =============================================================================
# APA CORRELATION TABLE (BLANK OR FILLED)
# =============================================================================

#' APA Correlation Table (Blank or Filled)
#'
#' Creates a correlation matrix table that can be either filled with computed
#' values (answer key) or blank (student worksheet). Unlike [create_apa_corr_table()],
#' this function supports a `Key` toggle and can produce empty templates.
#'
#' @param data A data frame or `NULL` (for blank table when `Key = FALSE`).
#' @param vars Character vector or `NULL`. Variable names to include.
#' @param var_labels Character vector or `NULL`. Display labels for variables.
#'   If `NULL`, labels are auto-generated.
#' @param Key Logical. If `TRUE` and data/vars are provided, compute and fill
#'   values. If `FALSE`, create blank template.
#' @param table_title Character. Table caption text.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman)
#'
#' # Filled answer key table
#' ft_key <- create_corr_apa_table(
#'   data = superman,
#'   vars = c("clark_height_in", "rt_critics_score", "rt_audience_score"),
#'   var_labels = c("1. Clark Height", "2. Critics Score", "3. Audience Score"),
#'   Key = TRUE
#' )
#' ft_key
#'
#' # Blank student worksheet table
#' ft_blank <- create_corr_apa_table(
#'   var_labels = c("1. Clark Height", "2. Critics Score", "3. Audience Score"),
#'   Key = FALSE
#' )
#' ft_blank
#'
#' @export
create_corr_apa_table <- function(data = NULL, vars = NULL, var_labels = NULL,
                                  Key = TRUE,
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

  if (Key && !is.null(data) && !is.null(vars)) {
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
      round(2)

    sds <- desc_stats |>
      dplyr::select(dplyr::ends_with("_sd")) |>
      unlist() |>
      round(2)

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
      M = as.character(means),
      SD = as.character(sds),
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
