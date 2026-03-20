#' Format Chi-Square Results for Fill-in-the-Blank
#'
#' Returns a formatted text string of chi-square results, either filled in
#' (answer KEY) or blank (student worksheet).
#'
#' @param rh_name Character. Research hypothesis name/label.
#' @param vars Character vector of length 2. Variable names.
#' @param chi_results_list Output from [chi_square_answers()].
#' @param var1_labels Character vector of length 2. Labels for the two levels
#'   of the first variable (rows).
#' @param var2_labels Character vector of length 2. Labels for the two levels
#'   of the second variable (columns).
#' @param KEY Logical. If `TRUE` (default), show answers; if `FALSE`, show blanks.
#'
#' @return A character string with formatted results.
#'
#' @examples
#' \dontrun{
#' data(superman, package = "psych350data")
#' result <- chi_square_answers(superman, "clark_grp", "tomatometer")
#' # Answer KEY
#' cat(format_chi2_results("RH1", c("clark_grp", "tomatometer"), result,
#'   var1_labels = c("Under 6ft", "6ft+"),
#'   var2_labels = c("Rotten", "Fresh")))
#' # Blank version
#' cat(format_chi2_results("RH1", c("clark_grp", "tomatometer"), result,
#'   var1_labels = c("Under 6ft", "6ft+"),
#'   var2_labels = c("Rotten", "Fresh"), KEY = FALSE))
#' }
#'
#' @export
format_chi2_results <- function(rh_name, vars, chi_results_list,
                                var1_labels = c("1", "2"),
                                var2_labels = c("1", "2"),
                                KEY = TRUE) {

  # Extract chi-square stats
  chi_sq <- chi_results_list$ChiSquare$chi_sq
  p_value <- chi_results_list$ChiSquare$p_value
  df <- chi_results_list$ChiSquare$df

  # Get descriptives
  var1_desc <- chi_results_list$Var1_Descriptives
  var2_desc <- chi_results_list$Var2_Descriptives

  # Determine decisions based on p-value
  if (p_value < 0.05) {
    h0_decision <- "Reject H\u2080"
    rh_support <- "Yes"
  } else {
    h0_decision <- "Retain H\u2080"
    rh_support <- "No"
  }

  if (KEY) {
    output <- paste0(
      "Number of ", var1_labels[1], " in the sample: ", var1_desc$n[1], "\n",
      "Number of ", var1_labels[2], " in the sample: ", var1_desc$n[2], "\n",
      "Number of ", var2_labels[1], " in the sample: ", var2_desc$n[1], "\n",
      "Number of ", var2_labels[2], " in the sample: ", var2_desc$n[2], "\n\n",
      "\u03C7\u00B2 = ", chi_sq, "     df = ", df, "     p = ", p_value, "\n\n",
      "State the H\u2080: There is no pattern of relationship between ",
      vars[1], " and ", vars[2], "\n\n",
      "Retain or reject H\u2080? ", h0_decision, "\n\n",
      "Support research hypothesis? ", rh_support, "\n\n"
    )
  } else {
    output <- paste0(
      "Number of ", var1_labels[1], " in the sample: ____\n\n",
      "Number of ", var1_labels[2], " in the sample: ____\n\n",
      "Number of ", var2_labels[1], " in the sample: ____\n\n",
      "Number of ", var2_labels[2], " in the sample: ____\n\n",
      "\u03C7\u00B2 = ____     df = ____     p = ____\n\n",
      "State the H\u2080: There is no pattern of relationship between ",
      vars[1], " and ", vars[2], "\n\n",
      "Retain or reject H\u2080? ____\n\n",
      "Support research hypothesis? ____\n\n"
    )
  }

  return(output)
}


#' Contingency Table for Research Hypothesis
#'
#' Creates a flextable showing a contingency table with observed counts and
#' comparison operators (>, <, =). Can produce either a filled answer KEY
#' or a blank template for student worksheets.
#'
#' @param var1_name Character. Display name for the row variable.
#' @param var2_name Character. Display name for the column variable.
#' @param var1_levels Character vector. Labels for row levels.
#' @param var2_levels Character vector of length 2. Labels for column levels.
#' @param KEY Logical. If `TRUE`, show observed counts and comparison operators.
#'   If `FALSE` (default), show blank template.
#' @param chi_results Output from [chi_square_answers()]. Required when
#'   `KEY = TRUE`.
#' @param hypothesis_pattern Character vector of comparison operators
#'   (e.g., `c("=", "<")` or `c(">", "=")`). Each element corresponds to a
#'   row in the table. Required when `KEY = TRUE`.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' \dontrun{
#' data(superman, package = "psych350data")
#' result <- chi_square_answers(superman, "clark_grp", "tomatometer")
#'
#' # Filled version
#' ft <- create_rh_contingency(
#'   var1_name = "Height Group",
#'   var2_name = "Tomatometer",
#'   var1_levels = c("Under 6ft", "6ft+"),
#'   var2_levels = c("Rotten", "Fresh"),
#'   KEY = TRUE,
#'   chi_results = result,
#'   hypothesis_pattern = c("=", "<")
#' )
#' ft
#'
#' # Blank version
#' ft_blank <- create_rh_contingency(
#'   var1_name = "Height Group",
#'   var2_name = "Tomatometer",
#'   var1_levels = c("Under 6ft", "6ft+"),
#'   var2_levels = c("Rotten", "Fresh"),
#'   KEY = FALSE
#' )
#' ft_blank
#' }
#'
#' @export
create_rh_contingency <- function(var1_name = "Variable 1",
                                  var2_name = "Variable 2",
                                  var1_levels = c("Level 1", "Level 2"),
                                  var2_levels = c("Level A", "Level B"),
                                  KEY = FALSE,
                                  chi_results = NULL,
                                  hypothesis_pattern = NULL) {

  # Validate inputs when KEY is TRUE
  if (KEY) {
    if (is.null(chi_results)) {
      stop("chi_results must be provided when KEY = TRUE")
    }
    if (!"Expected" %in% names(chi_results)) {
      stop("chi_results must contain an 'Expected' element from chi_square_answers()")
    }
    if (is.null(hypothesis_pattern)) {
      stop("hypothesis_pattern must be provided when KEY = TRUE. ",
           "Should be a vector like c('=', '<') or c('>', '=')")
    }
    if (length(hypothesis_pattern) != length(var1_levels)) {
      stop("hypothesis_pattern must have the same length as var1_levels (",
           length(var1_levels), ")")
    }
  }

  if (KEY && !is.null(chi_results)) {
    # Get observed values from chi_results
    observed <- chi_results$Observed

    # Warn if dimensions don't match
    if (nrow(observed) != length(var1_levels) || ncol(observed) != length(var2_levels)) {
      warning("Dimension mismatch between levels and chi_results. Using available data.")
    }

    # Use hypothesis_pattern for operators
    operators <- hypothesis_pattern

    # Extract observed values as character strings
    level_a_vals <- character(nrow(observed))
    level_b_vals <- character(nrow(observed))

    for (i in seq_len(nrow(observed))) {
      level_a_vals[i] <- as.character(observed[i, 1])
      level_b_vals[i] <- as.character(observed[i, 2])
    }

    # Create filled data frame
    data <- data.frame(
      " " = var1_levels,
      "Level_A" = level_a_vals,
      "Operator" = operators,
      "Level_B" = level_b_vals,
      check.names = FALSE
    )
  } else {
    # Create blank template
    data <- data.frame(
      " " = var1_levels,
      "Level_A" = rep(" ", length(var1_levels)),
      "Operator" = rep("?", length(var1_levels)),
      "Level_B" = rep(" ", length(var1_levels)),
      check.names = FALSE
    )
  }

  # Set column names to match variable labels
  names(data) <- c(var1_name, var2_levels[1], "Use: >, <, =", var2_levels[2])

  # Create the flextable
  contingency_table <- data |>
    flextable::flextable() |>
    flextable::theme_box() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::autofit()

  return(contingency_table)
}


#' APA Chi-Square Crosstabs Table
#'
#' Creates an APA-formatted crosstabulation table showing observed counts,
#' row percentages, row and column totals, and chi-square test results in
#' the footer. Can produce either a filled answer KEY or a blank template.
#'
#' @param chi_results_list Output from [chi_square_answers()] or `NULL`
#'   (for blank table when `KEY = FALSE`).
#' @param var1_name Character. Display name for the row variable.
#' @param var2_name Character. Display name for the column variable (appears
#'   as a spanning header).
#' @param var1_labels Character vector of length 2. Display labels for row
#'   variable levels.
#' @param var2_labels Character vector of length 2. Display labels for column
#'   variable levels.
#' @param KEY Logical. If `TRUE` (default), fill with computed values.
#'   If `FALSE`, create a blank template.
#' @param include_percentages Logical. If `TRUE` (default), include row
#'   percentages alongside counts (e.g., "15 (62.5%)").
#' @param table_title Character or `NULL`. Optional table caption.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' \dontrun{
#' data(superman, package = "psych350data")
#' result <- chi_square_answers(superman, "clark_grp", "tomatometer")
#'
#' # Filled table with percentages
#' ft <- create_chi_crosstabs_table(
#'   chi_results_list = result,
#'   var1_name = "Height Group",
#'   var2_name = "Tomatometer",
#'   var1_labels = c("Under 6ft", "6ft+"),
#'   var2_labels = c("Rotten", "Fresh"),
#'   KEY = TRUE,
#'   include_percentages = TRUE,
#'   table_title = "Table 1. Crosstabulation of Height Group and Tomatometer Status"
#' )
#' ft
#'
#' # Filled table without percentages
#' ft_counts <- create_chi_crosstabs_table(
#'   chi_results_list = result,
#'   var1_name = "Height Group",
#'   var2_name = "Tomatometer",
#'   var1_labels = c("Under 6ft", "6ft+"),
#'   var2_labels = c("Rotten", "Fresh"),
#'   KEY = TRUE,
#'   include_percentages = FALSE
#' )
#' ft_counts
#'
#' # Blank table
#' ft_blank <- create_chi_crosstabs_table(
#'   var1_name = "Height Group",
#'   var2_name = "Tomatometer",
#'   var1_labels = c("Under 6ft", "6ft+"),
#'   var2_labels = c("Rotten", "Fresh"),
#'   KEY = FALSE
#' )
#' ft_blank
#' }
#'
#' @export
create_chi_crosstabs_table <- function(chi_results_list = NULL,
                                       var1_name = "Variable 1",
                                       var2_name = "Variable 2",
                                       var1_labels = c("Level 1", "Level 2"),
                                       var2_labels = c("Group 1", "Group 2"),
                                       KEY = TRUE,
                                       include_percentages = TRUE,
                                       table_title = NULL) {

  if (KEY && !is.null(chi_results_list)) {
    # =============================================
    # FILLED TABLE - Extract data from results
    # =============================================

    cont_table <- chi_results_list$ContingencyTable

    # Get the counts for each cell
    cell_11 <- cont_table[1, 1]
    cell_12 <- cont_table[1, 2]
    cell_21 <- cont_table[2, 1]
    cell_22 <- cont_table[2, 2]

    # Calculate row totals
    row1_total <- cell_11 + cell_12
    row2_total <- cell_21 + cell_22

    # Calculate column totals
    col1_total <- cell_11 + cell_21
    col2_total <- cell_12 + cell_22

    # Grand total
    grand_total <- sum(cont_table)

    if (include_percentages) {
      # Format cells with count and row percentage
      format_cell <- function(count, row_total) {
        pct <- format_stat((count / row_total) * 100, digits = 1)
        paste0(count, " (", pct, "%)")
      }

      cell_11_txt <- format_cell(cell_11, row1_total)
      cell_12_txt <- format_cell(cell_12, row1_total)
      cell_21_txt <- format_cell(cell_21, row2_total)
      cell_22_txt <- format_cell(cell_22, row2_total)

      row1_total_txt <- paste0(row1_total, " (100%)")
      row2_total_txt <- paste0(row2_total, " (100%)")
    } else {
      # Just counts, no percentages
      cell_11_txt <- as.character(cell_11)
      cell_12_txt <- as.character(cell_12)
      cell_21_txt <- as.character(cell_21)
      cell_22_txt <- as.character(cell_22)
      row1_total_txt <- as.character(row1_total)
      row2_total_txt <- as.character(row2_total)
    }

    # Create data frame
    data <- data.frame(
      " " = c(var1_labels[1], var1_labels[2], "Total"),
      "Group1" = c(cell_11_txt, cell_21_txt, as.character(col1_total)),
      "Group2" = c(cell_12_txt, cell_22_txt, as.character(col2_total)),
      "Total" = c(row1_total_txt, row2_total_txt, as.character(grand_total)),
      check.names = FALSE
    )

    names(data) <- c(" ", var2_labels[1], var2_labels[2], "Total")

    # Extract chi-square stats for footer
    chi_sq <- chi_results_list$ChiSquare$chi_sq
    p_value <- chi_results_list$ChiSquare$p_value
    df <- chi_results_list$ChiSquare$df

    # Format p-value for footer
    if (p_value < 0.001) {
      p_text <- "< .001"
    } else {
      p_text <- sprintf("= %.3f", p_value)
    }

    footer_text <- paste0("Note. \u03C7\u00B2(", df, ") = ", chi_sq, ", p ", p_text, ".")

  } else {
    # =============================================
    # BLANK TABLE - Empty template
    # =============================================

    data <- data.frame(
      " " = c(var1_labels[1], var1_labels[2], "Total"),
      "Group1" = c("", "", ""),
      "Group2" = c("", "", ""),
      "Total" = c("", "", ""),
      check.names = FALSE
    )

    names(data) <- c(" ", var2_labels[1], var2_labels[2], "Total")

    footer_text <- "Note. Fill in cell counts. * p < .05. ** p < .01."
  }

  # =============================================
  # BUILD THE FLEXTABLE
  # =============================================

  apa_table <- data |>
    flextable::flextable() |>
    flextable::set_table_properties(layout = "autofit", align = "left", width = 0.8) |>
    # Set first column header to row variable name
    flextable::set_header_labels(" " = var1_name) |>
    # Add spanning header for column variable
    flextable::add_header_row(
      values = c("", var2_name, ""),
      colwidths = c(1, 2, 1)
    )

  # Add table title if provided
  if (!is.null(table_title)) {
    apa_table <- apa_table |>
      flextable::set_caption(caption = table_title)
  }

  # APA-style formatting
  apa_table <- apa_table |>
    # Borders
    flextable::border_remove() |>
    flextable::hline_top(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline(i = 1, part = "header", border = officer::fp_border(width = 1)) |>
    flextable::hline_bottom(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "body", border = officer::fp_border(width = 2)) |>
    # Alignment
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    # Footer
    flextable::add_footer_lines(footer_text) |>
    flextable::fontsize(part = "footer", size = 10) |>
    flextable::align(part = "footer", align = "left") |>
    # Font sizes
    flextable::fontsize(size = 11, part = "header") |>
    flextable::fontsize(size = 11, part = "body")

  return(apa_table)
}


#' Chi-Square Combined Results Table
#'
#' Creates a compact flextable combining chi-square test results with
#' frequency counts in a single table. Row 1 shows chi-square statistics,
#' rows 2-3 show frequency counts per variable level.
#'
#' @param rh_name Character. Research hypothesis name/label.
#' @param vars Character vector of length 2. Variable names.
#' @param chi_results_list Output from [chi_square_answers()].
#' @param var1_labels Character vector of length 2. Labels for var1 levels.
#' @param var2_labels Character vector of length 2. Labels for var2 levels.
#' @param KEY Logical. If `TRUE`, fill with values; if `FALSE`, blank.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' \dontrun{
#' data(superman, package = "psych350data")
#' result <- chi_square_answers(superman, "clark_grp", "tomatometer")
#' ft <- create_chi_combined_table("RH1", c("clark_grp", "tomatometer"),
#'   result,
#'   var1_labels = c("Under 6ft", "6ft+"),
#'   var2_labels = c("Rotten", "Fresh"))
#' ft
#' }
#'
#' @export
create_chi_combined_table <- function(rh_name, vars, chi_results_list,
                                      var1_labels = c("Level 1", "Level 2"),
                                      var2_labels = c("Level A", "Level B"),
                                      KEY = TRUE) {

  if (KEY && !is.null(chi_results_list)) {
    # Extract stats
    chi_sq <- chi_results_list$ChiSquare$chi_sq
    p_value <- chi_results_list$ChiSquare$p_value
    df <- chi_results_list$ChiSquare$df
    var1_desc <- chi_results_list$Var1_Descriptives
    var2_desc <- chi_results_list$Var2_Descriptives

    # Determine decision
    if (p_value < 0.05) {
      decision <- "Reject H\u2080"
    } else {
      decision <- "Retain H\u2080"
    }

    # Format p-value
    if (p_value < 0.001) {
      p_formatted <- "< .001"
    } else {
      p_formatted <- format_p_value(p_value)
    }

    # Build combined table
    combined_data <- tibble::tibble(
      ` ` = c(
        paste0("Chi-Square: ", rh_name),
        paste0("  ", vars[1], " (", var1_labels[1], " / ", var1_labels[2], ")"),
        paste0("  ", vars[2], " (", var2_labels[1], " / ", var2_labels[2], ")")
      ),
      Column2 = c(
        as.character(chi_sq),
        paste0(var1_desc$n[1], " / ", var1_desc$n[2]),
        paste0(var2_desc$n[1], " / ", var2_desc$n[2])
      ),
      Column3 = c(p_formatted, "", ""),
      Column4 = c(as.character(df), "", ""),
      Column5 = c(decision, "", "")
    )
  } else {
    combined_data <- tibble::tibble(
      ` ` = c(
        paste0("Chi-Square: ", rh_name),
        paste0("  ", vars[1], " (", var1_labels[1], " / ", var1_labels[2], ")"),
        paste0("  ", vars[2], " (", var2_labels[1], " / ", var2_labels[2], ")")
      ),
      Column2 = c("", "", ""),
      Column3 = c("", "", ""),
      Column4 = c("", "", ""),
      Column5 = c("", "", "")
    )
  }

  ft <- flextable::flextable(combined_data) |>
    flextable::set_header_labels(
      ` ` = " ",
      Column2 = "\u03C7\u00B2 / Counts",
      Column3 = "p",
      Column4 = "df",
      Column5 = "Decision"
    ) |>
    flextable::theme_box() |>
    flextable::autofit() |>
    flextable::fontsize(size = 10, part = "all")

  return(ft)
}


#' Chi-Square Contingency Table (Flextable)
#'
#' Creates a formatted contingency table showing observed frequencies.
#'
#' @param chi_results Output from [chi_square_answers()] or
#'   [chi_square_kgroup_answers()], or NULL for blank table.
#' @param var1_name Character. Display name for the row variable.
#' @param var2_name Character. Display name for the column variable.
#' @param var1_levels Character vector or NULL. Labels for row levels.
#' @param var2_levels Character vector or NULL. Labels for column levels.
#' @param KEY Logical. If TRUE (default), fill with values; if FALSE, blank.
#' @param table_title Character or NULL. Optional table title.
#'
#' @return A [flextable::flextable()] object.
#'
#' @export

create_chi_contingency_table <- function(chi_results = NULL,
                                         var1_name = "Variable 1",
                                         var2_name = "Variable 2",
                                         var1_levels = NULL,
                                         var2_levels = NULL,
                                         KEY = TRUE,
                                         table_title = NULL) {

  if (KEY && !is.null(chi_results)) {
    ct_matrix <- chi_results$ContingencyTable

    if (!is.null(var1_levels) && !is.null(var2_levels)) {
      if (length(var1_levels) == ncol(ct_matrix) && length(var2_levels) == nrow(ct_matrix)) {
        ct_matrix <- t(ct_matrix)
      }
    }

    n_rows <- nrow(ct_matrix)
    n_cols <- ncol(ct_matrix)

    if (!is.null(var1_levels)) {
      use_var1_levels <- var1_levels
    } else {
      use_var1_levels <- rownames(ct_matrix)
      if (is.null(use_var1_levels)) use_var1_levels <- paste("Level", 1:n_rows)
    }

    if (!is.null(var2_levels)) {
      use_var2_levels <- var2_levels
    } else {
      use_var2_levels <- colnames(ct_matrix)
      if (is.null(use_var2_levels)) use_var2_levels <- paste("Group", 1:n_cols)
    }

    data <- data.frame(` ` = use_var1_levels,
                       stringsAsFactors = FALSE,
                       check.names = FALSE)

    for (i in 1:n_cols) {
      data[[use_var2_levels[i]]] <- as.character(ct_matrix[, i])
    }

  } else {
    if (is.null(var1_levels)) var1_levels <- c("Level 1", "Level 2")
    if (is.null(var2_levels)) var2_levels <- c("Group A", "Group B", "Group C")

    data <- data.frame(` ` = var1_levels,
                       stringsAsFactors = FALSE,
                       check.names = FALSE)

    for (i in 1:length(var2_levels)) {
      data[[var2_levels[i]]] <- rep("", length(var1_levels))
    }
  }

  data <- tibble::as_tibble(data)

  contingency_table <- data |>
    flextable::flextable() |>
    flextable::set_header_labels(` ` = var1_name)

  contingency_table <- contingency_table |>
    flextable::add_header_row(
      values = c("", var2_name),
      colwidths = c(1, ncol(data) - 1),
      top = TRUE
    )

  if (!is.null(table_title)) {
    contingency_table <- contingency_table |>
      flextable::add_header_lines(values = table_title, top = TRUE)
  }

  contingency_table <- contingency_table |>
    flextable::theme_box() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::autofit()

  if (KEY && !is.null(chi_results)) {
    contingency_table <- contingency_table |>
      flextable::color(j = 2:ncol(data), color = "#d00000", part = "body")
  }

  return(contingency_table)
}


#' Format Chi-Square Omnibus Results Text
#'
#' Creates formatted text output for chi-square omnibus statistics.
#'
#' @param chi_results_list Output from chi-square analysis function.
#' @param var1_name Character. Display name for variable 1.
#' @param var2_name Character. Display name for variable 2.
#' @param KEY Logical. If TRUE, show answers; if FALSE, show blanks.
#'
#' @return A character string.
#'
#' @export
format_chi_omnibus_results <- function(chi_results_list,
                                       var1_name = "Variable 1",
                                       var2_name = "Variable 2",
                                       KEY = TRUE) {

  if (KEY) {
    chi_sq <- sprintf("%.2f", chi_results_list$ChiSquare$chi_sq)
    df <- chi_results_list$ChiSquare$df
    p_value <- sprintf("%.3f", chi_results_list$ChiSquare$p_value)
    n <- sum(chi_results_list$ContingencyTable)
    k <- ncol(chi_results_list$ContingencyTable)
    n_per_group <- round(n / nrow(chi_results_list$ContingencyTable), 2)

    if (chi_results_list$ChiSquare$p_value < 0.05) {
      pairwise_needed <- "Yes because there is a pattern of difference that needs to be determined based on p-value."
    } else {
      pairwise_needed <- "No because the omnibus test was not significant."
    }

    chi_crit <- sprintf("%.2f", chi_results_list$ChiCrit)

    output <- paste0(
      "\\u03C7\\u00B2 = ", chi_sq, "        df = ", df, "        p = ", p_value,
      "       N = ", n, "       k = ", k, "       n = ", n_per_group, "\n\n",
      "Pairwise X\\u00B2-critical, Pairwise Comparisons, Effect Sizes & Statistical Decision Errors\n\n",
      "Do we need to perform pairwise X\\u00B2 comparisons to test the RH? Why or why not?\n",
      pairwise_needed, "\n\n",
      "X\\u00B2-critical = ", chi_crit, "\n"
    )

  } else {
    output <- paste0(
      "\\u03C7\\u00B2 = ____      df = ____      p = ____      N = ____      k = ____      n = ____\n\n",
      "Pairwise X\\u00B2-critical, Pairwise Comparisons, Effect Sizes & Statistical Decision Errors\n\n",
      "Do we need to perform pairwise X\\u00B2 comparisons to test the RH? Why or why not?\n",
      "____\n\n",
      "X\\u00B2-critical = ____\n"
    )
  }

  return(output)
}


#' Chi-Square Pairwise Results Table (Answer KEY)
#'
#' Creates a flextable showing pairwise chi-square comparison results.
#'
#' @param chi_results_list Output from [chi_square_kgroup_answers()].
#' @param KEY Logical. If TRUE (default), fill with values; if FALSE, blank.
#' @param comparison_var_label Character or NULL. Label for the percentage
#'   comparison variable.
#'
#' @return A [flextable::flextable()] object.
#'
#' @export
chisq_pairwise_KEY <- function(chi_results_list,
                               KEY = TRUE,
                               comparison_var_label = NULL) {

  pairwise <- chi_results_list$Pairwise
  n_pairwise <- length(pairwise)

  if (is.null(comparison_var_label)) {
    comparison_var_label <- chi_results_list$pct_var2_label
  }

  pct_row_label <- if (!is.null(comparison_var_label)) {
    paste0("% ", comparison_var_label, " \u2192")
  } else {
    "% comparison \u2192"
  }

  row_labels <- c(
    "Pairwise comparison \u2192",
    pct_row_label,
    "\u03C7\u00B2 result \u2192",
    "Type of Stat Error risked \u2192",
    "Pairwise effect size (r) \u2192",
    "Power Problem? \u2192"
  )

  if (KEY) {
    pairwise_data <- data.frame(
      ` ` = row_labels,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    for (i in seq_len(n_pairwise)) {
      power_text <- pairwise[[i]]$power_problem
      power_code <- if (grepl("rejecting H0", power_text)) {
        "*"
      } else if (grepl("too small", power_text)) {
        "**"
      } else {
        "***"
      }

      col_name <- paste0("Comp", i)
      pairwise_data[[col_name]] <- c(
        pairwise[[i]]$comparison,
        paste0(pairwise[[i]]$pct1, "% vs ", pairwise[[i]]$pct2, "%"),
        paste0(pairwise[[i]]$chi_sq, " ", pairwise[[i]]$chi_result),
        pairwise[[i]]$error_type,
        as.character(pairwise[[i]]$effect_size),
        power_code
      )
    }
  } else {
    pairwise_data <- data.frame(
      ` ` = row_labels,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    for (i in seq_len(n_pairwise)) {
      col_name <- paste0("Comp", i)
      pairwise_data[[col_name]] <- c(
        pairwise[[i]]$comparison,
        "", "", "", "", ""
      )
    }
  }

  pairwise_data <- tibble::as_tibble(pairwise_data)

  ft <- pairwise_data |>
    flextable::flextable() |>
    flextable::theme_box() |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::delete_part(part = "header") |>
    flextable::hline_top(border = officer::fp_border(width = 1), part = "body") |>
    flextable::autofit()

  if (KEY) {
    ft <- ft |>
      flextable::color(i = 2:6, j = 2:(n_pairwise + 1), color = "#d00000", part = "body")
  }

  caption_text <- paste0(
    "*   No \u2013 rejecting H0: means there was sufficient power\n",
    "**  No \u2013 effect is \"too small to be interesting,\" (r < .10) ",
    "so being nonsignificant doesn't indicate a power problem\n",
    "*** Yes \u2013 The effect is \"large enough to be interesting,\" (r > .10) ",
    "so being nonsignificant indicates there likely is a power problem"
  )

  ft |>
    flextable::add_footer_lines(values = caption_text) |>
    flextable::align(align = "left", part = "footer") |>
    flextable::fontsize(size = 9, part = "footer") |>
    flextable::merge_at(part = "footer", i = 1) |>
    flextable::hline(part = "footer", border = officer::fp_border(width = 0))
}




