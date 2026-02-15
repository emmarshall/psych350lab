#' Factorial ANOVA Statistics Table (Interactive Webexercise)
#'
#' Creates an interactive table using tinytable and webexercises showing
#' interaction, main effects, and LSD calculation parameters with
#' fill-in-the-blank and multiple choice inputs.
#'
#' @param rh_name Character. Research hypothesis name/label.
#' @param anova_results_list Output from [anova_factorial_answers()].
#' @param iv1_name Character. Display name for IV1.
#' @param iv2_name Character. Display name for IV2.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' data(superman_data)
#' sm <- superman_data
#' sm$era <- ifelse(sm$year >= 2000, 2, 1)
#' result <- anova_factorial_answers(sm, dv = "clark_height_in",
#'   iv1 = "clark_grp", iv2 = "era",
#'   iv1_labels = c("Under 6ft", "6ft+"),
#'   iv2_labels = c("Pre-2000", "Post-2000"))
#' create_factorial_anova_stats_table("RH1", result,
#'   iv1_name = "Height Group", iv2_name = "Era")
#' }
#'
#' @export
create_factorial_anova_stats_table <- function(rh_name, anova_results_list,
                                               iv1_name = "IV1", iv2_name = "IV2") {

  if (!requireNamespace("tinytable", quietly = TRUE)) {
    stop("Package 'tinytable' is required. Install with install.packages('tinytable')")
  }
  if (!requireNamespace("webexercises", quietly = TRUE)) {
    stop("Package 'webexercises' is required. Install with install.packages('webexercises')")
  }

  # Extract ANOVA stats
  f_iv1 <- anova_results_list$ANOVA$MainEffect_IV1$F
  p_iv1 <- anova_results_list$ANOVA$MainEffect_IV1$p_value
  df_iv1 <- anova_results_list$ANOVA$MainEffect_IV1$df

  f_iv2 <- anova_results_list$ANOVA$MainEffect_IV2$F
  p_iv2 <- anova_results_list$ANOVA$MainEffect_IV2$p_value
  df_iv2 <- anova_results_list$ANOVA$MainEffect_IV2$df

  f_interaction <- anova_results_list$ANOVA$Interaction$F
  p_interaction <- anova_results_list$ANOVA$Interaction$p_value
  df_interaction <- anova_results_list$ANOVA$Interaction$df

  df_within <- anova_results_list$ANOVA$df_within
  mse <- anova_results_list$ANOVA$mse
  k <- anova_results_list$ANOVA$k
  mean_n <- anova_results_list$ANOVA$mean_n
  lsd_mmd <- anova_results_list$LSD$lsd_mmd

  # Format p-values
  p_iv1_fmt <- ifelse(p_iv1 < 0.001, "<.001", sprintf("%.2f", p_iv1))
  p_iv2_fmt <- ifelse(p_iv2 < 0.001, "<.001", sprintf("%.2f", p_iv2))
  p_interaction_fmt <- ifelse(p_interaction < 0.001, "<.001", sprintf("%.2f", p_interaction))

  # Determine correct answer for LSD question based on interaction
  if (p_interaction < 0.05) {
    posthoc_mcq <- webexercises::mcq(c(
      "No \u2014 a nonsignificant interaction",
      answer = "Yes \u2014 significant interaction"
    ))
  } else {
    posthoc_mcq <- webexercises::mcq(c(
      answer = "No \u2014 a nonsignificant interaction",
      "Yes \u2014 significant interaction"
    ))
  }

  # Table 1: Interaction
  interaction_table_data <- tibble::tibble(
    ` ` = paste("Interaction:", iv1_name, "\u00D7", iv2_name),
    F = webexercises::fitb(f_interaction),
    p = webexercises::fitb(p_interaction_fmt),
    `df (between)` = webexercises::fitb(df_interaction),
    `df (within)` = webexercises::fitb(df_within),
    MSE = webexercises::fitb(mse),
    `Do we need to perform LSD pairwise comparisons?` = posthoc_mcq
  )

  interaction_table <- tinytable::tt(interaction_table_data) |>
    tinytable::format_tt(escape = FALSE)

  # Table 2: Main Effect IV1
  iv1_table_data <- tibble::tibble(
    ` ` = paste("Main Effect:", iv1_name),
    F = webexercises::fitb(f_iv1),
    p = webexercises::fitb(p_iv1_fmt),
    `df (between)` = webexercises::fitb(df_iv1),
    `df (within)` = webexercises::fitb(df_within),
    MSE = webexercises::fitb(mse),
    `  ` = ""
  )

  iv1_table <- tinytable::tt(iv1_table_data) |>
    tinytable::format_tt(escape = FALSE)

  # Table 3: Main Effect IV2
  iv2_table_data <- tibble::tibble(
    ` ` = paste("Main Effect:", iv2_name),
    F = webexercises::fitb(f_iv2),
    p = webexercises::fitb(p_iv2_fmt),
    `df (between)` = webexercises::fitb(df_iv2),
    `df (within)` = webexercises::fitb(df_within),
    MSE = webexercises::fitb(mse),
    `   ` = ""
  )

  iv2_table <- tinytable::tt(iv2_table_data) |>
    tinytable::format_tt(escape = FALSE)

  # Table 4: LSD Calculation Parameters
  lsd_calc_table_data <- tibble::tibble(
    ` ` = "# conditions =",
    `N` = webexercises::fitb(k),
    k = paste("n ="),
    `average n` = webexercises::fitb(mean_n),
    `    ` = "df error =",
    `     ` = webexercises::fitb(df_within),
    `      ` = paste("MSe =", webexercises::fitb(mse), "LSDmmd =", webexercises::fitb(lsd_mmd))
  )

  lsd_calc_table <- tinytable::tt(lsd_calc_table_data) |>
    tinytable::format_tt(escape = FALSE)

  # Combine tables
  combined_table <- tinytable::rbind2(interaction_table, iv1_table, use_names = FALSE) |>
    tinytable::rbind2(iv2_table, use_names = FALSE) |>
    tinytable::rbind2(lsd_calc_table, use_names = FALSE) |>
    tinytable::style_tt(bootstrap_class = "table table-striped table-hover table-sm",
                        bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;")

  return(combined_table)
}


#' Factorial ANOVA Checker (Interactive Webexercise)
#'
#' Creates a compact interactive checker table for factorial ANOVA results
#' with the LSD calculation row placed directly after the interaction row.
#'
#' @param rh_name Character. Research hypothesis name/label.
#' @param anova_results_list Output from [anova_factorial_answers()].
#' @param iv1_name Character. Display name for IV1.
#' @param iv2_name Character. Display name for IV2.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' data(superman_data)
#' sm <- superman_data
#' sm$era <- ifelse(sm$year >= 2000, 2, 1)
#' result <- anova_factorial_answers(sm, dv = "clark_height_in",
#'   iv1 = "clark_grp", iv2 = "era",
#'   iv1_labels = c("Under 6ft", "6ft+"),
#'   iv2_labels = c("Pre-2000", "Post-2000"))
#' create_factorial_checker("RH1", result,
#'   iv1_name = "Height Group", iv2_name = "Era")
#' }
#'
#' @export
create_factorial_checker <- function(rh_name, anova_results_list,
                                     iv1_name = "IV1", iv2_name = "IV2") {

  if (!requireNamespace("tinytable", quietly = TRUE)) {
    stop("Package 'tinytable' is required. Install with install.packages('tinytable')")
  }
  if (!requireNamespace("webexercises", quietly = TRUE)) {
    stop("Package 'webexercises' is required. Install with install.packages('webexercises')")
  }

  # Extract ANOVA stats
  f_iv1 <- anova_results_list$ANOVA$MainEffect_IV1$F
  p_iv1 <- anova_results_list$ANOVA$MainEffect_IV1$p_value
  df_iv1 <- anova_results_list$ANOVA$MainEffect_IV1$df

  f_iv2 <- anova_results_list$ANOVA$MainEffect_IV2$F
  p_iv2 <- anova_results_list$ANOVA$MainEffect_IV2$p_value
  df_iv2 <- anova_results_list$ANOVA$MainEffect_IV2$df

  f_interaction <- anova_results_list$ANOVA$Interaction$F
  p_interaction <- anova_results_list$ANOVA$Interaction$p_value
  df_interaction <- anova_results_list$ANOVA$Interaction$df

  df_within <- anova_results_list$ANOVA$df_within
  mse <- anova_results_list$ANOVA$mse
  k <- anova_results_list$ANOVA$k
  mean_n <- anova_results_list$ANOVA$mean_n
  lsd_mmd <- anova_results_list$LSD$lsd_mmd

  # Format p-values
  p_iv1_fmt <- ifelse(p_iv1 < 0.001, "<.001", sprintf("%.2f", p_iv1))
  p_iv2_fmt <- ifelse(p_iv2 < 0.001, "<.001", sprintf("%.2f", p_iv2))
  p_interaction_fmt <- ifelse(p_interaction < 0.001, "<.001", sprintf("%.2f", p_interaction))

  # Determine correct answer for LSD question
  if (p_interaction < 0.05) {
    posthoc_mcq <- webexercises::mcq(c(
      "No \u2014 a nonsignificant interaction",
      answer = "Yes \u2014 significant interaction"
    ))
  } else {
    posthoc_mcq <- webexercises::mcq(c(
      answer = "No \u2014 a nonsignificant interaction",
      "Yes \u2014 significant interaction"
    ))
  }

  # Table 1: Interaction
  interaction_table_data <- tibble::tibble(
    ` ` = paste("Interaction:", iv1_name, "\u00D7", iv2_name),
    F = webexercises::fitb(f_interaction),
    p = webexercises::fitb(p_interaction_fmt),
    `df (between)` = webexercises::fitb(df_interaction),
    `df (within)` = webexercises::fitb(df_within),
    MSE = webexercises::fitb(mse),
    `Do we need to perform LSD pairwise comparisons?` = posthoc_mcq
  )

  interaction_table <- tinytable::tt(interaction_table_data) |>
    tinytable::format_tt(escape = FALSE)

  # Table 2: LSD Calculation Parameters (placed right after interaction)
  lsd_calc_table_data <- tibble::tibble(
    ` ` = "Components for LSDmmd:",
    `# of conditions` = webexercises::fitb(k),
    `average n` = webexercises::fitb(mean_n),
    `df error` = webexercises::fitb(df_within),
    `MSe` = webexercises::fitb(mse),
    `LSDmmd` = webexercises::fitb(lsd_mmd),
    `   ` = ""
  )

  lsd_calc_table <- tinytable::tt(lsd_calc_table_data) |>
    tinytable::format_tt(escape = FALSE)

  # Table 3: Main Effect IV1
  iv1_table_data <- tibble::tibble(
    ` ` = paste("Main Effect:", iv1_name),
    F = webexercises::fitb(f_iv1),
    p = webexercises::fitb(p_iv1_fmt),
    `df (between)` = webexercises::fitb(df_iv1),
    `df (within)` = webexercises::fitb(df_within),
    MSE = webexercises::fitb(mse),
    `  ` = ""
  )

  iv1_table <- tinytable::tt(iv1_table_data) |>
    tinytable::format_tt(escape = FALSE)

  # Table 4: Main Effect IV2
  iv2_table_data <- tibble::tibble(
    ` ` = paste("Main Effect:", iv2_name),
    F = webexercises::fitb(f_iv2),
    p = webexercises::fitb(p_iv2_fmt),
    `df (between)` = webexercises::fitb(df_iv2),
    `df (within)` = webexercises::fitb(df_within),
    MSE = webexercises::fitb(mse),
    `   ` = ""
  )

  iv2_table <- tinytable::tt(iv2_table_data) |>
    tinytable::format_tt(escape = FALSE)

  # Combine: Interaction -> LSD -> ME IV1 -> ME IV2
  combined_table <- tinytable::rbind2(interaction_table, lsd_calc_table, use_names = FALSE) |>
    tinytable::rbind2(iv1_table, use_names = FALSE) |>
    tinytable::rbind2(iv2_table, use_names = FALSE) |>
    tinytable::style_tt(bootstrap_class = "table table-striped table-hover table-sm",
                        bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;")

  return(combined_table)
}


#' Factorial Descriptives Checker (Interactive Webexercise Grid)
#'
#' Creates an interactive grid table showing cell means and estimated marginal
#' means for a factorial ANOVA, with fill-in-the-blank inputs.
#'
#' @param anova_results_list Output from [anova_factorial_answers()].
#' @param iv1_name Character. Display name for IV1.
#' @param iv2_name Character. Display name for IV2.
#' @param iv1_labels Character vector or `NULL`. Override labels for IV1 levels.
#' @param iv2_labels Character vector or `NULL`. Override labels for IV2 levels.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' data(superman_data)
#' sm <- superman_data
#' sm$era <- ifelse(sm$year >= 2000, 2, 1)
#' result <- anova_factorial_answers(sm, dv = "clark_height_in",
#'   iv1 = "clark_grp", iv2 = "era",
#'   iv1_labels = c("Under 6ft", "6ft+"),
#'   iv2_labels = c("Pre-2000", "Post-2000"))
#' create_factorial_desc_checker(result,
#'   iv1_name = "Height Group", iv2_name = "Era")
#' }
#'
#' @export
create_factorial_desc_checker <- function(anova_results_list,
                                          iv1_name = "IV1",
                                          iv2_name = "IV2",
                                          iv1_labels = NULL,
                                          iv2_labels = NULL) {

  if (!requireNamespace("tinytable", quietly = TRUE)) {
    stop("Package 'tinytable' is required. Install with install.packages('tinytable')")
  }
  if (!requireNamespace("webexercises", quietly = TRUE)) {
    stop("Package 'webexercises' is required. Install with install.packages('webexercises')")
  }

  # Get descriptives and EMMs
  desc_stats <- anova_results_list$Descriptives
  emm_iv1 <- anova_results_list$EMMs$IV1
  emm_iv2 <- anova_results_list$EMMs$IV2

  # Use labels from FactorLevels (defines correct order)
  if (!is.null(anova_results_list$FactorLevels)) {
    iv1_levels_actual <- anova_results_list$FactorLevels$iv1_levels
    iv2_levels_actual <- anova_results_list$FactorLevels$iv2_levels
    final_iv1_labels <- anova_results_list$FactorLevels$iv1_labels
    final_iv2_labels <- anova_results_list$FactorLevels$iv2_labels
  } else {
    stop("FactorLevels not found. Please re-run anova_factorial_answers() with the updated function.")
  }

  # Override with user-provided labels if given
  if (!is.null(iv1_labels)) final_iv1_labels <- iv1_labels
  if (!is.null(iv2_labels)) final_iv2_labels <- iv2_labels

  n_iv1 <- length(final_iv1_labels)
  n_iv2 <- length(final_iv2_labels)

  # Build row labels
  row_labels <- final_iv1_labels

  # Build columns for IV2 cell means
  col_data <- list()
  for (j in seq_len(n_iv2)) {
    col_values <- c()
    for (i in seq_len(n_iv1)) {
      cell_idx <- which(desc_stats$iv1_level == iv1_levels_actual[i] &
                          desc_stats$iv2_level == iv2_levels_actual[j])

      if (length(cell_idx) > 0) {
        col_values <- c(col_values, webexercises::fitb(desc_stats$mean[cell_idx[1]]))
      } else {
        col_values <- c(col_values, "")
      }
    }
    col_data[[j]] <- col_values
  }

  # Add EMM column for IV1
  emm_iv1_values <- c()
  for (i in seq_len(n_iv1)) {
    emm_row <- which(emm_iv1$iv1_label == final_iv1_labels[i])
    if (length(emm_row) > 0) {
      emm_iv1_values <- c(emm_iv1_values,
                          webexercises::fitb(round(as.numeric(emm_iv1$mean[emm_row[1]]), 2)))
    } else {
      emm_iv1_values <- c(emm_iv1_values, webexercises::fitb("ERROR"))
    }
  }

  # Add footer row for IV2 marginal means
  row_labels <- c(row_labels, paste0("EMM: ", iv2_name))

  for (j in seq_len(n_iv2)) {
    emm_row <- which(emm_iv2$iv2_label == final_iv2_labels[j])
    if (length(emm_row) > 0) {
      col_data[[j]] <- c(col_data[[j]],
                         webexercises::fitb(round(as.numeric(emm_iv2$mean[emm_row[1]]), 2)))
    } else {
      col_data[[j]] <- c(col_data[[j]], webexercises::fitb("ERROR"))
    }
  }

  # Add empty cell in EMM column for footer row
  emm_iv1_values <- c(emm_iv1_values, "")

  # Build final tibble
  table_data <- tibble::tibble(
    ` ` = row_labels,
    `  ` = col_data[[1]],
    `   ` = col_data[[2]],
    `    ` = emm_iv1_values
  )

  # Set proper column names
  colnames(table_data) <- c(iv1_name, final_iv2_labels[1], final_iv2_labels[2],
                            paste0("EMM: ", iv1_name))

  # Create table
  desc_table <- tinytable::tt(table_data) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(bootstrap_class = "table table-striped table-hover table-sm",
                        bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;")

  return(desc_table)
}


#' Factorial ANOVA Table with Cell Comparisons
#'
#' Creates a flextable showing cell means in a 2x2 grid with comparison
#' operators (>, <, =) based on LSD minimum mean difference, plus
#' estimated marginal means. Includes vertical comparison rows between
#' IV1 levels for 2x2 designs.
#'
#' @param anova_results_list Output from [anova_factorial_answers()].
#' @param iv1_name Character. Display name for IV1 (rows).
#' @param iv2_name Character. Display name for IV2 (columns).
#' @param KEY Logical. If `TRUE`, show filled values and comparisons.
#'   If `FALSE`, show blank template.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' \dontrun{
#' data(superman_data)
#' sm <- superman_data
#' sm$era <- ifelse(sm$year >= 2000, 2, 1)
#' result <- anova_factorial_answers(sm, dv = "clark_height_in",
#'   iv1 = "clark_grp", iv2 = "era",
#'   iv1_labels = c("Under 6ft", "6ft+"),
#'   iv2_labels = c("Pre-2000", "Post-2000"))
#' ft <- factorial_table_with_comparisons(result,
#'   iv1_name = "Height Group", iv2_name = "Era")
#' ft
#' }
#'
#' @export
factorial_table_with_comparisons <- function(anova_results_list,
                                             iv1_name = "IV1",
                                             iv2_name = "IV2",
                                             KEY = TRUE) {

  desc_stats <- anova_results_list$Descriptives
  emm_iv1 <- anova_results_list$EMMs$IV1
  emm_iv2 <- anova_results_list$EMMs$IV2
  lsd_mmd <- anova_results_list$LSD$lsd_mmd

  # Validate labels exist
  if (!"iv1_label" %in% names(desc_stats) || !"iv2_label" %in% names(desc_stats)) {
    stop("Labels not found in desc_stats. Make sure you passed iv1_labels and iv2_labels to anova_factorial_answers()")
  }

  # Get labels in correct order from FactorLevels
  if (!is.null(anova_results_list$FactorLevels)) {
    iv1_labels <- anova_results_list$FactorLevels$iv1_labels
    iv2_labels <- anova_results_list$FactorLevels$iv2_labels
  } else {
    iv1_labels <- unique(desc_stats$iv1_label)
    iv2_labels <- unique(desc_stats$iv2_label)
  }

  n_iv1 <- length(iv1_labels)
  n_iv2 <- length(iv2_labels)

  if (KEY) {
    data_list <- list()
    data_list[[iv1_name]] <- iv1_labels

    # First IV2 level column - cell means in order of iv1_labels
    col_values <- c()
    for (i in seq_len(n_iv1)) {
      cell_row <- desc_stats[desc_stats$iv1_label == iv1_labels[i] &
                               desc_stats$iv2_label == iv2_labels[1], ]
      if (nrow(cell_row) > 0) {
        col_values <- c(col_values, sprintf("%.4f", cell_row$mean[1]))
      } else {
        col_values <- c(col_values, "NA")
      }
    }
    data_list[[as.character(iv2_labels[1])]] <- col_values

    # Horizontal comparison operators (between IV2 levels within each IV1 level)
    comp_col <- c()
    for (i in seq_len(n_iv1)) {
      cell1 <- desc_stats[desc_stats$iv1_label == iv1_labels[i] &
                            desc_stats$iv2_label == iv2_labels[1], ]
      cell2 <- desc_stats[desc_stats$iv1_label == iv1_labels[i] &
                            desc_stats$iv2_label == iv2_labels[2], ]

      if (nrow(cell1) > 0 && nrow(cell2) > 0) {
        mean1 <- cell1$mean[1]
        mean2 <- cell2$mean[1]
        mean_diff <- abs(mean1 - mean2)

        if (mean_diff > lsd_mmd) {
          comp_col <- c(comp_col, ifelse(mean1 < mean2, "<", ">"))
        } else {
          comp_col <- c(comp_col, "=")
        }
      } else {
        comp_col <- c(comp_col, "?")
      }
    }
    data_list[[" "]] <- comp_col

    # Second IV2 level column
    col_values <- c()
    for (i in seq_len(n_iv1)) {
      cell_row <- desc_stats[desc_stats$iv1_label == iv1_labels[i] &
                               desc_stats$iv2_label == iv2_labels[2], ]
      if (nrow(cell_row) > 0) {
        col_values <- c(col_values, sprintf("%.4f", cell_row$mean[1]))
      } else {
        col_values <- c(col_values, "NA")
      }
    }
    data_list[[as.character(iv2_labels[2])]] <- col_values

    # EMM column for IV1 (marginal means)
    marginal_col_name <- paste0("EMM: ", iv1_name)
    data_list[[marginal_col_name]] <- sprintf("%.4f", emm_iv1$mean)

    data <- as.data.frame(data_list, stringsAsFactors = FALSE, check.names = FALSE)

    # Add vertical comparison row between IV1 levels (for 2x2 designs)
    if (n_iv1 == 2) {
      # Column 1 comparison (first IV2 level, going down)
      cell1_iv2_1 <- desc_stats[desc_stats$iv1_label == iv1_labels[1] &
                                  desc_stats$iv2_label == iv2_labels[1], ]
      cell2_iv2_1 <- desc_stats[desc_stats$iv1_label == iv1_labels[2] &
                                  desc_stats$iv2_label == iv2_labels[1], ]

      mean_diff_1 <- abs(cell1_iv2_1$mean[1] - cell2_iv2_1$mean[1])
      if (mean_diff_1 > lsd_mmd) {
        # v = top is higher (arrow pointing down), ^ = bottom is higher
        comp_iv2_1 <- ifelse(cell1_iv2_1$mean[1] > cell2_iv2_1$mean[1], "v", "^")
      } else {
        comp_iv2_1 <- "="
      }

      # Column 2 comparison (second IV2 level, going down)
      cell1_iv2_2 <- desc_stats[desc_stats$iv1_label == iv1_labels[1] &
                                  desc_stats$iv2_label == iv2_labels[2], ]
      cell2_iv2_2 <- desc_stats[desc_stats$iv1_label == iv1_labels[2] &
                                  desc_stats$iv2_label == iv2_labels[2], ]

      mean_diff_2 <- abs(cell1_iv2_2$mean[1] - cell2_iv2_2$mean[1])
      if (mean_diff_2 > lsd_mmd) {
        comp_iv2_2 <- ifelse(cell1_iv2_2$mean[1] > cell2_iv2_2$mean[1], "v", "^")
      } else {
        comp_iv2_2 <- "="
      }

      # EMM column comparison
      mean1 <- emm_iv1$mean[1]
      mean2 <- emm_iv1$mean[2]
      me_comp <- ifelse(mean1 > mean2, "v", ifelse(mean1 < mean2, "^", "="))

      # Build comparison row
      new_row <- data.frame(matrix("", nrow = 1, ncol = ncol(data)))
      names(new_row) <- names(data)
      new_row[[1]] <- ""
      new_row[[2]] <- comp_iv2_1
      new_row[[3]] <- ""
      new_row[[4]] <- comp_iv2_2
      new_row[[ncol(data)]] <- me_comp

      # Insert: row 1, comparison row, row 2
      data <- rbind(data[1, ], new_row, data[2, ])
    }

  } else {
    # BLANK version
    data_list <- list()
    data_list[[iv1_name]] <- iv1_labels
    data_list[[as.character(iv2_labels[1])]] <- rep("", n_iv1)
    data_list[[" "]] <- rep("", n_iv1)
    data_list[[as.character(iv2_labels[2])]] <- rep("", n_iv1)
    marginal_col_name <- paste0("EMM: ", iv1_name)
    data_list[[marginal_col_name]] <- rep("", n_iv1)

    data <- as.data.frame(data_list, stringsAsFactors = FALSE, check.names = FALSE)
  }

  # Create flextable
  ft <- flextable::flextable(data)

  # Add spanning header for IV2
  ft <- flextable::add_header_row(ft,
                                  values = c("", iv2_name, ""),
                                  colwidths = c(1, 3, 1),
                                  top = TRUE)

  # Remove all borders, then add borders only around cell means
  ft <- flextable::border_remove(ft)

  cell_rows <- if (KEY && n_iv1 == 2) c(1, 3) else seq_len(n_iv1)

  ft <- flextable::border(ft,
                          i = cell_rows,
                          j = c(2, 4),
                          border.top = officer::fp_border(color = "black", width = 1),
                          border.bottom = officer::fp_border(color = "black", width = 1),
                          border.left = officer::fp_border(color = "black", width = 1),
                          border.right = officer::fp_border(color = "black", width = 1),
                          part = "body")

  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft <- flextable::autofit(ft)

  # Add footer row for IV2 marginal means
  if (KEY) {
    marginal_row_label <- paste0("EMM: ", iv2_name)
    marginal_row <- c(marginal_row_label)
    marginal_row <- c(marginal_row, sprintf("%.4f", emm_iv2$mean[1]))

    # IV2 EMM comparison
    mean1 <- emm_iv2$mean[1]
    mean2 <- emm_iv2$mean[2]
    iv2_comp <- ifelse(mean1 < mean2, "<", ifelse(mean1 > mean2, ">", "="))
    marginal_row <- c(marginal_row, iv2_comp)

    marginal_row <- c(marginal_row, sprintf("%.4f", emm_iv2$mean[2]))
    marginal_row <- c(marginal_row, "")

    ft <- flextable::add_footer_row(ft, values = marginal_row,
                                    colwidths = rep(1, ncol(data)))
  } else {
    marginal_row <- c(paste0("EMM: ", iv2_name), rep("", ncol(data) - 1))
    ft <- flextable::add_footer_row(ft, values = marginal_row,
                                    colwidths = rep(1, ncol(data)))
  }

  return(ft)
}


#' Factorial Interaction Results Text
#'
#' Returns formatted text for the interaction test and LSD components,
#' either filled (answer key with red highlighting) or blank (worksheet).
#'
#' @param anova_results_list Output from [anova_factorial_answers()].
#' @param KEY Logical. If `TRUE`, show filled answers. If `FALSE`, show blanks.
#' @param highlight Logical. If `TRUE` and `KEY = TRUE`, wrap answers in
#'   highlight markup for Quarto/RMarkdown.
#'
#' @return A character string with markdown/HTML formatting.
#'
#' @examples
#' \dontrun{
#' data(superman_data)
#' sm <- superman_data
#' sm$era <- ifelse(sm$year >= 2000, 2, 1)
#' result <- anova_factorial_answers(sm, dv = "clark_height_in",
#'   iv1 = "clark_grp", iv2 = "era",
#'   iv1_labels = c("Under 6ft", "6ft+"),
#'   iv2_labels = c("Pre-2000", "Post-2000"))
#' cat(factorial_interaction_results(result))
#' }
#'
#' @export
factorial_interaction_results <- function(anova_results_list,
                                          KEY = TRUE,
                                          highlight = TRUE) {

  f_int <- anova_results_list$ANOVA$Interaction$F
  p_int <- anova_results_list$ANOVA$Interaction$p_value
  df_int <- anova_results_list$ANOVA$Interaction$df
  df_within <- anova_results_list$ANOVA$df_within
  mse <- anova_results_list$ANOVA$mse
  k <- anova_results_list$ANOVA$k
  mean_n <- anova_results_list$ANOVA$mean_n
  lsd_mmd <- anova_results_list$LSD$lsd_mmd

  hl <- function(text) {
    if (highlight && KEY) {
      paste0("[", text, "]{custom-style=\"highlight-yellow\"}")
    } else {
      as.character(text)
    }
  }

  if (KEY) {
    has_interaction <- ifelse(p_int < 0.05, "Yes", "No")

    output <- paste0(
      "Find the results of the test of the interaction:\n\n",
      '<p style="color: red;">',
      "F = ", hl(f_int), "     ",
      "df = ", hl(df_int), ", ", hl(df_within), "     ",
      "p = ", hl(sprintf("%.3f", p_int)), "     ",
      "MSe = ", hl(mse), "     ",
      "Is there an interaction ??? ", hl(has_interaction),
      "</p>\n\n",
      "Do we need an LSDmmd to compare cell means to describe the pattern of the interaction? ______ Why or why not?\n\n",
      "If necessary, find the components for the LSDmmd computation:\n\n",
      '<p style="color: red;">',
      "`# conditions` = ", hl(k), "     ",
      "n = ", hl(mean_n), "     ",
      "df error = ", hl(df_within), "     ",
      "MSe = ", hl(mse), "     ",
      "LSDmmd = ", hl(lsd_mmd),
      "</p>\n"
    )
  } else {
    output <- paste0(
      "## Find the results of the test of the interaction:\n\n",
      "F = ______     ",
      "df = ____, ____     ",
      "p = ______     ",
      "MSe = ______     ",
      "Is there an interaction ???\n\n",
      "Do we need an LSDmmd to compare cell means to describe the pattern of the interaction? ______ Why or why not?\n\n",
      "### If necessary, find the components for the LSDmmd computation:\n\n",
      "`# conditions` = ______     ",
      "n = ______     ",
      "df error = ______     ",
      "MSe = ______     ",
      "LSDmmd = ______\n"
    )
  }

  return(output)
}


#' Factorial Main Effect Results Text
#'
#' Returns formatted text for a main effect test with descriptive/misleading
#' evaluation, either filled (answer key) or blank (worksheet).
#'
#' @param anova_results_list Output from [anova_factorial_answers()].
#' @param iv_name Character. Display name for the IV being tested.
#' @param which_iv Character. `"IV1"` or `"IV2"` to select which main effect.
#' @param KEY Logical. If `TRUE`, show filled answers. If `FALSE`, show blanks.
#' @param highlight Logical. If `TRUE` and `KEY = TRUE`, wrap answers in
#'   highlight markup.
#'
#' @return A character string with markdown/HTML formatting.
#'
#' @examples
#' \dontrun{
#' data(superman_data)
#' sm <- superman_data
#' sm$era <- ifelse(sm$year >= 2000, 2, 1)
#' result <- anova_factorial_answers(sm, dv = "clark_height_in",
#'   iv1 = "clark_grp", iv2 = "era",
#'   iv1_labels = c("Under 6ft", "6ft+"),
#'   iv2_labels = c("Pre-2000", "Post-2000"))
#' cat(factorial_main_effect_results(result,
#'   iv_name = "Height Group", which_iv = "IV1"))
#' }
#'
#' @export
factorial_main_effect_results <- function(anova_results_list,
                                          iv_name = "IV",
                                          which_iv = "IV2",
                                          KEY = TRUE,
                                          highlight = TRUE) {

  # Extract appropriate stats
  if (which_iv == "IV1") {
    f_stat <- anova_results_list$ANOVA$MainEffect_IV1$F
    p_val <- anova_results_list$ANOVA$MainEffect_IV1$p_value
    df_between <- anova_results_list$ANOVA$MainEffect_IV1$df
  } else {
    f_stat <- anova_results_list$ANOVA$MainEffect_IV2$F
    p_val <- anova_results_list$ANOVA$MainEffect_IV2$p_value
    df_between <- anova_results_list$ANOVA$MainEffect_IV2$df
  }

  df_within <- anova_results_list$ANOVA$df_within
  mse <- anova_results_list$ANOVA$mse
  p_int <- anova_results_list$ANOVA$Interaction$p_value

  hl <- function(text) {
    if (highlight && KEY) {
      paste0("[", text, "]{custom-style=\"highlight-yellow\"}")
    } else {
      as.character(text)
    }
  }

  if (KEY) {
    has_main_effect <- ifelse(p_val < 0.05, "Yes", "No")

    # LSD for marginal means answer (always "no" for 2-level IV)
    lsd_marginal_answer <- "No, because this is a 2-level variable so we do not need an LSDmmd to determine the pattern of the main effect. The F-test already tells us the direction."

    # Descriptive or misleading determination
    if (p_int < 0.05) {
      desc_stats <- anova_results_list$Descriptives
      is_consistent <- .check_simple_effects_consistency(desc_stats, which_iv)

      if (is_consistent) {
        desc_misleading <- "Descriptive - although there is a significant interaction, all simple effects are in the same direction as the main effect (quantitative interaction), so the main effect accurately describes the general pattern."
      } else {
        desc_misleading <- "Misleading - there is a significant interaction with simple effects going in different directions (qualitative/crossover interaction), so the main effect does not accurately describe the pattern of cell means. The simple effects show the actual pattern."
      }
    } else {
      desc_misleading <- "Descriptive - there is no significant interaction, so the main effect accurately describes the pattern."
    }

    output <- paste0(
      "Find the results for the test of the Main effect of ", iv_name, "\n\n",
      '<p style="color: red;">',
      "F = ", hl(f_stat), "     ",
      "df = ", hl(df_between), ", ", hl(df_within), "     ",
      "p = ", hl(sprintf("%.3f", p_val)), "     ",
      "MSe = ", hl(mse), "     ",
      "Is there a main effect ??? ", hl(has_main_effect),
      "</p>\n\n",
      "Do we need an LSDmmd to compare marginal means to determine the pattern of the main effect? ______ Why or why not\n\n",
      '<p style="color: red;">', hl(lsd_marginal_answer), "</p>\n\n",
      "So, is the main effect of ", hl(iv_name), " descriptive or misleading?\n\n",
      '<p style="color: red;">', hl(desc_misleading), "</p>\n"
    )
  } else {
    output <- paste0(
      "Find the results for the test of the Main effect of ", hl(iv_name), "\n\n",
      "F = ______     ",
      "df = ____, ____     ",
      "p = ______     ",
      "MSe = ______     ",
      "Is there a main effect ???\n\n",
      "Do we need an LSDmmd to compare marginal means to determine the pattern of the main effect? ______ Why or why not\n\n",
      "So, is the main effect of ", hl(iv_name), " descriptive or misleading?\n"
    )
  }

  return(output)
}


#' Check Simple Effects Consistency (Internal Helper)
#'
#' Determines whether simple effects are all in the same direction
#' (quantitative/ordinal interaction) or in different directions
#' (qualitative/crossover interaction).
#'
#' @param desc_stats Descriptives tibble from [anova_factorial_answers()] results.
#' @param which_iv Character. `"IV1"` or `"IV2"` indicating which main effect
#'   to check consistency for.
#'
#' @return Logical. `TRUE` if simple effects are consistent (all same direction),
#'   `FALSE` if inconsistent (crossover).
#'
#' @noRd
.check_simple_effects_consistency <- function(desc_stats, which_iv) {
  iv1_levels <- unique(desc_stats$iv1_level)
  iv2_levels <- unique(desc_stats$iv2_level)

  if (which_iv == "IV1") {
    # Check main effect of IV1: look at simple effects across IV2 levels
    # For each level of IV2, see which IV1 level is higher
    directions <- c()
    for (j in seq_along(iv2_levels)) {
      cell1 <- desc_stats[desc_stats$iv1_level == iv1_levels[1] &
                            desc_stats$iv2_level == iv2_levels[j], ]
      cell2 <- desc_stats[desc_stats$iv1_level == iv1_levels[2] &
                            desc_stats$iv2_level == iv2_levels[j], ]
      if (nrow(cell1) > 0 && nrow(cell2) > 0) {
        directions <- c(directions, sign(cell1$mean[1] - cell2$mean[1]))
      }
    }
    return(length(unique(directions)) == 1 && unique(directions) != 0)
  } else {
    # Check main effect of IV2: look at simple effects across IV1 levels
    # For each level of IV1, see which IV2 level is higher
    directions <- c()
    for (i in seq_along(iv1_levels)) {
      cell1 <- desc_stats[desc_stats$iv1_level == iv1_levels[i] &
                            desc_stats$iv2_level == iv2_levels[1], ]
      cell2 <- desc_stats[desc_stats$iv1_level == iv1_levels[i] &
                            desc_stats$iv2_level == iv2_levels[2], ]
      if (nrow(cell1) > 0 && nrow(cell2) > 0) {
        directions <- c(directions, sign(cell1$mean[1] - cell2$mean[1]))
      }
    }
    return(length(unique(directions)) == 1 && unique(directions) != 0)
  }
}


#' Factorial APA Descriptive Statistics Table (Flextable)
#'
#' Creates an APA-formatted descriptive statistics table for a factorial
#' design showing cell means, SDs, and ns organized by IV1 and IV2 levels.
#'
#' @param anova_results_list Output from [anova_factorial_answers()].
#' @param iv1_name Character. Display name for IV1.
#' @param iv2_name Character. Display name for IV2.
#' @param dv_name Character. Display name for DV (used in caption).
#' @param Key Logical. If `TRUE`, fill with values; if `FALSE`, blank.
#' @param table_title Character or `NULL`. Optional table caption.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' \dontrun{
#' data(superman_data)
#' sm <- superman_data
#' sm$era <- ifelse(sm$year >= 2000, 2, 1)
#' result <- anova_factorial_answers(sm, dv = "clark_height_in",
#'   iv1 = "clark_grp", iv2 = "era",
#'   iv1_labels = c("Under 6ft", "6ft+"),
#'   iv2_labels = c("Pre-2000", "Post-2000"))
#' ft <- create_factorial_apa_desc_table(result,
#'   iv1_name = "Height Group", iv2_name = "Era",
#'   dv_name = "Height (inches)")
#' ft
#' }
#'
#' @export
create_factorial_apa_desc_table <- function(anova_results_list,
                                            iv1_name = "IV1",
                                            iv2_name = "IV2",
                                            dv_name = "DV",
                                            Key = TRUE,
                                            table_title = NULL) {

  if (!is.null(anova_results_list$FactorLevels)) {
    iv1_labels <- anova_results_list$FactorLevels$iv1_labels
    iv2_labels <- anova_results_list$FactorLevels$iv2_labels
    iv1_levels_actual <- anova_results_list$FactorLevels$iv1_levels
    iv2_levels_actual <- anova_results_list$FactorLevels$iv2_levels
  } else {
    stop("FactorLevels not found in results.")
  }

  desc_stats <- anova_results_list$Descriptives
  n_iv1 <- length(iv1_labels)
  n_iv2 <- length(iv2_labels)

  if (Key) {
    # Build rows: each IV1 level gets a row per IV2 level
    rows <- list()
    for (i in seq_len(n_iv1)) {
      for (j in seq_len(n_iv2)) {
        cell <- desc_stats[desc_stats$iv1_level == iv1_levels_actual[i] &
                             desc_stats$iv2_level == iv2_levels_actual[j], ]

        if (nrow(cell) > 0) {
          rows[[length(rows) + 1]] <- data.frame(
            IV1 = ifelse(j == 1, iv1_labels[i], ""),
            IV2 = iv2_labels[j],
            M = sprintf("%.2f", cell$mean[1]),
            SD = sprintf("%.2f", cell$sd[1]),
            n = as.character(cell$n[1]),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    table_data <- do.call(rbind, rows)

  } else {
    # Blank version
    rows <- list()
    for (i in seq_len(n_iv1)) {
      for (j in seq_len(n_iv2)) {
        rows[[length(rows) + 1]] <- data.frame(
          IV1 = ifelse(j == 1, iv1_labels[i], ""),
          IV2 = iv2_labels[j],
          M = "",
          SD = "",
          n = "",
          stringsAsFactors = FALSE
        )
      }
    }
    table_data <- do.call(rbind, rows)
  }

  names(table_data) <- c(iv1_name, iv2_name, "M", "SD", "n")

  if (is.null(table_title)) {
    table_title <- paste0("Descriptive Statistics for ", dv_name,
                          " by ", iv1_name, " and ", iv2_name)
  }

  apa_table <- table_data |>
    flextable::flextable() |>
    flextable::set_caption(caption = table_title) |>
    flextable::set_table_properties(layout = "autofit", align = "left") |>
    flextable::border_remove() |>
    flextable::hline_top(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "body", border = officer::fp_border(width = 2)) |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::align(j = 2, align = "left", part = "all") |>
    flextable::fontsize(size = 11, part = "all")

  # Add horizontal line between IV1 groups
  if (n_iv2 > 1) {
    for (i in seq_len(n_iv1 - 1)) {
      row_idx <- i * n_iv2
      apa_table <- flextable::hline(apa_table, i = row_idx, part = "body",
                                    border = officer::fp_border(width = 0.5))
    }
  }

  return(apa_table)
}


#' Format Factorial ANOVA Results for Fill-in-the-Blank
#'
#' Returns a formatted text string of all factorial ANOVA results including
#' interaction, both main effects, and LSD calculations.
#'
#' @param rh_name Character. Research hypothesis name.
#' @param anova_results_list Output from [anova_factorial_answers()].
#' @param iv1_name Character. Display name for IV1.
#' @param iv2_name Character. Display name for IV2.
#' @param dv_name Character. Display name for DV.
#' @param Key Logical. If `TRUE`, show answers; if `FALSE`, show blanks.
#'
#' @return A character string.
#'
#' @examples
#' \dontrun{
#' data(superman_data)
#' sm <- superman_data
#' sm$era <- ifelse(sm$year >= 2000, 2, 1)
#' result <- anova_factorial_answers(sm, dv = "clark_height_in",
#'   iv1 = "clark_grp", iv2 = "era",
#'   iv1_labels = c("Under 6ft", "6ft+"),
#'   iv2_labels = c("Pre-2000", "Post-2000"))
#' cat(format_factorial_results("RH1", result,
#'   iv1_name = "Height Group", iv2_name = "Era",
#'   dv_name = "Height (inches)"))
#' }
#'
#' @export
format_factorial_results <- function(rh_name, anova_results_list,
                                     iv1_name = "IV1", iv2_name = "IV2",
                                     dv_name = "DV", Key = TRUE) {

  anova <- anova_results_list$ANOVA
  desc_stats <- anova_results_list$Descriptives
  lsd <- anova_results_list$LSD

  if (!Key) {
    output <- paste0(
      "#### ", rh_name, " Results\n\n",
      "**Factorial ANOVA: ", iv1_name, " x ", iv2_name, " on ", dv_name, "**\n\n",
      "**Interaction (", iv1_name, " x ", iv2_name, "):**\n",
      "  F = ___    df = ___, ___    p = ___    MSE = ___\n\n",
      "**Main Effect of ", iv1_name, ":**\n",
      "  F = ___    df = ___, ___    p = ___\n\n",
      "**Main Effect of ", iv2_name, ":**\n",
      "  F = ___    df = ___, ___    p = ___\n\n",
      "**LSD Components:**\n",
      "  k = ___    n = ___    df error = ___    MSe = ___    LSDmmd = ___\n\n",
      "**Cell Means:**\n"
    )

    # Add blank cell means
    if (!is.null(anova_results_list$FactorLevels)) {
      iv1_labels <- anova_results_list$FactorLevels$iv1_labels
      iv2_labels <- anova_results_list$FactorLevels$iv2_labels
      for (i in seq_along(iv1_labels)) {
        for (j in seq_along(iv2_labels)) {
          output <- paste0(output,
                           "  ", iv1_labels[i], " x ", iv2_labels[j], ":    M = ___    SD = ___    n = ___\n")
        }
      }
    }

  } else {
    # Format p-values
    fmt_p <- function(p) {
      if (p < 0.001) "< .001" else sprintf("%.3f", p)
    }

    output <- paste0(
      "#### ", rh_name, " Results\n\n",
      "**Factorial ANOVA: ", iv1_name, " x ", iv2_name, " on ", dv_name, "**\n\n",
      "**Interaction (", iv1_name, " x ", iv2_name, "):**\n",
      "  F = ", anova$Interaction$F,
      "    df = ", anova$Interaction$df, ", ", anova$df_within,
      "    p = ", fmt_p(anova$Interaction$p_value),
      "    MSE = ", anova$mse, "\n\n",
      "**Main Effect of ", iv1_name, ":**\n",
      "  F = ", anova$MainEffect_IV1$F,
      "    df = ", anova$MainEffect_IV1$df, ", ", anova$df_within,
      "    p = ", fmt_p(anova$MainEffect_IV1$p_value), "\n\n",
      "**Main Effect of ", iv2_name, ":**\n",
      "  F = ", anova$MainEffect_IV2$F,
      "    df = ", anova$MainEffect_IV2$df, ", ", anova$df_within,
      "    p = ", fmt_p(anova$MainEffect_IV2$p_value), "\n\n",
      "**LSD Components:**\n",
      "  k = ", anova$k,
      "    n = ", anova$mean_n,
      "    df error = ", anova$df_within,
      "    MSe = ", anova$mse,
      "    LSDmmd = ", lsd$lsd_mmd, "\n\n",
      "**Cell Means:**\n"
    )

    # Add filled cell means
    for (i in seq_len(nrow(desc_stats))) {
      output <- paste0(output,
                       "  ", desc_stats$group_label[i],
                       ":    M = ", desc_stats$mean[i],
                       "    SD = ", desc_stats$sd[i],
                       "    n = ", desc_stats$n[i], "\n")
    }

    # Add EMMs
    emm_iv1 <- anova_results_list$EMMs$IV1
    emm_iv2 <- anova_results_list$EMMs$IV2

    output <- paste0(output, "\n**Estimated Marginal Means:**\n")
    output <- paste0(output, "  ", iv1_name, ":\n")
    for (i in seq_len(nrow(emm_iv1))) {
      output <- paste0(output, "    ", emm_iv1$iv1_label[i],
                       ": M = ", round(emm_iv1$mean[i], 2), "\n")
    }
    output <- paste0(output, "  ", iv2_name, ":\n")
    for (i in seq_len(nrow(emm_iv2))) {
      output <- paste0(output, "    ", emm_iv2$iv2_label[i],
                       ": M = ", round(emm_iv2$mean[i], 2), "\n")
    }
  }

  return(output)
}
