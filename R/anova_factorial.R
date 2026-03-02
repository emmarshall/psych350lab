# =============================================================================
# anova_factorial.R
# Factorial (Two-Way) ANOVA Analysis Functions
# =============================================================================
# Performs 2x2 factorial ANOVA using jmv package, returning main effects,
# interaction, estimated marginal means, descriptive statistics, and
# LSD/HSD post-hoc values.
#
# Updated for dplyr 1.2.0 (February 2026)
# =============================================================================


# -----------------------------------------------------------------------------
# INTERNAL HELPERS
# -----------------------------------------------------------------------------

#' Create factor with appropriate level ordering
#' @noRd
.create_ordered_factor <- function(x, explicit_levels = NULL) {
  if (!is.null(explicit_levels)) {
    return(factor(x, levels = explicit_levels))
  }

  unique_vals <- unique(x[!is.na(x)])
  if (is.numeric(unique_vals)) {
    factor(x, levels = sort(unique_vals))
  } else {
    factor(x)
  }
}


#' Format p-value for display
#' @noRd
.format_p_factorial <- function(p_value) {
  if (p_value < 0.001) "< .001" else sprintf("%.3f", p_value)
}


# =============================================================================
# MAIN FACTORIAL ANOVA FUNCTION
# =============================================================================

#' Factorial (Two-Way) ANOVA
#'
#' Performs a 2x2 factorial ANOVA using the `jmv` package, returning main effects,
#' interaction, estimated marginal means, descriptive statistics for all cells,
#' and LSD/HSD post-hoc values.
#'
#' @param data A data frame or tibble.
#' @param dv Character string. Name of the dependent variable.
#' @param iv1 Character string. Name of the first independent variable.
#' @param iv2 Character string. Name of the second independent variable.
#' @param iv1_labels Character vector. Labels for levels of IV1.
#' @param iv2_labels Character vector. Labels for levels of IV2.
#' @param iv1_levels Numeric or character vector. Explicit ordering of IV1 levels.
#' @param iv2_levels Numeric or character vector. Explicit ordering of IV2 levels.
#' @param interaction_label Character string. Optional label for the interaction.
#'
#' @return A list with elements:
#' \describe{
#'   \item{ANOVA}{Main effects, interaction F-tests, df, MSE, etc.}
#'   \item{Descriptives}{Cell means, SDs, and ns with labels.}
#'   \item{EMMs}{Estimated marginal means for IV1 and IV2.}
#'   \item{LSD}{LSD and HSD minimum mean difference values.}
#'   \item{Pairwise}{Reserved for pairwise comparison results.}
#'   \item{FactorLevels}{Level ordering and label mappings.}
#' }
#'
#' @examples
#' data(superman)
#' # Create a binary predictor for demonstration
#' sm <- superman
#' sm$era <- ifelse(sm$year >= 2000, 2, 1)
#'
#' \dontrun{
#' result <- anova_factorial_answers(
#'   data = sm,
#'   dv = "clark_height_in",
#'   iv1 = "clark_grp",
#'   iv2 = "era",
#'   iv1_labels = c("Under 6ft", "6ft+"),
#'   iv2_labels = c("Pre-2000", "Post-2000")
#' )
#' result$ANOVA
#' result$Descriptives
#' }
#'
#' @export
anova_factorial_answers <- function(data, dv, iv1, iv2,
                                    iv1_labels = NULL, iv2_labels = NULL,
                                    iv1_levels = NULL, iv2_levels = NULL,
                                    interaction_label = NULL) {

  # Extract and convert DV
  dv_vector <- as.numeric(data[[dv]])

  # Create ordered factors using helper
  iv1_vector <- .create_ordered_factor(data[[iv1]], iv1_levels)
  iv2_vector <- .create_ordered_factor(data[[iv2]], iv2_levels)

  # Build analysis data frame and filter complete cases
  analysis_df <- tibble::tibble(
    dv = dv_vector,
    iv1 = iv1_vector,
    iv2 = iv2_vector
  ) |>
    dplyr::filter(!is.na(dv), !is.na(iv1), !is.na(iv2))

  # Run ANOVA via jmv
  anova_jmv <- jmv::ANOVA(
    data = analysis_df,
    dep = "dv",
    factors = c("iv1", "iv2"),
    emMeans = list(list("iv1"), list("iv2")),
    emmPlots = FALSE,
    emmTables = TRUE
  )

  anova_table <- anova_jmv$main$asDF

  # Extract ANOVA statistics using filter_out for cleaner selection
  # Note: Using base R extraction here as it's cleaner for single-row lookups
  f_iv1 <- anova_table$`F`[anova_table$name == "iv1"]
  p_iv1 <- anova_table$`p`[anova_table$name == "iv1"]
  df_iv1 <- anova_table$`df`[anova_table$name == "iv1"]

  f_iv2 <- anova_table$`F`[anova_table$name == "iv2"]
  p_iv2 <- anova_table$`p`[anova_table$name == "iv2"]
  df_iv2 <- anova_table$`df`[anova_table$name == "iv2"]

  f_interaction <- anova_table$`F`[anova_table$name == "iv1:iv2"]
  p_interaction <- anova_table$`p`[anova_table$name == "iv1:iv2"]
  df_interaction <- anova_table$`df`[anova_table$name == "iv1:iv2"]

  df_within <- anova_table$`df`[anova_table$name == "Residuals"]
  mse <- anova_table$`ss`[anova_table$name == "Residuals"] / df_within

  # Calculate cell descriptives using modern dplyr patterns
  analysis_df <- analysis_df |>
    dplyr::mutate(cell = interaction(iv1, iv2))

  # Use .by for per-operation grouping (dplyr 1.1+)
  desc_stats <- analysis_df |>
    dplyr::summarise(
      mean = round(mean(dv, na.rm = TRUE), 2),
      sd = round(stats::sd(dv, na.rm = TRUE), 2),
      n = dplyr::n(),
      .by = cell
    ) |>
    dplyr::mutate(
      iv1_level = sub("\\..*", "", cell),
      iv2_level = sub(".*\\.", "", cell)
    )

  # Get actual factor levels
  iv1_levels_actual <- levels(analysis_df$iv1)
  iv2_levels_actual <- levels(analysis_df$iv2)

  # Apply labels if provided
  if (!is.null(iv1_labels) && !is.null(iv2_labels)) {
    desc_stats <- desc_stats |>
      dplyr::mutate(
        iv1_label = iv1_labels[match(iv1_level, iv1_levels_actual)],
        iv2_label = iv2_labels[match(iv2_level, iv2_levels_actual)],
        group_label = paste(iv1_label, iv2_label, sep = " x ")
      )
  } else {
    desc_stats <- desc_stats |>
      dplyr::mutate(
        iv1_label = iv1_level,
        iv2_label = iv2_level,
        group_label = as.character(cell)
      )
  }

  # Calculate weighted EMMs using .by (dplyr 1.1+)
  emm_iv1_data <- desc_stats |>
    dplyr::summarise(
      mean = sum(mean * n) / sum(n),
      se = sqrt(mse / sum(n)),
      iv1_label = dplyr::first(iv1_label),
      .by = iv1_level
    ) |>
    dplyr::arrange(match(iv1_level, iv1_levels_actual))

  emm_iv2_data <- desc_stats |>
    dplyr::summarise(
      mean = sum(mean * n) / sum(n),
      se = sqrt(mse / sum(n)),
      iv2_label = dplyr::first(iv2_label),
      .by = iv2_level
    ) |>
    dplyr::arrange(match(iv2_level, iv2_levels_actual))

  # Convert to data frames for output
  emm_iv1 <- tibble::tibble(
    iv1 = emm_iv1_data$iv1_level,
    mean = emm_iv1_data$mean,
    se = emm_iv1_data$se,
    iv1_label = emm_iv1_data$iv1_label
  )

  emm_iv2 <- tibble::tibble(
    iv2 = emm_iv2_data$iv2_level,
    mean = emm_iv2_data$mean,
    se = emm_iv2_data$se,
    iv2_label = emm_iv2_data$iv2_label
  )

  # Calculate LSD/HSD parameters
  total_n <- nrow(analysis_df)
  k <- nrow(desc_stats)
  mean_n <- total_n / k

  lsd_hsd_results <- lsd_hsd_calculator(
    k = k,
    n_per_group = mean_n,
    mse = mse,
    df_error_input = df_within
  )

  # Build results list
  results_list <- list(
    ANOVA = list(
      MainEffect_IV1 = list(
        F = round(f_iv1, 2),
        p_value = round(p_iv1, 3),
        df = df_iv1
      ),
      MainEffect_IV2 = list(
        F = round(f_iv2, 2),
        p_value = round(p_iv2, 3),
        df = df_iv2
      ),
      Interaction = list(
        F = round(f_interaction, 2),
        p_value = round(p_interaction, 3),
        df = df_interaction
      ),
      df_within = df_within,
      mse = round(mse, 2),
      total_n = total_n,
      k = k,
      mean_n = round(mean_n, 2)
    ),
    Descriptives = desc_stats,
    EMMs = list(IV1 = emm_iv1, IV2 = emm_iv2),
    LSD = list(
      lsd_mmd = round(lsd_hsd_results$lsd, 2),
      hsd_mmd = round(lsd_hsd_results$hsd, 2),
      need_posthoc = p_interaction < 0.05,
      parameters_used = lsd_hsd_results$parameters_used
    ),
    Pairwise = list(),
    FactorLevels = list(
      iv1_levels = iv1_levels_actual,
      iv2_levels = iv2_levels_actual,
      iv1_labels = if (!is.null(iv1_labels)) iv1_labels else iv1_levels_actual,
      iv2_labels = if (!is.null(iv2_labels)) iv2_labels else iv2_levels_actual
    )
  )

  invisible(results_list)
}


# =============================================================================
# LSD/HSD CALCULATOR
# =============================================================================

#' LSD/HSD Post-Hoc Calculator
#'
#' Calculates LSD and HSD minimum mean differences using a studentized range table.
#'
#' @param k Integer. Number of groups (3-6).
#' @param n_per_group Numeric. Average n per group.
#' @param mse Numeric. Mean square error from the ANOVA.
#' @param df_error_input Numeric. Error degrees of freedom.
#'
#' @return A list with `lsd`, `hsd`, and `parameters_used`.
#'
#' @examples
#' lsd_hsd_calculator(k = 4, n_per_group = 25, mse = 10.5, df_error_input = 96)
#'
#' @export
lsd_hsd_calculator <- function(k, n_per_group, mse, df_error_input) {

  # Studentized range table (stored as tibble for clarity)
  studentized_range_table <- tibble::tibble(
    df = c(5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 90, 120, 200, 999),
    tcrit_p.05 = c(2.57, 2.23, 2.13, 2.09, 2.06, 2.04, 2.03, 2.02, 2.01, 2.00, 2.01, 1.98, 1.97, 1.96),
    k3 = c(4.60, 3.88, 3.67, 3.58, 3.52, 3.49, 3.47, 3.44, 3.42, 3.40, 3.37, 3.36, 3.34, 3.31),
    k4 = c(5.22, 4.33, 4.08, 3.96, 3.89, 3.85, 3.83, 3.79, 3.76, 3.74, 3.71, 3.68, 3.67, 3.63),
    k5 = c(5.67, 4.65, 4.37, 4.23, 4.15, 4.10, 4.08, 4.04, 4.01, 3.98, 3.95, 3.92, 3.90, 3.86),
    k6 = c(6.03, 4.91, 4.59, 4.45, 4.36, 4.30, 4.28, 4.23, 4.20, 4.16, 4.13, 4.10, 4.08, 4.03)
  )

  # Find closest df row
  closest_df_index <- which.min(abs(studentized_range_table$df - df_error_input))

  # Get q-value for the appropriate k
  k_col <- paste0("k", k)
  if (!k_col %in% names(studentized_range_table)) {
    stop("k must be between 3 and 6 for this implementation")
  }

  q_value <- studentized_range_table[[k_col]][closest_df_index]
  t_crit <- studentized_range_table$tcrit_p.05[closest_df_index]

  # Calculate LSD and HSD
  lsd <- t_crit * sqrt(2 * mse / n_per_group)
  hsd <- (q_value / sqrt(2)) * sqrt(mse / n_per_group)

  list(
    lsd = lsd,
    hsd = hsd,
    parameters_used = list(
      closest_df = studentized_range_table$df[closest_df_index],
      q_value = q_value,
      t_crit = t_crit
    )
  )
}


# =============================================================================
# DIAGNOSTIC FUNCTION
# =============================================================================

#' Check Factor Level Alignment (Diagnostic)
#'
#' Prints diagnostic information about factor level ordering, EMMs,
#' and cell means from a factorial ANOVA result.
#'
#' @param anova_results_list Output from [anova_factorial_answers()].
#'
#' @examples
#' \dontrun{
#' check_factor_alignment(result)
#' }
#'
#' @export
check_factor_alignment <- function(anova_results_list) {
  cat("=== Factor Level Alignment Check ===\n\n")

  if (!is.null(anova_results_list$FactorLevels)) {
    cat("IV1 Levels (in order):\n")
    for (i in seq_along(anova_results_list$FactorLevels$iv1_levels)) {
      cat(sprintf("  %d. Level: %s  ->  Label: %s\n",
                  i,
                  anova_results_list$FactorLevels$iv1_levels[i],
                  anova_results_list$FactorLevels$iv1_labels[i]))
    }

    cat("\nIV2 Levels (in order):\n")
    for (i in seq_along(anova_results_list$FactorLevels$iv2_levels)) {
      cat(sprintf("  %d. Level: %s  ->  Label: %s\n",
                  i,
                  anova_results_list$FactorLevels$iv2_levels[i],
                  anova_results_list$FactorLevels$iv2_labels[i]))
    }
  }

  cat("\n=== Cell Descriptives ===\n")
  print(anova_results_list$Descriptives)

  cat("\n=== EMM IV1 ===\n")
  print(anova_results_list$EMMs$IV1)

  cat("\n=== EMM IV2 ===\n")
  print(anova_results_list$EMMs$IV2)

  invisible(NULL)
}
