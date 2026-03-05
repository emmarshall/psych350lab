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
# FACTORIAL ANOVA FUNCTION
# =============================================================================

#' Factorial (Two-Way) ANOVA
#'
#' Performs a 2x2 factorial ANOVA using the `jmv` package, returning main effects,
#' interaction, estimated marginal means, descriptive statistics for all cells,
#' and LSD/HSD post-hoc values. All numeric values stored unrounded.
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
#' @return A list with elements (all numeric values unrounded).
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

  # Extract ANOVA statistics - NO ROUNDING
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

  # Calculate cell descriptives - NO ROUNDING
  analysis_df <- analysis_df |>
    dplyr::mutate(cell = interaction(iv1, iv2))

  desc_stats <- analysis_df |>
    dplyr::summarise(
      mean = mean(dv, na.rm = TRUE),
      sd = stats::sd(dv, na.rm = TRUE),
      n = dplyr::n(),
      .by = cell
    ) |>
    dplyr::mutate(
      sem = sd / sqrt(n),
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

  # Calculate weighted EMMs - NO ROUNDING
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

  # Convert to tibbles for output
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

  # Calculate LSD/HSD parameters - NO ROUNDING
  total_n <- nrow(analysis_df)
  k <- nrow(desc_stats)
  mean_n <- total_n / k

  lsd_hsd_results <- lsd_hsd_calculator(
    k = k,
    n_per_group = mean_n,
    mse = mse,
    df_error_input = df_within
  )

  # Build results list - NO ROUNDING
  results_list <- list(
    ANOVA = list(
      MainEffect_IV1 = list(
        F = f_iv1,
        p_value = p_iv1,
        df = df_iv1
      ),
      MainEffect_IV2 = list(
        F = f_iv2,
        p_value = p_iv2,
        df = df_iv2
      ),
      Interaction = list(
        F = f_interaction,
        p_value = p_interaction,
        df = df_interaction
      ),
      df_within = df_within,
      mse = mse,
      total_n = total_n,
      k = k,
      mean_n = mean_n
    ),
    Descriptives = desc_stats,
    EMMs = list(IV1 = emm_iv1, IV2 = emm_iv2),
    LSD = list(
      lsd_mmd = lsd_hsd_results$lsd,
      hsd_mmd = lsd_hsd_results$hsd,
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
