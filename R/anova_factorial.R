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
# BG FACTORIAL ANOVA FUNCTION
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
#' @examples
#' \dontrun{
#' library(psych350data)
#' # Between-groups factorial ANOVA: clark_grp x tomatometer predicting rt_critics_score
#' bg_results <- anova_factorial_answers(
#'   data = superman,
#'   dv = "rt_critics_score",
#'   iv1 = "clark_grp",
#'   iv2 = "tomatometer",
#'   iv1_labels = c("Under 6ft", "6ft or taller"),
#'   iv2_labels = c("Rotten", "Fresh")
#' )
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

# =============================================================================
# anova_factmg.R
# Mixed Groups Factorial (2x2) ANOVA Analysis Function
# =============================================================================
# Performs 2x2 mixed factorial ANOVA (one BG factor, one WG factor) using jmv
# package, returning within-subjects effects, between-subjects effects,
# interaction, descriptive statistics, and estimated marginal means.
#
# Updated for dplyr 1.2.0 (February 2026)
# =============================================================================

# =============================================================================
# MIXED GROUPS FACTORIAL ANOVA FUNCTION
# =============================================================================

#' Mixed Groups Factorial (2x2) ANOVA
#'
#' Performs a 2x2 mixed factorial ANOVA using the `jmv` package, with one
#' between-groups factor and one within-groups factor. Returns within-subjects
#' effects (WG main effect, BG x WG interaction), between-subjects effects
#' (BG main effect), descriptive statistics for all cells, and estimated
#' marginal means. All numeric values stored unrounded.
#'
#' @param data A data frame in wide format (one row per subject).
#' @param dv_vars Character vector of length 2. Names of the DV columns
#'   (one per WG condition).
#' @param bg_iv Character string. Name of the between-groups IV column.
#' @param wg_name Character string. Label for the within-groups factor
#'   (default: "Measure").
#' @param wg_labels Character vector or NULL. Display labels for the WG
#'   conditions. Must be same length as `dv_vars`. Defaults to `dv_vars`.
#' @param bg_labels Character vector or NULL. Display labels for BG levels.
#' @param bg_levels Numeric or character vector or NULL. Explicit ordering of
#'   BG factor levels.
#'
#' @return A list with elements (all numeric values unrounded):
#' \describe{
#'   \item{WithinSubjects}{List with `MainEffect_WG`, `Interaction`, and
#'     `Error`, each containing `F`, `p_value`, `df`, `ss`, `ms` as applicable.}
#'   \item{BetweenSubjects}{List with `MainEffect_BG` and `Error`, each
#'     containing `F`, `p_value`, `df`, `ss`, `ms` as applicable.}
#'   \item{Descriptives}{Tibble with cell-level descriptive statistics including
#'     `bg_level`, `wg_level`, `mean`, `sd`, `n`, `sem`, `bg_label`,
#'     `wg_label`.}
#'   \item{EMMs}{List with `BG` and `WG` tibbles of estimated marginal means.}
#'   \item{FactorInfo}{List with `wg_name`, `wg_labels`, `dv_vars`,
#'     `bg_levels`, `bg_labels`, `n_obs`, `group_ns`.}
#' }
#'
#' @examples
#' \dontrun{
#' library(psych350data)
#' # Mixed factorial ANOVA: clark_grp (BG) x two repeated measures (WG)
#' # For example, rt_critics_score measured under two conditions
#' mg_results <- anova_factmg_answers(
#'   data = superman,
#'   dv_vars = c("rt_critics_score", "rt_audience_score"),
#'   bg_iv = "clark_grp",
#'   wg_name = "Rating Source",
#'   wg_labels = c("Critics", "Audience"),
#'   bg_labels = c("Under 6ft", "6ft or taller")
#' )
#' }
#'
#' @export
anova_factmg_answers <- function(data, dv_vars, bg_iv,
                                 wg_name = "Measure",
                                 wg_labels = NULL,
                                 bg_labels = NULL,
                                 bg_levels = NULL) {

  n_cond <- length(dv_vars)
  if (is.null(wg_labels)) wg_labels <- dv_vars

  # Create BG factor
  bg_vector <- .create_ordered_factor(data[[bg_iv]], bg_levels)

  # Build analysis data frame
  analysis_df <- as.data.frame(data[, dv_vars, drop = FALSE])

  analysis_df$bg <- bg_vector
  analysis_df <- analysis_df[stats::complete.cases(analysis_df), ]

  n_obs <- nrow(analysis_df)
  bg_levels_actual <- levels(analysis_df$bg)
  group_ns <- table(analysis_df$bg)

  # ---------------------------------------------------------------------------
  # Run mixed ANOVA via jmv::anovaRM
  # ---------------------------------------------------------------------------
  rm_spec <- list(list(
    label = wg_name,
    levels = wg_labels
  ))

  rm_cells <- lapply(seq_along(dv_vars), function(i) {
    list(measure = dv_vars[i], cell = wg_labels[i])
  })

  anova_result <- jmv::anovaRM(
    data       = analysis_df,
    rm         = rm_spec,
    rmCells    = rm_cells,
    bs         = "bg",
    spherTests = FALSE,
    spherCorr  = list("none"),
    effectSize = list(),
    emmPlots   = FALSE,
    emmTables  = FALSE
  )

  # ---------------------------------------------------------------------------
  # Extract within-subjects effects
  # ---------------------------------------------------------------------------
  within_df <- anova_result$rmTable$asDF

  # Column names use [none] suffix for sphericity correction = none
  # Find the right column names dynamically
  ss_col <- grep("^ss", names(within_df), value = TRUE)[1]
  df_col <- grep("^df", names(within_df), value = TRUE)[1]
  ms_col <- grep("^ms", names(within_df), value = TRUE)[1]
  f_col  <- grep("^F",  names(within_df), value = TRUE)[1]
  p_col  <- grep("^p",  names(within_df), value = TRUE)[1]

  # WG main effect
  wg_row <- within_df[within_df$name == wg_name, ]
  f_wg   <- wg_row[[f_col]]
  p_wg   <- wg_row[[p_col]]
  df_wg  <- wg_row[[df_col]]
  ss_wg  <- wg_row[[ss_col]]
  ms_wg  <- wg_row[[ms_col]]

  # Interaction (BG x WG)
  ix_row <- within_df[grepl(":", within_df$name), ]
  f_ix   <- ix_row[[f_col]]
  p_ix   <- ix_row[[p_col]]
  df_ix  <- ix_row[[df_col]]
  ss_ix  <- ix_row[[ss_col]]
  ms_ix  <- ix_row[[ms_col]]

  # Within-subjects error
  err_w_row <- within_df[grepl("Residual", within_df$name, ignore.case = TRUE), ]
  ss_err_w  <- err_w_row[[ss_col]]
  df_err_w  <- err_w_row[[df_col]]
  ms_err_w  <- err_w_row[[ms_col]]

  # ---------------------------------------------------------------------------
  # Extract between-subjects effects
  # ---------------------------------------------------------------------------
  between_df <- anova_result$bsTable$asDF

  bg_row <- between_df[between_df$name == "bg", ]
  f_bg   <- bg_row[["F"]]
  p_bg   <- bg_row[["p"]]
  df_bg  <- bg_row[["df"]]
  ss_bg  <- bg_row[["ss"]]
  ms_bg  <- bg_row[["ms"]]

  err_b_row <- between_df[grepl("Residual", between_df$name, ignore.case = TRUE), ]
  ss_err_b  <- err_b_row[["ss"]]
  df_err_b  <- err_b_row[["df"]]
  ms_err_b  <- err_b_row[["ms"]]

  # ---------------------------------------------------------------------------
  # Cell descriptives - NO ROUNDING
  # ---------------------------------------------------------------------------
  desc_rows <- list()
  for (bg_lev in bg_levels_actual) {
    bg_subset <- analysis_df[analysis_df$bg == bg_lev, ]
    for (i in seq_along(dv_vars)) {
      vals <- bg_subset[[dv_vars[i]]]
      vals <- vals[!is.na(vals)]
      desc_rows <- c(desc_rows, list(tibble::tibble(
        bg_level = bg_lev,
        wg_level = wg_labels[i],
        dv_var   = dv_vars[i],
        mean     = mean(vals),
        sd       = stats::sd(vals),
        n        = length(vals),
        sem      = stats::sd(vals) / sqrt(length(vals))
      )))
    }
  }
  desc_stats <- dplyr::bind_rows(desc_rows)

  # Apply labels
  if (!is.null(bg_labels)) {
    desc_stats$bg_label <- bg_labels[match(desc_stats$bg_level, bg_levels_actual)]
  } else {
    desc_stats$bg_label <- desc_stats$bg_level
  }
  desc_stats$wg_label <- desc_stats$wg_level

  # ---------------------------------------------------------------------------
  # Estimated marginal means - NO ROUNDING
  # ---------------------------------------------------------------------------
  emm_bg <- desc_stats |>
    dplyr::summarise(
      mean = sum(mean * n) / sum(n),
      se   = sqrt(ms_err_b / sum(n)),
      bg_label = dplyr::first(bg_label),
      .by = bg_level
    ) |>
    dplyr::arrange(match(bg_level, bg_levels_actual))

  emm_wg <- desc_stats |>
    dplyr::summarise(
      mean = sum(mean * n) / sum(n),
      se   = sqrt(ms_err_w / sum(n)),
      wg_label = dplyr::first(wg_label),
      .by = wg_level
    )

  # ---------------------------------------------------------------------------
  # Build results list - NO ROUNDING
  # ---------------------------------------------------------------------------
  results_list <- list(
    WithinSubjects = list(
      MainEffect_WG = list(
        F       = f_wg,
        p_value = p_wg,
        df      = df_wg,
        ss      = ss_wg,
        ms      = ms_wg
      ),
      Interaction = list(
        F       = f_ix,
        p_value = p_ix,
        df      = df_ix,
        ss      = ss_ix,
        ms      = ms_ix
      ),
      Error = list(
        ss = ss_err_w,
        df = df_err_w,
        ms = ms_err_w
      )
    ),
    BetweenSubjects = list(
      MainEffect_BG = list(
        F       = f_bg,
        p_value = p_bg,
        df      = df_bg,
        ss      = ss_bg,
        ms      = ms_bg
      ),
      Error = list(
        ss = ss_err_b,
        df = df_err_b,
        ms = ms_err_b
      )
    ),
    Descriptives = desc_stats,
    EMMs = list(
      BG = emm_bg,
      WG = emm_wg
    ),
    FactorInfo = list(
      wg_name    = wg_name,
      wg_labels  = wg_labels,
      dv_vars    = dv_vars,
      bg_levels  = bg_levels_actual,
      bg_labels  = if (!is.null(bg_labels)) bg_labels else bg_levels_actual,
      n_obs      = n_obs,
      group_ns   = group_ns
    )
  )

  invisible(results_list)
}

# =============================================================================
# anova_factwg.R
# Within Groups Factorial ANOVA Analysis Function
# =============================================================================
# Performs a within-groups factorial ANOVA (two WG factors, any k x j design)
# using jmv package, returning within-subjects effects for both main effects
# and the interaction (each with their own error term), descriptive statistics,
# and estimated marginal means.
#
# Updated for dplyr 1.2.0 (February 2026)
# =============================================================================

# =============================================================================
# WITHIN GROUPS FACTORIAL ANOVA FUNCTION
# =============================================================================

#' Within Groups Factorial ANOVA
#'
#' Performs a within-groups factorial ANOVA using the `jmv` package, with
#' two repeated-measures factors. Supports any k x j design (not limited
#' to 2x2). Returns within-subjects effects for both main effects and the
#' interaction (each with separate error terms), descriptive statistics for
#' all cells, and estimated marginal means. All numeric values stored
#' unrounded.
#'
#' Data must be in wide format with one column per cell of the k x j design
#' (k * j columns total). The columns should be ordered to match the crossing
#' of IV1 levels x IV2 levels, with IV2 varying fastest:
#'
#' For a 2x3 design (IV1 has 2 levels, IV2 has 3 levels):
#'   Column 1 = IV1_Level1 + IV2_Level1
#'   Column 2 = IV1_Level1 + IV2_Level2
#'   Column 3 = IV1_Level1 + IV2_Level3
#'   Column 4 = IV1_Level2 + IV2_Level1
#'   Column 5 = IV1_Level2 + IV2_Level2
#'   Column 6 = IV1_Level2 + IV2_Level3
#'
#' @param data A data frame in wide format (one row per subject).
#' @param dv_vars Character vector. Names of the DV columns, ordered as
#'   described above. Must have length = length(iv1_labels) * length(iv2_labels).
#' @param iv1_name Character string. Label for the first WG factor
#'   (default: "Factor1").
#' @param iv2_name Character string. Label for the second WG factor
#'   (default: "Factor2").
#' @param iv1_labels Character vector or NULL. Display labels for the levels
#'   of IV1. Defaults to c("Level1", "Level2").
#' @param iv2_labels Character vector or NULL. Display labels for the levels
#'   of IV2. Defaults to c("Level1", "Level2").
#'
#' @return A list with elements (all numeric values unrounded):
#' \describe{
#'   \item{ANOVA}{List with `MainEffect_IV1`, `MainEffect_IV2`, `Interaction`,
#'     each containing `F`, `p_value`, `df`, `ss`, `ms`. Also `Error_IV1`,
#'     `Error_IV2`, `Error_Interaction` each with `ss`, `df`, `ms`.
#'     Plus `n_obs`.}
#'   \item{Descriptives}{Tibble with cell-level descriptive statistics including
#'     `iv1_level`, `iv2_level`, `dv_var`, `mean`, `sd`, `n`, `sem`,
#'     `iv1_label`, `iv2_label`.}
#'   \item{EMMs}{List with `IV1` and `IV2` tibbles of estimated marginal means.}
#'   \item{FactorInfo}{List with `iv1_name`, `iv2_name`, `iv1_labels`,
#'     `iv2_labels`, `dv_vars`, `n_obs`.}
#' }
#'
#' @examples
#' \dontrun{
#' library(psych350data)
#' # Within-groups factorial ANOVA: height_gap x age_grp (both repeated measures)
#' # This requires data in wide format with 6 columns (3 x 2 design)
#' wg_results <- anova_factwg_answers(
#'   data = superman_smes,
#'   dv_vars = c("emotional_impact_min_min", "emotional_impact_min_avg",
#'               "emotional_impact_avg_min", "emotional_impact_avg_avg",
#'               "emotional_impact_big_min", "emotional_impact_big_avg"),
#'   iv1_name = "Height Gap",
#'   iv2_name = "Age Group",
#'   iv1_labels = c("Minimal", "Average", "Big"),
#'   iv2_labels = c("Minimal", "Average")
#' )
#' }
#'
#' @export
anova_factwg_answers <- function(data, dv_vars,
                                 iv1_name = "Factor1",
                                 iv2_name = "Factor2",
                                 iv1_labels = NULL,
                                 iv2_labels = NULL) {

  if (is.null(iv1_labels)) iv1_labels <- c("Level1", "Level2")
  if (is.null(iv2_labels)) iv2_labels <- c("Level1", "Level2")

  n_iv1 <- length(iv1_labels)
  n_iv2 <- length(iv2_labels)
  expected_cols <- n_iv1 * n_iv2

  if (length(dv_vars) != expected_cols) {
    stop(
      "dv_vars must have exactly ", expected_cols, " elements for a ",
      n_iv1, "x", n_iv2, " within-groups design, but got ", length(dv_vars)
    )
  }

  # Build analysis data frame and filter complete cases
  analysis_df <- as.data.frame(data[, dv_vars, drop = FALSE])

  analysis_df <- analysis_df[stats::complete.cases(analysis_df), ]
  n_obs <- nrow(analysis_df)

  # ---------------------------------------------------------------------------
  # Build cell mapping: dv_vars -> IV1 x IV2 crossing
  # IV2 varies fastest (columns ordered IV1_L1:IV2_L1, IV1_L1:IV2_L2, ...,
  #                                      IV1_L2:IV2_L1, IV1_L2:IV2_L2, ...)
  # ---------------------------------------------------------------------------
  cell_map <- tibble::tibble(
    dv_var    = dv_vars,
    iv1_level = rep(iv1_labels, each = n_iv2),
    iv2_level = rep(iv2_labels, times = n_iv1)
  )

  # ---------------------------------------------------------------------------
  # Run fully within-subjects ANOVA via jmv::anovaRM
  # ---------------------------------------------------------------------------
  rm_spec <- list(
    list(label = iv1_name, levels = iv1_labels),
    list(label = iv2_name, levels = iv2_labels)
  )

  # Map each column to its cell in the IV1 x IV2 crossing
  rm_cells <- lapply(seq_len(nrow(cell_map)), function(i) {
    list(
      measure = cell_map$dv_var[i],
      cell    = c(cell_map$iv1_level[i], cell_map$iv2_level[i])
    )
  })

  anova_result <- jmv::anovaRM(
    data       = analysis_df,
    rm         = rm_spec,
    rmCells    = rm_cells,
    spherTests = FALSE,
    spherCorr  = list("none"),
    effectSize = list(),
    emmPlots   = FALSE,
    emmTables  = FALSE
  )

  # ---------------------------------------------------------------------------
  # Extract within-subjects effects
  # ---------------------------------------------------------------------------
  within_df <- anova_result$rmTable$asDF

  # Find column names dynamically (may have [none] suffix)
  ss_col <- grep("^ss", names(within_df), value = TRUE)[1]
  df_col <- grep("^df", names(within_df), value = TRUE)[1]
  ms_col <- grep("^ms", names(within_df), value = TRUE)[1]
  f_col  <- grep("^F",  names(within_df), value = TRUE)[1]
  p_col  <- grep("^p",  names(within_df), value = TRUE)[1]

  # IV1 main effect
  iv1_row <- within_df[within_df$name == iv1_name, ]
  f_iv1   <- iv1_row[[f_col]]
  p_iv1   <- iv1_row[[p_col]]
  df_iv1  <- iv1_row[[df_col]]
  ss_iv1  <- iv1_row[[ss_col]]
  ms_iv1  <- iv1_row[[ms_col]]

  # IV2 main effect
  iv2_row <- within_df[within_df$name == iv2_name, ]
  f_iv2   <- iv2_row[[f_col]]
  p_iv2   <- iv2_row[[p_col]]
  df_iv2  <- iv2_row[[df_col]]
  ss_iv2  <- iv2_row[[ss_col]]
  ms_iv2  <- iv2_row[[ms_col]]

  # Interaction (IV1 x IV2)
  ix_row <- within_df[grepl(":", within_df$name), ]
  f_ix   <- ix_row[[f_col]]
  p_ix   <- ix_row[[p_col]]
  df_ix  <- ix_row[[df_col]]
  ss_ix  <- ix_row[[ss_col]]
  ms_ix  <- ix_row[[ms_col]]

  # Error terms - jmv anovaRM reports separate residuals for each effect
  # Residual rows follow each effect row in the table
  residual_rows <- which(grepl("Residual", within_df$name, ignore.case = TRUE))

  # The residuals appear in order: Error(IV1), Error(IV2), Error(IV1xIV2)
  ss_err_iv1  <- within_df[[ss_col]][residual_rows[1]]
  df_err_iv1  <- within_df[[df_col]][residual_rows[1]]
  ms_err_iv1  <- within_df[[ms_col]][residual_rows[1]]

  ss_err_iv2  <- within_df[[ss_col]][residual_rows[2]]
  df_err_iv2  <- within_df[[df_col]][residual_rows[2]]
  ms_err_iv2  <- within_df[[ms_col]][residual_rows[2]]

  ss_err_ix   <- within_df[[ss_col]][residual_rows[3]]
  df_err_ix   <- within_df[[df_col]][residual_rows[3]]
  ms_err_ix   <- within_df[[ms_col]][residual_rows[3]]

  # ---------------------------------------------------------------------------
  # Cell descriptives - NO ROUNDING
  # ---------------------------------------------------------------------------
  desc_rows <- list()
  for (i in seq_len(nrow(cell_map))) {
    vals <- analysis_df[[cell_map$dv_var[i]]]
    vals <- vals[!is.na(vals)]
    desc_rows <- c(desc_rows, list(tibble::tibble(
      iv1_level = cell_map$iv1_level[i],
      iv2_level = cell_map$iv2_level[i],
      dv_var    = cell_map$dv_var[i],
      mean      = mean(vals),
      sd        = stats::sd(vals),
      n         = length(vals),
      sem       = stats::sd(vals) / sqrt(length(vals))
    )))
  }
  desc_stats <- dplyr::bind_rows(desc_rows)

  desc_stats$iv1_label <- desc_stats$iv1_level
  desc_stats$iv2_label <- desc_stats$iv2_level

  # ---------------------------------------------------------------------------
  # Estimated marginal means - NO ROUNDING
  # ---------------------------------------------------------------------------
  emm_iv1 <- desc_stats |>
    dplyr::summarise(
      mean = mean(mean),
      se   = sqrt(ms_err_iv1 / dplyr::first(n)),
      iv1_label = dplyr::first(iv1_label),
      .by = iv1_level
    )

  emm_iv2 <- desc_stats |>
    dplyr::summarise(
      mean = mean(mean),
      se   = sqrt(ms_err_iv2 / dplyr::first(n)),
      iv2_label = dplyr::first(iv2_label),
      .by = iv2_level
    )

  # ---------------------------------------------------------------------------
  # Build results list - NO ROUNDING
  # ---------------------------------------------------------------------------
  results_list <- list(
    ANOVA = list(
      MainEffect_IV1 = list(
        F       = f_iv1,
        p_value = p_iv1,
        df      = df_iv1,
        ss      = ss_iv1,
        ms      = ms_iv1
      ),
      MainEffect_IV2 = list(
        F       = f_iv2,
        p_value = p_iv2,
        df      = df_iv2,
        ss      = ss_iv2,
        ms      = ms_iv2
      ),
      Interaction = list(
        F       = f_ix,
        p_value = p_ix,
        df      = df_ix,
        ss      = ss_ix,
        ms      = ms_ix
      ),
      Error_IV1 = list(
        ss = ss_err_iv1,
        df = df_err_iv1,
        ms = ms_err_iv1
      ),
      Error_IV2 = list(
        ss = ss_err_iv2,
        df = df_err_iv2,
        ms = ms_err_iv2
      ),
      Error_Interaction = list(
        ss = ss_err_ix,
        df = df_err_ix,
        ms = ms_err_ix
      ),
      n_obs = n_obs
    ),
    Descriptives = desc_stats,
    EMMs = list(
      IV1 = emm_iv1,
      IV2 = emm_iv2
    ),
    FactorInfo = list(
      iv1_name   = iv1_name,
      iv2_name   = iv2_name,
      iv1_labels = iv1_labels,
      iv2_labels = iv2_labels,
      dv_vars    = dv_vars,
      n_obs      = n_obs
    )
  )

  invisible(results_list)
}

