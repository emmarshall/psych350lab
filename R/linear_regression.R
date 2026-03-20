# =============================================================================
# Multiple Linear Regression Analysis
# =============================================================================
# Updated for dplyr 1.2.0+
# =============================================================================

#' Multiple Linear Regression Analysis
#'
#' Performs a multiple linear regression analysis with separate handling of
#' quantitative and binary predictors, including univariate, bivariate, and
#' multivariate results with interpretation categories (a-d).
#' All numeric values stored unrounded.
#'
#' @param data A data frame or tibble.
#' @param criterion Character string. Name of the criterion (dependent) variable.
#' @param quant_predictors Character vector or `NULL`. Names of quantitative predictors.
#' @param binary_predictors Character vector or `NULL`. Names of binary predictors.
#' @param quant_labels Character vector or `NULL`. Display labels for quantitative predictors.
#' @param binary_labels Character vector or `NULL`. Display labels for binary predictors.
#' @param criterion_label Character string or `NULL`. Display label for the criterion.
#' @param verbose Logical. Print diagnostic information. Default is `FALSE`.
#'
#' @return A list with elements (all numeric values unrounded).
#'
#' @examples
#' \dontrun{
#' # Using Superman data: predict rt_critics_score from year and clark_age
#' result <- linear_reg_answers(
#'   psych350data::superman,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("year", "clark_age"),
#'   quant_labels = c("Year", "Clark's Age")
#' )
#'
#' # Access model results
#' result$Model
#' result$Bivariate
#' result$Regression_Weights
#' }
#'
#' @export
linear_reg_answers <- function(data, criterion,
                               quant_predictors = NULL,
                               binary_predictors = NULL,
                               quant_labels = NULL,
                               binary_labels = NULL,
                               criterion_label = NULL,
                               verbose = FALSE) {

  # Validate that at least one predictor type is provided
  if (is.null(quant_predictors) && is.null(binary_predictors)) {
    stop("Must provide at least one of: quant_predictors or binary_predictors")
  }

  # Handle NULL cases
  quant_predictors <- quant_predictors %||% character(0)
  binary_predictors <- binary_predictors %||% character(0)
  quant_labels <- quant_labels %||% quant_predictors
  binary_labels <- binary_labels %||% binary_predictors

  # Combine predictors in order: quantitative first, then binary
  predictors <- c(quant_predictors, binary_predictors)
  predictor_labels <- c(quant_labels, binary_labels)
  predictor_types <- c(
    rep("Quant", length(quant_predictors)),
    rep("Binary", length(binary_predictors))
  )
  names(predictor_types) <- predictors

  # Clean data - remove NAs
  vars_needed <- c(criterion, predictors)

  if (verbose) {
    cat("\n=== MISSING DATA ANALYSIS ===\n")
    cat("Variables in analysis:", paste(vars_needed, collapse = ", "), "\n\n")
    cat("Starting N:", nrow(data), "\n")

    cat("\nMissing data by variable:\n")
    for (var in vars_needed) {
      n_missing <- sum(is.na(data[[var]]))
      pct_missing <- round(n_missing / nrow(data) * 100, 1)
      cat("  ", var, ":", n_missing, "(", pct_missing, "%)\n")
    }
  }

  analysis_df <- data[, vars_needed, drop = FALSE]

  if (verbose) {
    cat("\nRows with any missing data:", sum(!stats::complete.cases(analysis_df)), "\n")
  }

  analysis_df <- analysis_df[stats::complete.cases(analysis_df), ]

  if (verbose) {
    cat("Final N after listwise deletion:", nrow(analysis_df), "\n")
    cat("============================\n\n")
  }

  # Convert to numeric
  for (var in vars_needed) {
    analysis_df[[var]] <- as.numeric(analysis_df[[var]])
  }

  n <- nrow(analysis_df)
  k <- length(predictors)

  # Set default criterion label if not provided
  criterion_label <- criterion_label %||% criterion

  # =========================================================================
  # DETECT AND VALIDATE VARIABLE CHARACTERISTICS
  # =========================================================================

  .detect_variable_info <- function(x, var_name, expected_type) {
    unique_vals <- unique(x[!is.na(x)])
    n_unique <- length(unique_vals)

    if (expected_type == "Binary" && n_unique > 2) {
      warning(
        "Variable '", var_name, "' specified as Binary but has ",
        n_unique, " unique values. Treating as Binary anyway."
      )
    }

    if (expected_type == "Quant" && n_unique == 2) {
      warning(
        "Variable '", var_name, "' specified as Quant but has only 2 unique values. ",
        "Consider specifying as Binary."
      )
    }

    list(
      type = expected_type,
      levels = sort(unique_vals),
      n_levels = n_unique,
      min = min(unique_vals),
      max = max(unique_vals)
    )
  }

  # Get info for all predictors
  detected_info <- purrr::map(
    stats::setNames(predictors, predictors),
    \(p) .detect_variable_info(analysis_df[[p]], p, predictor_types[p])
  )

  # Criterion info
  criterion_info <- .detect_variable_info(analysis_df[[criterion]], criterion, "Quant")

  # =========================================================================
  # UNIVARIATE ANALYSIS - NO ROUNDING
  # =========================================================================

  univariate_list <- list()

  # Criterion variable
  univariate_list[[criterion]] <- list(
    label = criterion_label,
    type = criterion_info$type,
    mean = mean(analysis_df[[criterion]], na.rm = TRUE),
    sd = stats::sd(analysis_df[[criterion]], na.rm = TRUE),
    min = min(analysis_df[[criterion]], na.rm = TRUE),
    max = max(analysis_df[[criterion]], na.rm = TRUE),
    n = n,
    n_unique = criterion_info$n_levels
  )

  # Predictor variables
  for (i in seq_along(predictors)) {
    p <- predictors[i]
    p_type <- predictor_types[i]
    type_info <- detected_info[[p]]

    if (p_type == "Binary") {
      freq_table <- table(analysis_df[[p]])

      levels_vec <- type_info$levels
      low_level <- min(levels_vec)
      high_level <- max(levels_vec)

      mean_low <- mean(analysis_df[[criterion]][analysis_df[[p]] == low_level], na.rm = TRUE)
      mean_high <- mean(analysis_df[[criterion]][analysis_df[[p]] == high_level], na.rm = TRUE)

      univariate_list[[p]] <- list(
        label = predictor_labels[i],
        type = "Binary",
        frequencies = as.list(freq_table),
        levels = names(freq_table),
        levels_numeric = levels_vec,
        low_level = low_level,
        high_level = high_level,
        n_low = as.numeric(freq_table[as.character(low_level)]),
        n_high = as.numeric(freq_table[as.character(high_level)]),
        criterion_mean_low = mean_low,
        criterion_mean_high = mean_high,
        n = n
      )
    } else {
      univariate_list[[p]] <- list(
        label = predictor_labels[i],
        type = "Quant",
        mean = mean(analysis_df[[p]], na.rm = TRUE),
        sd = stats::sd(analysis_df[[p]], na.rm = TRUE),
        min = min(analysis_df[[p]], na.rm = TRUE),
        max = max(analysis_df[[p]], na.rm = TRUE),
        n = n,
        n_unique = type_info$n_levels
      )
    }
  }

  # =========================================================================
  # BIVARIATE ANALYSIS - NO ROUNDING
  # =========================================================================

  if (verbose) {
    cat("\n=== CORRELATION DIAGNOSTICS ===\n")
    cat("Correlations computed on N =", n, "cases\n")
    cat("All cases have complete data on all variables\n")
    cat("Using Pearson correlation (point-biserial for binary predictors)\n\n")
  }

  bivariate_list <- list()

  for (i in seq_along(predictors)) {
    p <- predictors[i]
    p_type <- predictor_types[i]

    cor_test <- stats::cor.test(analysis_df[[p]], analysis_df[[criterion]])
    r_value <- cor_test$estimate
    p_value <- cor_test$p.value

    if (verbose) {
      cat(p, "with", criterion, ":\n")
      cat("  r =", round(r_value, 3), "\n")
      cat("  p =", round(p_value, 4), "\n")
      cat("  Type:", p_type, "\n\n")
    }

    if (p_type == "Binary") {
      univar_info <- univariate_list[[p]]

      direction_desc <- dplyr::if_else(
        r_value > 0,
        paste0("Higher coded group (", univar_info$high_level,
               ") has higher ", criterion_label, " scores"),
        paste0("Lower coded group (", univar_info$low_level,
               ") has higher ", criterion_label, " scores")
      )

      bivariate_list[[p]] <- list(
        label = predictor_labels[i],
        type = "Binary",
        r = as.numeric(r_value),
        p_value = p_value,
        significant = p_value < 0.05,
        direction = dplyr::if_else(r_value > 0, "positive", "negative"),
        direction_desc = direction_desc,
        low_level = univar_info$low_level,
        high_level = univar_info$high_level,
        criterion_mean_low = univar_info$criterion_mean_low,
        criterion_mean_high = univar_info$criterion_mean_high
      )

    } else {
      bivariate_list[[p]] <- list(
        label = predictor_labels[i],
        type = "Quant",
        r = as.numeric(r_value),
        p_value = p_value,
        significant = p_value < 0.05,
        direction = dplyr::if_else(r_value > 0, "positive", "negative"),
        direction_desc = dplyr::if_else(
          r_value > 0,
          "As predictor increases, criterion increases",
          "As predictor increases, criterion decreases"
        )
      )
    }
  }

  # =========================================================================
  # MULTIVARIATE ANALYSIS - NO ROUNDING
  # =========================================================================

  formula_str <- paste(criterion, "~", paste(predictors, collapse = " + "))
  model_formula <- stats::as.formula(formula_str)

  model <- stats::lm(model_formula, data = analysis_df)
  model_summary <- summary(model)

  r_squared <- model_summary$r.squared
  r <- sqrt(model_summary$r.squared)
  adj_r_squared <- model_summary$adj.r.squared
  f_stat <- model_summary$fstatistic[1]
  df1 <- model_summary$fstatistic[2]
  df2 <- model_summary$fstatistic[3]
  f_pvalue <- stats::pf(model_summary$fstatistic[1], df1, df2, lower.tail = FALSE)

  coef_table <- as.data.frame(model_summary$coefficients)

  regression_weights <- list()

  for (i in seq_along(predictors)) {
    p <- predictors[i]
    p_type <- predictor_types[i]

    b <- coef_table[p, "Estimate"]
    se <- coef_table[p, "Std. Error"]
    t_val <- coef_table[p, "t value"]
    b_pvalue <- coef_table[p, "Pr(>|t|)"]

    r_val <- bivariate_list[[p]]$r
    r_sig <- bivariate_list[[p]]$significant
    b_sig <- b_pvalue < 0.05

    # Determine interpretation category
    category <- dplyr::case_when(
      !r_sig && !b_sig ~ "a",
      r_sig && b_sig && sign(r_val) == sign(b) ~ "b",
      r_sig && !b_sig ~ "c",
      .default = "d"
    )

    category_desc <- dplyr::case_when(
      category == "a" ~ "Neither r nor b significant",
      category == "b" ~ "r & b both sig & same sign",
      category == "c" ~ "r sig but not b",
      category == "d" ~ "suppressor effect"
    )

    if (p_type == "Binary") {
      univar_info <- univariate_list[[p]]

      direction_desc <- dplyr::if_else(
        b > 0,
        "Higher coded group has higher criterion scores",
        "Higher coded group has lower criterion scores"
      )

      regression_weights[[p]] <- list(
        label = predictor_labels[i],
        type = "Binary",
        b = b,
        se = se,
        t = t_val,
        p_value = b_pvalue,
        significant = b_sig,
        category = category,
        category_desc = category_desc,
        direction = dplyr::if_else(b > 0, "positive", "negative"),
        direction_desc = direction_desc,
        low_level = univar_info$low_level,
        high_level = univar_info$high_level
      )

    } else {
      direction_desc <- dplyr::if_else(
        b > 0,
        "As predictor increases, criterion increases (controlling for others)",
        "As predictor increases, criterion decreases (controlling for others)"
      )

      regression_weights[[p]] <- list(
        label = predictor_labels[i],
        type = "Quant",
        b = b,
        se = se,
        t = t_val,
        p_value = b_pvalue,
        significant = b_sig,
        category = category,
        category_desc = category_desc,
        direction = dplyr::if_else(b > 0, "positive", "negative"),
        direction_desc = direction_desc
      )
    }
  }

  intercept <- coef_table["(Intercept)", "Estimate"]

  # =========================================================================
  # COMPILE RESULTS - NO ROUNDING
  # =========================================================================

  results_list <- list(
    Model = list(
      R = r,
      R_squared = r_squared,
      Adj_R_squared = adj_r_squared,
      F = f_stat,
      df1 = df1,
      df2 = df2,
      p_value = f_pvalue,
      significant = f_pvalue < 0.05,
      variance_explained = r_squared * 100,
      intercept = intercept,
      n = n,
      k = k
    ),
    Univariate = univariate_list,
    Bivariate = bivariate_list,
    Regression_Weights = regression_weights,
    Labels = list(
      criterion = criterion,
      criterion_label = criterion_label,
      predictors = predictors,
      predictor_labels = predictor_labels,
      predictor_types = predictor_types,
      quant_predictors = quant_predictors,
      binary_predictors = binary_predictors,
      quant_labels = quant_labels,
      binary_labels = binary_labels,
      detected_info = detected_info
    ),
    Raw_Model = model
  )

  results_list$Variable_Types <- tibble::tibble(
    variable = c(criterion, predictors),
    label = c(criterion_label, predictor_labels),
    type = c(criterion_info$type, predictor_types),
    n_unique = c(
      criterion_info$n_levels,
      purrr::map_int(detected_info, \(x) x$n_levels)
    )
  )

  invisible(results_list)
}
