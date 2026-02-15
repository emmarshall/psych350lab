#' Multiple Regression Analysis
#'
#' Performs a multiple regression analysis with separate handling of quantitative
#' and binary predictors, including univariate, bivariate, and multivariate results
#' with interpretation categories (a-d).
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
#' @return A list with elements:
#' \describe{
#'   \item{Model}{Overall model statistics (R, R-squared, F, df, p).}
#'   \item{Univariate}{Descriptive statistics for each variable.}
#'   \item{Bivariate}{Correlation results for each predictor with criterion.}
#'   \item{Regression_Weights}{Regression coefficients with interpretation categories.}
#'   \item{Labels}{Variable names, labels, and types.}
#'   \item{Raw_Model}{The raw `lm` model object.}
#'   \item{Variable_Types}{Summary data frame of variable types.}
#' }
#'
#' @details
#' Interpretation categories for each predictor:
#' \itemize{
#'   \item **a**: Neither r nor b significant
#'   \item **b**: r and b both significant with same sign
#'   \item **c**: r significant but not b
#'   \item **d**: Suppressor effect
#' }
#'
#' @examples
#' data(superman_data)
#' sm <- superman_data[!is.na(superman_data$rt_critics_score) &
#'                     !is.na(superman_data$rt_audience_score), ]
#'
#' result <- regression_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Clark Height (in)", "Audience Score"),
#'   criterion_label = "Critics Score"
#' )
#' result$Model
#' result$Regression_Weights
#'
#' @export
regression_answers <- function(data, criterion,
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
  if (is.null(quant_predictors)) quant_predictors <- character(0)
  if (is.null(binary_predictors)) binary_predictors <- character(0)
  if (is.null(quant_labels)) quant_labels <- quant_predictors
  if (is.null(binary_labels)) binary_labels <- binary_predictors

  # Combine predictors in order: quantitative first, then binary
  predictors <- c(quant_predictors, binary_predictors)
  predictor_labels <- c(quant_labels, binary_labels)
  predictor_types <- c(rep("Quant", length(quant_predictors)),
                       rep("Binary", length(binary_predictors)))
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
  if (is.null(criterion_label)) {
    criterion_label <- criterion
  }

  # =========================================================================
  # DETECT AND VALIDATE VARIABLE CHARACTERISTICS
  # =========================================================================

  detect_variable_info <- function(x, var_name, expected_type) {
    unique_vals <- unique(x[!is.na(x)])
    n_unique <- length(unique_vals)

    if (expected_type == "Binary" && n_unique > 2) {
      warning(paste0("Variable '", var_name, "' specified as Binary but has ",
                     n_unique, " unique values. Treating as Binary anyway."))
    }

    if (expected_type == "Quant" && n_unique == 2) {
      warning(paste0("Variable '", var_name, "' specified as Quant but has only 2 unique values. ",
                     "Consider specifying as Binary."))
    }

    return(list(
      type = expected_type,
      levels = sort(unique_vals),
      n_levels = n_unique,
      min = min(unique_vals),
      max = max(unique_vals)
    ))
  }

  # Get info for all predictors
  detected_info <- list()
  for (i in seq_along(predictors)) {
    p <- predictors[i]
    detected_info[[p]] <- detect_variable_info(
      analysis_df[[p]], p, predictor_types[i]
    )
  }

  # Criterion info
  criterion_info <- detect_variable_info(analysis_df[[criterion]], criterion, "Quant")

  # =========================================================================
  # UNIVARIATE ANALYSIS
  # =========================================================================

  univariate_list <- list()

  # Criterion variable
  univariate_list[[criterion]] <- list(
    label = criterion_label,
    type = criterion_info$type,
    mean = round(mean(analysis_df[[criterion]], na.rm = TRUE), 3),
    sd = round(stats::sd(analysis_df[[criterion]], na.rm = TRUE), 3),
    min = round(min(analysis_df[[criterion]], na.rm = TRUE), 3),
    max = round(max(analysis_df[[criterion]], na.rm = TRUE), 3),
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
        criterion_mean_low = round(mean_low, 3),
        criterion_mean_high = round(mean_high, 3),
        n = n
      )
    } else {
      univariate_list[[p]] <- list(
        label = predictor_labels[i],
        type = "Quant",
        mean = round(mean(analysis_df[[p]], na.rm = TRUE), 3),
        sd = round(stats::sd(analysis_df[[p]], na.rm = TRUE), 3),
        min = round(min(analysis_df[[p]], na.rm = TRUE), 3),
        max = round(max(analysis_df[[p]], na.rm = TRUE), 3),
        n = n,
        n_unique = type_info$n_levels
      )
    }
  }

  # =========================================================================
  # BIVARIATE ANALYSIS
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

    cor_test <- stats::cor.test(analysis_df[[criterion]], analysis_df[[p]],
                                method = "pearson",
                                use = "complete.obs")

    r_value <- cor_test$estimate
    p_value <- cor_test$p.value
    p_value_formatted <- format_p_value(p_value)

    if (p_type == "Binary") {
      univar_info <- univariate_list[[p]]
      mean_low <- univar_info$criterion_mean_low
      mean_high <- univar_info$criterion_mean_high

      if (r_value > 0) {
        higher_group <- "high"
        direction_desc <- "Higher coded group has higher scores"
      } else {
        higher_group <- "low"
        direction_desc <- "Lower coded group has higher scores"
      }

      bivariate_list[[p]] <- list(
        label = predictor_labels[i],
        type = "Binary",
        r = round(r_value, 3),
        p_value = round(p_value, 3),
        p_value_formatted = p_value_formatted,
        significant = p_value < 0.05,
        direction = ifelse(r_value > 0, "positive", "negative"),
        higher_group = higher_group,
        direction_desc = direction_desc,
        mean_difference = round(mean_high - mean_low, 3),
        low_level = univar_info$low_level,
        high_level = univar_info$high_level,
        criterion_mean_low = mean_low,
        criterion_mean_high = mean_high
      )

    } else {
      bivariate_list[[p]] <- list(
        label = predictor_labels[i],
        type = "Quant",
        r = round(r_value, 3),
        p_value = round(p_value, 3),
        p_value_formatted = p_value_formatted,
        significant = p_value < 0.05,
        direction = ifelse(r_value > 0, "positive", "negative"),
        direction_desc = ifelse(r_value > 0,
                                "As predictor increases, criterion increases",
                                "As predictor increases, criterion decreases")
      )
    }
  }

  # =========================================================================
  # MULTIVARIATE ANALYSIS
  # =========================================================================

  formula_str <- paste(criterion, "~", paste(predictors, collapse = " + "))
  model_formula <- stats::as.formula(formula_str)

  model <- stats::lm(model_formula, data = analysis_df)
  model_summary <- summary(model)

  r_squared <- round(model_summary$r.squared, 3)
  r <- round(sqrt(model_summary$r.squared), 3)
  adj_r_squared <- round(model_summary$adj.r.squared, 3)
  f_stat <- round(model_summary$fstatistic[1], 3)
  df1 <- model_summary$fstatistic[2]
  df2 <- model_summary$fstatistic[3]
  f_pvalue_raw <- stats::pf(model_summary$fstatistic[1], df1, df2, lower.tail = FALSE)
  f_pvalue <- round(f_pvalue_raw, 3)
  f_pvalue_formatted <- format_p_value(f_pvalue_raw)

  coef_table <- as.data.frame(model_summary$coefficients)

  regression_weights <- list()

  for (i in seq_along(predictors)) {
    p <- predictors[i]
    p_type <- predictor_types[i]

    b <- coef_table[p, "Estimate"]
    se <- coef_table[p, "Std. Error"]
    t_val <- coef_table[p, "t value"]
    b_pvalue <- coef_table[p, "Pr(>|t|)"]
    b_pvalue_formatted <- format_p_value(b_pvalue)

    r_val <- bivariate_list[[p]]$r
    r_sig <- bivariate_list[[p]]$significant
    b_sig <- b_pvalue < 0.05

    # Determine interpretation category
    if (!r_sig && !b_sig) {
      category <- "a"
      category_desc <- "Neither r nor b significant"
    } else if (r_sig && b_sig && sign(r_val) == sign(b)) {
      category <- "b"
      category_desc <- "r & b both sig & same sign"
    } else if (r_sig && !b_sig) {
      category <- "c"
      category_desc <- "r sig but not b"
    } else {
      category <- "d"
      category_desc <- "suppressor effect"
    }

    if (p_type == "Binary") {
      univar_info <- univariate_list[[p]]

      if (b > 0) {
        direction_desc <- "Higher coded group has higher criterion scores"
      } else {
        direction_desc <- "Higher coded group has lower criterion scores"
      }

      regression_weights[[p]] <- list(
        label = predictor_labels[i],
        type = "Binary",
        b = round(b, 3),
        se = round(se, 3),
        t = round(t_val, 3),
        p_value = round(b_pvalue, 3),
        p_value_formatted = b_pvalue_formatted,
        significant = b_sig,
        category = category,
        category_desc = category_desc,
        direction = ifelse(b > 0, "positive", "negative"),
        direction_desc = direction_desc,
        low_level = univar_info$low_level,
        high_level = univar_info$high_level
      )

    } else {
      if (b > 0) {
        direction_desc <- "As predictor increases, criterion increases (controlling for others)"
      } else {
        direction_desc <- "As predictor increases, criterion decreases (controlling for others)"
      }

      regression_weights[[p]] <- list(
        label = predictor_labels[i],
        type = "Quant",
        b = round(b, 3),
        se = round(se, 3),
        t = round(t_val, 3),
        p_value = round(b_pvalue, 3),
        p_value_formatted = b_pvalue_formatted,
        significant = b_sig,
        category = category,
        category_desc = category_desc,
        direction = ifelse(b > 0, "positive", "negative"),
        direction_desc = direction_desc
      )
    }
  }

  intercept <- round(coef_table["(Intercept)", "Estimate"], 3)

  # =========================================================================
  # COMPILE RESULTS
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
      p_value_formatted = f_pvalue_formatted,
      significant = f_pvalue < 0.05,
      variance_explained = round(r_squared * 100, 1),
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

  results_list$Variable_Types <- data.frame(
    variable = c(criterion, predictors),
    label = c(criterion_label, predictor_labels),
    type = c(criterion_info$type, predictor_types),
    n_unique = c(criterion_info$n_levels,
                 sapply(detected_info, function(x) x$n_levels)),
    stringsAsFactors = FALSE
  )

  invisible(results_list)
}
