#' Chi-Square Test of Independence
#'
#' Performs a chi-square test of independence on two categorical variables,
#' returning observed and expected frequencies, proportions, and
#' descriptive counts. All numeric values stored unrounded.
#'
#' @param data A data frame or tibble.
#' @param var1 Character string. Name of the first categorical variable (rows).
#' @param var2 Character string. Name of the second categorical variable (columns).
#'
#' @return A list with elements (all numeric values unrounded).
#'
#' @examples
#' \dontrun{
#' # Using Superman data: test relationship between clark_grp and tomatometer
#' result <- chi_square_answers(psych350data::superman, "clark_grp", "tomatometer")
#'
#' # Access the chi-square statistic and p-value
#' result$ChiSquare
#' }
#'
#' @export
chi_square_answers <- function(data, var1, var2) {

  vector1 <- .prepare_categorical(data[[var1]])
  vector2 <- .prepare_categorical(data[[var2]])

  valid_cases <- !is.na(vector1) & !is.na(vector2)
  vector1 <- droplevels(vector1[valid_cases])
  vector2 <- droplevels(vector2[valid_cases])

  if (length(vector1) == 0 || length(vector2) == 0) {
    stop("No valid cases remaining after removing missing values")
  }

  contingency_table <- table(vector1, vector2)

  if (length(dim(contingency_table)) < 2 || any(dim(contingency_table) < 2)) {
    warning("Contingency table may be too small for reliable chi-square test")
  }

  chi_test <- stats::chisq.test(contingency_table, correct = FALSE)

  prop_table <- prop.table(contingency_table)
  observed <- as.data.frame.matrix(contingency_table)
  expected <- as.data.frame.matrix(chi_test$expected)

  var1_levels <- rownames(contingency_table)
  var2_levels <- colnames(contingency_table)

  var1_counts <- rowSums(contingency_table)
  var2_counts <- colSums(contingency_table)

  var1_desc <- tibble::tibble(
    variable = var1,
    level = var1_levels,
    n = as.numeric(var1_counts)
  )

  var2_desc <- tibble::tibble(
    variable = var2,
    level = var2_levels,
    n = as.numeric(var2_counts)
  )

  # NO ROUNDING - store raw values
  results_list <- list(
    ChiSquare = list(
      chi_sq = as.numeric(chi_test$statistic),
      p_value = chi_test$p.value,
      df = as.integer(chi_test$parameter),
      method = chi_test$method
    ),
    ContingencyTable = contingency_table,
    Observed = observed,
    Expected = expected,
    Proportions = prop_table,
    Var1_Descriptives = var1_desc,
    Var2_Descriptives = var2_desc,
    Sample_Size = sum(contingency_table)
  )

  invisible(results_list)
}


#' k-Group Chi-Square Test with Pairwise Comparisons
#'
#' Performs a chi-square test of independence on two categorical variables
#' where var1 has 3+ levels and var2 has exactly 2 levels, then conducts
#' all pairwise chi-square comparisons with effect sizes.
#' All numeric values stored unrounded.
#'
#' @param data A data frame or tibble.
#' @param var1 Character string. Name of the multi-level categorical variable (rows).
#' @param var2 Character string. Name of the binary categorical variable (columns).
#' @param var1_labels Character vector or NULL. Display labels for var1 levels.
#' @param var2_labels Character vector or NULL. Display labels for var2 levels.
#' @param pct_var2_level Integer. Which level of var2 to use for percentage
#'   comparisons (1 or 2). Default is 2.
#'
#' @return A list with elements (all numeric values unrounded).
#'
#' @examples
#' \dontrun{
#' # Using Superman data: test relationship between type (3 levels) and tomatometer (2 levels)
#' result <- chi_square_kgroup_answers(
#'   psych350data::superman,
#'   var1 = "type",
#'   var2 = "tomatometer",
#'   var1_labels = c("Film", "TV Show", "Serial")
#' )
#'
#' # Access omnibus chi-square and pairwise comparisons
#' result$ChiSquare
#' result$Pairwise
#' }
#'
#' @export
chi_square_kgroup_answers <- function(data, var1, var2,
                                      var1_labels = NULL,
                                      var2_labels = NULL,
                                      pct_var2_level = 2) {

  if (!var1 %in% names(data)) stop(paste("Variable", var1, "not found in dataset"))
  if (!var2 %in% names(data)) stop(paste("Variable", var2, "not found in dataset"))

  vector1 <- .prepare_categorical(data[[var1]])
  vector2 <- .prepare_categorical(data[[var2]])

  valid_cases <- !is.na(vector1) & !is.na(vector2)
  vector1 <- droplevels(vector1[valid_cases])
  vector2 <- droplevels(vector2[valid_cases])

  if (length(vector1) == 0 || length(vector2) == 0) {
    stop("No valid data after removing NAs.")
  }

  contingency_table <- table(vector1, vector2)

  if (any(rowSums(contingency_table) == 0) || any(colSums(contingency_table) == 0)) {
    contingency_table <- contingency_table[rowSums(contingency_table) > 0, , drop = FALSE]
    contingency_table <- contingency_table[, colSums(contingency_table) > 0, drop = FALSE]
  }

  if (ncol(contingency_table) != 2) {
    stop("var2 must have exactly 2 levels for pairwise chi-square comparisons")
  }

  chi_test <- stats::chisq.test(contingency_table, correct = FALSE)
  chi_sq <- chi_test$statistic
  p_value <- chi_test$p.value
  df <- chi_test$parameter
  total_n <- sum(contingency_table)

  var1_levels <- rownames(contingency_table)
  var2_levels <- colnames(contingency_table)

  var1_counts <- rowSums(contingency_table)
  var2_counts <- colSums(contingency_table)

  n_var1 <- length(var1_levels)

  # Create labels that match contingency table order
  if (is.null(var1_labels)) {
    use_var1_labels <- var1_levels
  } else if (length(var1_labels) == n_var1) {
    use_var1_labels <- var1_labels
  } else {
    warning("var1_labels length doesn't match number of levels, using level names")
    use_var1_labels <- var1_levels
  }

  if (is.null(var2_labels)) {
    use_var2_labels <- var2_levels
  } else if (length(var2_labels) == 2) {
    use_var2_labels <- var2_labels
  } else {
    warning("var2_labels length doesn't match number of levels, using level names")
    use_var2_labels <- var2_levels
  }

  var1_desc <- tibble::tibble(
    variable = var1,
    level = var1_levels,
    level_label = use_var1_labels,
    n = as.numeric(var1_counts)
  )

  var2_desc <- tibble::tibble(
    variable = var2,
    level = var2_levels,
    level_label = use_var2_labels,
    n = as.numeric(var2_counts)
  )

  # Pairwise comparisons - NO ROUNDING
  pairwise_results <- list()
  comparison_counter <- 1
  chi_crit <- 3.84

  for (i in 1:(n_var1 - 1)) {
    for (j in (i + 1):n_var1) {
      row1_label <- use_var1_labels[i]
      row2_label <- use_var1_labels[j]

      pairwise_table <- contingency_table[c(i, j), , drop = FALSE]

      if (sum(pairwise_table) == 0 || any(colSums(pairwise_table) == 0)) next

      a <- pairwise_table[1, 1]
      b <- pairwise_table[1, 2]
      c <- pairwise_table[2, 1]
      d <- pairwise_table[2, 2]

      pairwise_results_calc <- pr_chi_to_r(a, b, c, d)
      pairwise_chi_stat <- pairwise_results_calc$chi_square
      effect_size <- pairwise_results_calc$r_effect_size

      if (pct_var2_level == 1) {
        pct_row1 <- (a / (a + b)) * 100
        pct_row2 <- (c / (c + d)) * 100
      } else {
        pct_row1 <- (b / (a + b)) * 100
        pct_row2 <- (d / (c + d)) * 100
      }

      is_sig <- pairwise_chi_stat > chi_crit

      if (is_sig) {
        chi_result <- if (pct_row1 > pct_row2) ">" else "<"
      } else {
        chi_result <- "="
      }

      error_type <- if (is_sig) "Type I & III" else "Type II"

      if (is_sig) {
        power_problem <- "No \u2013 rejecting H0: means there was sufficient power"
      } else {
        if (effect_size < 0.10) {
          power_problem <- "No \u2013 effect is \"too small to be interesting,\" (r < .10)"
        } else {
          power_problem <- "Yes \u2013 The effect is \"large enough to be interesting,\" (r > .10)"
        }
      }

      # NO ROUNDING - store raw values
      pairwise_results[[comparison_counter]] <- list(
        comparison = paste(row1_label, "vs", row2_label),
        group1_label = row1_label,
        group2_label = row2_label,
        pct1 = pct_row1,
        pct2 = pct_row2,
        chi_sq = pairwise_chi_stat,
        chi_result = chi_result,
        error_type = error_type,
        effect_size = effect_size,
        power_problem = power_problem,
        cell_freqs = list(a = a, b = b, c = c, d = d)
      )
      comparison_counter <- comparison_counter + 1
    }
  }

  pct_label <- use_var2_labels[pct_var2_level]

  # NO ROUNDING in final output
  results_list <- list(
    ChiSquare = list(
      chi_sq = as.numeric(chi_sq),
      p_value = p_value,
      df = as.integer(df)
    ),
    ContingencyTable = contingency_table,
    Var1_Descriptives = var1_desc,
    Var2_Descriptives = var2_desc,
    Sample_Size = total_n,
    ChiCrit = chi_crit,
    Pairwise = pairwise_results,
    var1_labels = var1_desc$level_label,
    var2_labels = var2_desc$level_label,
    pct_var2_level = pct_var2_level,
    pct_var2_label = pct_label
  )

  invisible(results_list)
}


#' Chi-Square Effect Size from 2x2 Table
#'
#' Computes chi-square statistic, p-value, and r effect size from
#' cell frequencies of a 2x2 contingency table.
#'
#' @param a Integer. Cell frequency (row 1, col 1).
#' @param b Integer. Cell frequency (row 1, col 2).
#' @param c Integer. Cell frequency (row 2, col 1).
#' @param d Integer. Cell frequency (row 2, col 2).
#'
#' @return A list with `chi_square`, `p_value`, and `r_effect_size`.
#'
#' @examples
#' pr_chi_to_r(10, 20, 30, 40)
#'
#' @export
pr_chi_to_r <- function(a, b, c, d) {
  n <- a + b + c + d
  chi_sq <- (n * (a*d - b*c)^2) / ((a + b) * (c + d) * (a + c) * (b + d))
  p_val <- stats::pchisq(chi_sq, df = 1, lower.tail = FALSE)
  r_effect <- sqrt(chi_sq / n)
  return(list(chi_square = chi_sq, p_value = p_val, r_effect_size = r_effect))
}


