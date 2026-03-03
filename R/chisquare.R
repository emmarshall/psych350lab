#' Chi-Square Test of Independence
#'
#' Performs a chi-square test of independence on two categorical variables,
#' returning observed and expected frequencies, proportions, and
#' descriptive counts.
#'
#' @param data A data frame or tibble.
#' @param var1 Character string. Name of the first categorical variable (rows).
#'   Can be numeric codes, character strings, or factors.
#' @param var2 Character string. Name of the second categorical variable (columns).
#'   Can be numeric codes, character strings, or factors.
#'
#' @return A list with elements:
#' \describe{
#'   \item{ChiSquare}{A list with `chi_sq`, `p_value`, `p_value_raw`, `df`, and `method`.}
#'   \item{ContingencyTable}{The raw contingency table.}
#'   \item{Observed}{Observed frequencies as a data frame.}
#'   \item{Expected}{Expected frequencies as a data frame.}
#'   \item{Proportions}{Cell proportions.}
#'   \item{Var1_Descriptives}{Counts for each level of var1.}
#'   \item{Var2_Descriptives}{Counts for each level of var2.}
#'   \item{Sample_Size}{Total number of valid cases.}
#' }
#'
#' @examples
#' data(superman)
#' # Chi-square test of clark_grp by tomatometer
#' result <- chi_square_answers(superman, "clark_grp", "tomatometer")
#' result$ChiSquare
#' result$ContingencyTable
#'
#' @export
chi_square_answers <- function(data, var1, var2) {

  # Convert to factors (handles numeric, character, and factor input)
  vector1 <- .prepare_categorical(data[[var1]])
  vector2 <- .prepare_categorical(data[[var2]])

  # Remove missing values
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

  var1_counts <- sort(table(vector1))
  var2_counts <- sort(table(vector2))

  var1_desc <- tibble::tibble(
    variable = var1,
    level = names(var1_counts),
    n = as.numeric(var1_counts)
  )  |>
    dplyr::arrange(level)  # ADD THIS

  var2_desc <- tibble::tibble(
    variable = var2,
    level = names(var2_counts),
    n = as.numeric(var2_counts)
  )  |>
    dplyr::arrange(level)  # ADD THIS

  p_value_formatted <- if (chi_test$p.value < 0.001) {
    0.001
  } else {
    round(chi_test$p.value, 3)
  }

  results_list <- list(
    ChiSquare = list(
      chi_sq = round(chi_test$statistic, 2),
      p_value = p_value_formatted,
      p_value_raw = chi_test$p.value,
      df = chi_test$parameter,
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

#' Chi-Square Effect Size from 2x2 Table
#'
#' Computes chi-square statistic, p-value, and r effect size from cell frequencies.
#'
#' @param a Cell frequency (row 1, col 1)
#' @param b Cell frequency (row 1, col 2)
#' @param c Cell frequency (row 2, col 1)
#' @param d Cell frequency (row 2, col 2)
#'
#' @return A list with `chi_square`, `p_value`, and `r_effect_size`.
#'
#' @examples
#' pr_chi_to_r(10, 20, 30, 40)
#'
#' @export
pr_chi_to_r <- function(a, b, c, d) {
  n <- a + b + c + d
  chi_sq <- (n * (a * d - b * c)^2) / ((a + b) * (c + d) * (a + c) * (b + d))
  p_val <- stats::pchisq(chi_sq, df = 1, lower.tail = FALSE)
  r_effect <- sqrt(chi_sq / n)

  list(
    chi_square = chi_sq,
    p_value = p_val,
    r_effect_size = r_effect
  )
}


#' k-Group Chi-Square Test with Pairwise Comparisons
#'
#' Performs a chi-square test of independence on two categorical variables
#' where var1 has 3+ levels and var2 has exactly 2 levels, then conducts
#' all pairwise chi-square comparisons with effect sizes.
#'
#' @param data A data frame or tibble.
#' @param var1 Character string. Name of the multi-level categorical variable (rows).
#'   Can be numeric codes, character strings, or factors.
#' @param var2 Character string. Name of the binary categorical variable (columns).
#'   Must have exactly 2 levels. Can be numeric codes, character strings, or factors.
#' @param var1_labels Character vector or NULL. Display labels for var1 levels.
#' @param var2_labels Character vector or NULL. Display labels for var2 levels.
#' @param pct_var2_level Integer. Which level of var2 to use for percentage
#'   comparisons (1 or 2). Default is 2.
#'
#' @return A list with elements:
#' \describe{
#'   \item{ChiSquare}{Omnibus chi-square results: `chi_sq`, `p_value`, `df`.}
#'   \item{ContingencyTable}{The raw contingency table.}
#'   \item{Var1_Descriptives}{Counts for each level of var1.}
#'   \item{Var2_Descriptives}{Counts for each level of var2.}
#'   \item{Sample_Size}{Total number of valid cases.}
#'   \item{ChiCrit}{Critical chi-square value (3.84 for df=1, alpha=.05).}
#'   \item{Pairwise}{List of pairwise comparison results.}
#'   \item{var1_labels}{Labels used for var1 levels.}
#'   \item{var2_labels}{Labels used for var2 levels.}
#'   \item{pct_var2_level}{Which var2 level was used for percentages.}
#'   \item{pct_var2_label}{Label for the percentage comparison level.}
#' }
#'
#' @examples
#' data(superman)
#' # Compare height_gap (3 levels) by tomatometer (2 levels)
#' result <- chi_square_kgroup_answers(
#'   superman,
#'   var1 = "height_gap",
#'   var2 = "tomatometer",
#'   var1_labels = c("Minimal", "Average", "Big"),
#'   var2_labels = c("Rotten", "Fresh")
#' )
#' result$ChiSquare
#' result$Pairwise
#'
#' @export
chi_square_kgroup_answers <- function(data, var1, var2,
                                          var1_labels = NULL,
                                          var2_labels = NULL,
                                          pct_var2_level = 2) {

  if (!var1 %in% names(data)) stop(paste("Variable", var1, "not found in dataset"))

  if (!var2 %in% names(data)) stop(paste("Variable", var2, "not found in dataset"))

  # Convert to factors (handles numeric, character, and factor input)
  vector1 <- .prepare_categorical(data[[var1]])
  vector2 <- .prepare_categorical(data[[var2]])

  # Remove missing values
  valid_cases <- !is.na(vector1) & !is.na(vector2)
  vector1 <- droplevels(vector1[valid_cases])
  vector2 <- droplevels(vector2[valid_cases])

  if (length(vector1) == 0 || length(vector2) == 0) {
    stop("No valid data after removing NAs.")
  }

  contingency_table <- table(vector1, vector2)

  # Remove empty rows/columns
  if (any(rowSums(contingency_table) == 0) || any(colSums(contingency_table) == 0)) {
    contingency_table <- contingency_table[rowSums(contingency_table) > 0, , drop = FALSE]
    contingency_table <- contingency_table[, colSums(contingency_table) > 0, drop = FALSE]
  }

  if (ncol(contingency_table) != 2) {
    stop("var2 must have exactly 2 levels for pairwise chi-square comparisons")
  }

  # Omnibus chi-square test
  chi_test <- stats::chisq.test(contingency_table, correct = FALSE)
  chi_sq <- chi_test$statistic
  p_value <- chi_test$p.value
  df <- chi_test$parameter
  total_n <- sum(contingency_table)

  # Descriptives
  var1_counts <- table(vector1)
  var2_counts <- table(vector2)

  var1_desc <- tibble::tibble(
    variable = var1,
    level = names(var1_counts),
    level_label = if (!is.null(var1_labels)) var1_labels else names(var1_counts),
    n = as.numeric(var1_counts)
  )

  var2_desc <- tibble::tibble(
    variable = var2,
    level = names(var2_counts),
    level_label = if (!is.null(var2_labels)) var2_labels else names(var2_counts),
    n = as.numeric(var2_counts)
  )

  # Pairwise comparisons
  var1_levels <- rownames(contingency_table)
  n_var1 <- length(var1_levels)
  pairwise_results <- list()
  comparison_counter <- 1
  chi_crit <- 3.84

  for (i in 1:(n_var1 - 1)) {
    for (j in (i + 1):n_var1) {
      if (!is.null(var1_labels)) {
        row1_label <- var1_labels[i]
        row2_label <- var1_labels[j]
      } else {
        row1_label <- var1_levels[i]
        row2_label <- var1_levels[j]
      }

      pairwise_table <- contingency_table[c(i, j), , drop = FALSE]

      if (sum(pairwise_table) == 0 || any(colSums(pairwise_table) == 0)) next

      a <- pairwise_table[1, 1]
      b <- pairwise_table[1, 2]
      c <- pairwise_table[2, 1]
      d <- pairwise_table[2, 2]

      pairwise_results_calc <- pr_chi_to_r(a, b, c, d)
      pairwise_chi_stat <- pairwise_results_calc$chi_square
      effect_size <- pairwise_results_calc$r_effect_size

      # Calculate percentages based on specified var2 level
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

      pairwise_results[[comparison_counter]] <- list(
        comparison = paste(row1_label, "vs", row2_label),
        group1_label = row1_label,
        group2_label = row2_label,
        pct1 = round(pct_row1, 2),
        pct2 = round(pct_row2, 2),
        chi_sq = round(pairwise_chi_stat, 2),
        chi_result = chi_result,
        error_type = error_type,
        effect_size = round(effect_size, 2),
        power_problem = power_problem,
        cell_freqs = list(a = a, b = b, c = c, d = d)
      )
      comparison_counter <- comparison_counter + 1
    }
  }

  pct_label <- if (!is.null(var2_labels)) {
    var2_labels[pct_var2_level]
  } else {
    paste("Level", pct_var2_level)
  }

  results_list <- list(
    ChiSquare = list(
      chi_sq = round(chi_sq, 2),
      p_value = round(p_value, 3),
      df = df
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


