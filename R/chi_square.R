#' Chi-Square Test of Independence
#'
#' Performs a chi-square test of independence on two categorical variables,
#' returning observed and expected frequencies, proportions, and
#' descriptive counts.
#'
#' @param data A data frame or tibble.
#' @param var1 Character string. Name of the first categorical variable (rows).
#' @param var2 Character string. Name of the second categorical variable (columns).
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
#' data(superman_data)
#' # Chi-square test of clark_grp by tomatometer
#' result <- chi_square_answers(superman_data, "clark_grp", "tomatometer")
#' result$ChiSquare
#' result$ContingencyTable
#'
#' @export
chi_square_answers <- function(data, var1, var2) {

  vector1 <- as.numeric(data[[var1]])
  vector2 <- as.numeric(data[[var2]])

  valid_cases <- !is.na(vector1) & !is.na(vector2) &
    vector1 != -99 & vector2 != -99
  vector1 <- vector1[valid_cases]
  vector2 <- vector2[valid_cases]

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

  var1_counts <- table(vector1)
  var2_counts <- table(vector2)

  var1_desc <- tibble::tibble(
    variable = var1,
    level = names(var1_counts),
    n = as.numeric(var1_counts)
  )

  var2_desc <- tibble::tibble(
    variable = var2,
    level = names(var2_counts),
    n = as.numeric(var2_counts)
  )

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
