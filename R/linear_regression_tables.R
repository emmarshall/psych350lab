# =============================================================================
# regression_tables.R
# Linear Regression Tables for Word/PDF Output (Flextable)
# =============================================================================
# Updated using dplyr 1.2.0+
# Note still needs to be adapted for logical regression
# =============================================================================

# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

#' Convert p-value to significance stars (internal)
#'
#' @param p Numeric p-value.
#' @return Character string: "***", "**", "*", or "ns".
#' @noRd
.p_to_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    .default = "ns"
  )
}


#' Create significance MCQ (internal)
#'
#' Creates a webexercises multiple choice question with the correct
#' significance level pre-selected.
#'
#' @param p_value Numeric p-value.
#' @return A webexercises mcq HTML string.
#' @noRd
.sig_mcq <- function(p_value) {
  if (!requireNamespace("webexercises", quietly = TRUE)) {
    stop("Package 'webexercises' is required.")
  }

  stars <- .p_to_stars(p_value)
  mcq_opts <- c("ns", "*", "**", "***")
  names(mcq_opts) <- dplyr::if_else(mcq_opts == stars, "answer", mcq_opts)

  webexercises::mcq(mcq_opts)
}


# =============================================================================
# MODEL STATISTICS LINE
# =============================================================================

#' Linear Regression Model Statistics Line
#'
#' Returns a formatted text string with overall model statistics (R, R-squared,
#' F, df, p) as either a filled answer key or blank worksheet.
#'
#' @param reg_results_list Output from [linear_reg_answers()].
#' @param KEY Logical. If `TRUE` (default), show filled values.
#'   If `FALSE`, show blanks.
#' @param highlight Logical. If `TRUE` (default) and `KEY = TRUE`,
#'   wrap values in highlight markup for Quarto/RMarkdown.
#' @param ... Additional arguments passed to underlying functions
#'
#' @return A character string.
#'
#' @examples
#' data(superman, package = "psych350data")
#' sm <- superman |>
#'   dplyr::filter(!is.na(rt_critics_score), !is.na(rt_audience_score))
#' result <- linear_reg_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' cat(linear_reg_model_statistics(result, KEY = TRUE, highlight = FALSE))
#'
#' @export
linear_reg_model_statistics <- function(reg_results_list,
                                        KEY = TRUE,
                                        highlight = TRUE) {

  r <- reg_results_list$Model$R
  r_sq <- reg_results_list$Model$R_squared
  f_stat <- reg_results_list$Model$F
  df1 <- reg_results_list$Model$df1
  df2 <- reg_results_list$Model$df2
  p_val_formatted <- reg_results_list$Model$p_value_formatted

  hl <- function(text) {
    if (highlight && KEY) {
      paste0("[", text, "]{custom-style=\"highlight-yellow\"}")
    } else {
      as.character(text)
    }
  }

  if (KEY) {
    output <- paste0(
      "R = ", hl(r), "     ",
      "R\u00B2 = ", hl(r_sq), "     ",
      "F = ", hl(f_stat), "     ",
      "df = ", hl(df1), ", ", hl(df2), "     ",
      "p ", hl(p_val_formatted), "\n"
    )
  } else {
    output <- paste0(
      "R = ______     ",
      "R\u00B2 = ______     ",
      "F = ______     ",
      "df = ____, ____     ",
      "p = ______\n"
    )
  }

  return(output)
}


#' @rdname linear_reg_model_statistics
#' @export
regression_model_statistics <- function(...) {
  .Deprecated("linear_reg_model_statistics")
  linear_reg_model_statistics(...)
}


# =============================================================================
# MODEL EVALUATION TEXT
# =============================================================================

#' Linear Regression Model Evaluation Text
#'
#' Returns formatted text for evaluating the multiple linear regression model:
#' (a) does the model work, and (b) how well does it work.
#' Either filled (answer key) or blank (worksheet).
#'
#' @param reg_results_list Output from [linear_reg_answers()].
#' @param KEY Logical. If `TRUE` (default), show filled answers.
#'   If `FALSE`, show blanks.
#' @param highlight Logical. If `TRUE` (default) and `KEY = TRUE`,
#'   wrap values in highlight markup.
#' @param ... Additional arguments passed to underlying functions
#'
#' @return A character string with markdown formatting.
#'
#' @examples
#' data(superman, package = "psych350data")
#' sm <- superman |>
#'   dplyr::filter(!is.na(rt_critics_score), !is.na(rt_audience_score))
#' result <- linear_reg_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' cat(linear_reg_model_evaluation(result, KEY = TRUE, highlight = FALSE))
#'
#' @export
linear_reg_model_evaluation <- function(reg_results_list,
                                        KEY = TRUE,
                                        highlight = TRUE) {

  r <- reg_results_list$Model$R
  r_sq <- reg_results_list$Model$R_squared
  f_stat <- reg_results_list$Model$F
  df1 <- reg_results_list$Model$df1
  df2 <- reg_results_list$Model$df2
  p_val <- reg_results_list$Model$p_value
  p_val_formatted <- reg_results_list$Model$p_value_formatted
  var_explained <- reg_results_list$Model$variance_explained
  criterion_label <- reg_results_list$Labels$criterion_label

  hl <- function(text) {
    if (highlight && KEY) {
      paste0("[", text, "]{custom-style=\"highlight-yellow\"}")
    } else {
      as.character(text)
    }
  }

  if (KEY) {
    model_works <- dplyr::if_else(p_val < 0.05, "Yes", "No")
    p_comparison <- dplyr::if_else(p_val < 0.05, "< .05", ">= .05")

    output <- paste0(
      "**a. Does the multiple linear regression model work? Where did you look to decide?**\n\n",
      hl(model_works), ", p ", hl(p_comparison),
      ", significant ANOVA (F = ", hl(f_stat),
      ", df = ", hl(df1), ", ", hl(df2),
      ", p ", hl(p_val_formatted), ")\n\n",
      "**b. How well does the multiple linear regression model work?**\n\n",
      "Accounts for ", hl(var_explained), "% of ", criterion_label,
      " variance (R\u00B2 = ", hl(r_sq), ")\n"
    )
  } else {
    output <- paste0(
      "**a. Does the multiple linear regression model work? Where did you look to decide?**\n\n",
      "\n\n",
      "**b. How well does the multiple linear regression model work?**\n\n",
      "\n\n"
    )
  }

  return(output)
}


#' @rdname linear_reg_model_evaluation
#' @export
regression_model_evaluation <- function(...) {
  .Deprecated("linear_reg_model_evaluation")
  linear_reg_model_evaluation(...)
}


# =============================================================================
# COMBINED PREDICTOR RESULTS TABLE
# =============================================================================

#' Linear Regression Combined Predictor Results Table (Flextable)
#'
#' Creates a flextable with one row per predictor showing variable type,
#' bivariate correlation r (with p), regression weight b (with p), and
#' bivariate/multivariate interpretation category (a-d).
#'
#' @param reg_results_list Output from [linear_reg_answers()].
#' @param KEY Logical. If `TRUE` (default), fill with computed values.
#' @param ... Additional arguments passed to underlying functions
#'   If `FALSE`, create a blank template (column widths are preserved).
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman, package = "psych350data")
#' sm <- superman |>
#'   dplyr::filter(!is.na(rt_critics_score), !is.na(rt_audience_score))
#' result <- linear_reg_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_linear_reg_combined_table(result, KEY = TRUE)
#' create_linear_reg_combined_table(result, KEY = FALSE)
#'
#' @export
create_linear_reg_combined_table <- function(reg_results_list, KEY = TRUE) {

  predictors <- reg_results_list$Labels$predictors
  predictor_labels <- reg_results_list$Labels$predictor_labels
  predictor_types <- reg_results_list$Labels$predictor_types

  if (KEY) {
    data_rows <- tibble::tibble(
      Predictor = predictor_labels,
      Type = predictor_types,
      r_p = purrr::map_chr(predictors, \(p) {
        bivar <- reg_results_list$Bivariate[[p]]
        paste0(format_stat(bivar$r, remove_leading_zero = TRUE),
               " (", format_p_value(bivar$p_value), ")")
      }),
      b_p = purrr::map_chr(predictors, \(p) {
        regwt <- reg_results_list$Regression_Weights[[p]]
        paste0(sprintf("%.3f", regwt$b), " (", regwt$p_value_formatted, ")")
      }),
      Result = purrr::map_chr(predictors, \(p) {
        reg_results_list$Regression_Weights[[p]]$category
      })
    )
  } else {
    data_rows <- tibble::tibble(
      Predictor = predictor_labels,
      Type = rep("", length(predictors)),
      r_p = rep("", length(predictors)),
      b_p = rep("", length(predictors)),
      Result = rep("", length(predictors))
    )
  }

  # Get criterion label for header
  criterion_label <- reg_results_list$Labels$criterion_label

  colnames(data_rows) <- c(
    "Predictor",
    "Is variable\nquant or binary?",
    paste0("r (p)\nWith ", criterion_label),
    "b (p)",
    paste0(
      "**Bivariate/Multivariate Results Categories:**\n",
      "a. Neither r nor b significant\n",
      "b. r & b both sig & same sign\n",
      "c. r sig but not b\n",
      "d. suppressor effect"
    )
  )

  ft <- flextable::flextable(data_rows) |>
    flextable::width(j = 1, width = 2.5) |>
    flextable::width(j = 2, width = 1.5) |>
    flextable::width(j = 3, width = 1.2) |>
    flextable::width(j = 4, width = 1.2) |>
    flextable::width(j = 5, width = 1.8) |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::align(j = 5, align = "left", part = "header") |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::border_outer(border = officer::fp_border(width = 2), part = "all") |>
    flextable::border_inner_h(border = officer::fp_border(width = 1), part = "all") |>
    flextable::border_inner_v(border = officer::fp_border(width = 1), part = "all") |>
    flextable::hline_top(border = officer::fp_border(width = 2), part = "header") |>
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "body") |>
    flextable::bg(bg = "#f0f0f0", part = "header") |>
    flextable::padding(padding = 3, part = "all") |>
    flextable::bold(j = 5, part = "header", bold = FALSE) |>
    flextable::compose(
      j = 5, part = "header",
      value = flextable::as_paragraph(
        flextable::as_chunk(
          "Bivariate/Multivariate Results Categories:",
          props = officer::fp_text(bold = TRUE)
        ),
        "\n",
        "a. Neither r nor b significant\n",
        "b. r & b both sig & same sign\n",
        "c. r sig but not b\n",
        "d. suppressor effect"
      )
    )

  return(ft)
}


#' @rdname create_linear_reg_combined_table
#' @export
create_regression_combined_table <- function(...) {
  .Deprecated("create_linear_reg_combined_table")
  create_linear_reg_combined_table(...)
}


# =============================================================================
# REGRESSION TABLE LEGEND
# =============================================================================

#' Linear Regression Table Legend
#'
#' Returns a formatted markdown string with the bivariate/multivariate
#' results category legend.
#'
#' @return A character string with markdown formatting.
#'
#' @examples
#' cat(linear_reg_table_legend())
#'
#' @export
linear_reg_table_legend <- function() {
  paste0(
    "**\\*\\*\\*Bivariate/Multivariate Results Categories:**  ",
    "a. Neither r nor b significant    ",
    "b. r & b both sig & same sign    ",
    "c. r sig but not b    ",
    "d. suppressor effect\n"
  )
}


#' @rdname linear_reg_table_legend
#' @export
regression_table_legend <- function() {
  .Deprecated("linear_reg_table_legend")
  linear_reg_table_legend()
}


# =============================================================================
# REGRESSION CATEGORY LEGEND
# =============================================================================

#' Linear Regression Category Legend
#'
#' Returns a formatted markdown string with the bivariate/multivariate
#' results category legend and significance key.
#'
#' @return A character string with markdown formatting.
#'
#' @examples
#' cat(linear_reg_category_legend())
#'
#' @export
linear_reg_category_legend <- function() {
  paste0(
    "**Bivariate/Multivariate Results Categories:**\n\n",
    "- **a.** Neither r nor b significant\n",
    "- **b.** r & b both significant & same sign\n",
    "- **c.** r significant but not b\n",
    "- **d.** suppressor effect\n\n",
    "**Significance Key:** ns = p > .05, * = p < .05, ** = p < .01, *** = p < .001\n"
  )
}


# =============================================================================
# CORRELATION INTERPRETATION TABLE (FLEXTABLE)
# =============================================================================

#' Linear Regression Correlation Interpretation Table (Flextable)
#'
#' Creates a flextable with one row per predictor showing the bivariate
#' correlation interpretation. Auto-generates interpretations or uses
#' custom-provided ones. Answer key uses red text.
#'
#' @param reg_results_list Output from [linear_reg_answers()].
#' @param interpretations Named list or `NULL`. Custom interpretations
#'   keyed by predictor variable name. If `NULL`, auto-generated.
#' @param KEY Logical. If `TRUE` (default), show filled interpretations
#'   (red text). If `FALSE`, blank cells.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman, package = "psych350data")
#' sm <- superman |>
#'   dplyr::filter(!is.na(rt_critics_score), !is.na(rt_audience_score))
#' result <- linear_reg_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_linear_reg_correlation_interp_table(result, KEY = TRUE)
#' create_linear_reg_correlation_interp_table(result, KEY = FALSE)
#'
#' @export
create_linear_reg_correlation_interp_table <- function(reg_results_list,
                                                       interpretations = NULL,
                                                       KEY = TRUE) {

  predictors <- reg_results_list$Labels$predictors
  predictor_labels <- reg_results_list$Labels$predictor_labels

  if (is.null(interpretations)) {
    # Auto-generate interpretations
    interpretations <- purrr::map_chr(predictors, \(p) {
      bivar <- reg_results_list$Bivariate[[p]]
      p_idx <- which(predictors == p)
      p_type <- reg_results_list$Labels$predictor_types[p_idx]
      p_label <- predictor_labels[p_idx]
      criterion_label <- reg_results_list$Labels$criterion_label

      if (!bivar$significant) {
        return(paste0(p_label, " is not correlated with ", criterion_label))
      }

      if (p_type == "Binary") {
        direction <- dplyr::if_else(
          bivar$r > 0,
          paste0("Higher coded group tends to have higher ", criterion_label, " scores"),
          paste0("Higher coded group tends to have lower ", criterion_label, " scores")
        )
        return(direction)
      } else {
        direction <- dplyr::if_else(bivar$r > 0, "increase", "decrease")
        return(paste0("As ", p_label, " increases, ", criterion_label, " scores tend to ", direction))
      }
    })
    names(interpretations) <- predictors
  } else {
    interpretations <- purrr::map_chr(predictors, \(p) interpretations[[p]])
  }

  if (KEY) {
    data <- tibble::tibble(
      Predictor = predictor_labels,
      Interpretation = interpretations
    )
  } else {
    data <- tibble::tibble(
      Predictor = predictor_labels,
      Interpretation = rep("", length(predictors))
    )
  }

  ft <- flextable::flextable(data) |>
    flextable::width(j = 1, width = 2) |>
    flextable::width(j = 2, width = 4.5) |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::align(j = 2, align = "left", part = "all") |>
    flextable::fontsize(size = 11, part = "all") |>
    flextable::border_remove() |>
    flextable::hline_top(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "body", border = officer::fp_border(width = 2))

  if (KEY) {
    ft <- ft |>
      flextable::color(j = 2, color = "red", part = "body")
  }

  return(ft)
}


#' @rdname create_linear_reg_correlation_interp_table
#' @param ... Arguments passed to [create_linear_reg_correlation_interp_table()].
#' @export
create_linear_reg_correlation_interp_ft <- function(...) {
  .Deprecated("create_linear_reg_correlation_interp_table")
  create_linear_reg_correlation_interp_table(...)
}


#' Correlation Interpretation Table (Flextable)
#'
#' Creates a flextable with one row per predictor showing the bivariate
#' correlation interpretation. Auto-generates interpretations or uses
#' custom-provided ones. Answer key uses red text.
#'
#' @param reg_results_list Output from \code{linear_reg_answers()}.
#' @param interpretations Named list or \code{NULL}. Custom interpretations
#'   keyed by predictor variable name. If \code{NULL}, auto-generated.
#' @param KEY Logical. If \code{TRUE} (default), show filled interpretations
#'   (red text). If \code{FALSE}, blank cells.
#'
#' @return A \code{\link[flextable]{flextable}} object.
#'
#' @examples
#' data(superman, package = "psych350data")
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- linear_reg_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_correlation_interp_table(result, KEY = TRUE)
#' create_correlation_interp_table(result, KEY = FALSE)
#'
#' @export
create_correlation_interp_table <- function(reg_results_list,
                                            interpretations = NULL,
                                            KEY = TRUE) {

  predictors <- reg_results_list$Labels$predictors
  predictor_labels <- reg_results_list$Labels$predictor_labels

  if (is.null(interpretations)) {
    # Auto-generate interpretations
    interpretations <- sapply(predictors, function(p) {
      bivar <- reg_results_list$Bivariate[[p]]
      p_type <- reg_results_list$Labels$predictor_types[which(predictors == p)]
      p_label <- predictor_labels[which(predictors == p)]
      criterion_label <- reg_results_list$Labels$criterion_label

      if (!bivar$significant) {
        return(paste0(p_label, " is not correlated with ", criterion_label))
      }

      if (p_type == "Binary") {
        if (bivar$r > 0) {
          return(paste0("Higher coded group tends to have higher ",
                        criterion_label, " scores"))
        } else {
          return(paste0("Higher coded group tends to have lower ",
                        criterion_label, " scores"))
        }
      } else {
        direction <- ifelse(bivar$r > 0, "increase", "decrease")
        return(paste0("As ", p_label, " increases, ",
                      criterion_label, " scores tend to ", direction))
      }
    })
  } else {
    interpretations <- sapply(predictors, function(p) interpretations[[p]])
  }

  if (KEY) {
    data <- data.frame(
      Predictor = predictor_labels,
      Interpretation = interpretations,
      stringsAsFactors = FALSE
    )
  } else {
    data <- data.frame(
      Predictor = predictor_labels,
      Interpretation = rep("", length(predictor_labels)),
      stringsAsFactors = FALSE
    )
  }

  ft <- flextable::flextable(data) |>
    flextable::width(j = 1, width = 2) |>
    flextable::width(j = 2, width = 5) |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::align(j = 2, align = "left", part = "body") |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::padding(padding = 3, part = "all") |>
    flextable::border_outer(border = officer::fp_border(width = 2), part = "all") |>
    flextable::border_inner_h(border = officer::fp_border(width = 1), part = "all") |>
    flextable::border_inner_v(border = officer::fp_border(width = 1), part = "all") |>
    flextable::hline_top(border = officer::fp_border(width = 2), part = "header")

  if (KEY) {
    ft <- ft |> flextable::color(j = 2, color = "red", part = "body")
  }

  return(ft)
}


#' @rdname create_correlation_interp_table
#' @param ... Arguments passed to [create_correlation_interp_table()].
#' @export
create_correlation_interp_ft <- function(...) {
  .Deprecated("create_correlation_interp_table")
  create_correlation_interp_table(...)
}


#' Regression Weight Interpretation Table (Flextable)
#'
#' Creates a flextable with one row per predictor showing the regression
#' weight (b) interpretation. Auto-generates interpretations or uses
#' custom-provided ones. Answer key uses red text.
#'
#' @param reg_results_list Output from \code{linear_reg_answers()}.
#' @param interpretations Named list or \code{NULL}. Custom interpretations
#'   keyed by predictor variable name. If \code{NULL}, auto-generated.
#' @param KEY Logical. If \code{TRUE} (default), show filled interpretations
#'   (red text). If \code{FALSE}, blank cells.
#'
#' @return A \code{\link[flextable]{flextable}} object.
#'
#' @examples
#' data(superman, package = "psych350data")
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- linear_reg_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_regwt_interp_table(result, KEY = TRUE)
#' create_regwt_interp_table(result, KEY = FALSE)
#'
#' @export
create_regwt_interp_table <- function(reg_results_list,
                                      interpretations = NULL,
                                      KEY = TRUE) {

  predictors <- reg_results_list$Labels$predictors
  predictor_labels <- reg_results_list$Labels$predictor_labels

  if (is.null(interpretations)) {
    interpretations <- sapply(predictors, function(p) {
      regwt <- reg_results_list$Regression_Weights[[p]]
      p_type <- reg_results_list$Labels$predictor_types[which(predictors == p)]
      p_label <- predictor_labels[which(predictors == p)]
      criterion_label <- reg_results_list$Labels$criterion_label

      if (!regwt$significant) {
        return(paste0(p_label, " does not contribute to the model"))
      }

      if (p_type == "Binary") {
        direction <- ifelse(regwt$b > 0, "higher", "lower")
        return(paste0("Higher coded group has ", criterion_label, " scores ",
                      abs(regwt$b), " ", direction,
                      " than lower coded group, after controlling for all other variables"))
      } else {
        direction <- ifelse(regwt$b > 0, "increase", "decrease")
        return(paste0("For each 1-unit increase in ", p_label, ", ",
                      criterion_label, " is expected to ", direction, " by ",
                      abs(regwt$b),
                      ", after controlling for all other variables in the model"))
      }
    })
  } else {
    interpretations <- sapply(predictors, function(p) interpretations[[p]])
  }

  if (KEY) {
    data <- data.frame(
      Predictor = predictor_labels,
      Interpretation = interpretations,
      stringsAsFactors = FALSE
    )
  } else {
    data <- data.frame(
      Predictor = predictor_labels,
      Interpretation = rep("", length(predictor_labels)),
      stringsAsFactors = FALSE
    )
  }

  ft <- flextable::flextable(data) |>
    flextable::width(j = 1, width = 2) |>
    flextable::width(j = 2, width = 5) |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::align(j = 2, align = "left", part = "body") |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::padding(padding = 3, part = "all") |>
    flextable::border_outer(border = officer::fp_border(width = 2), part = "all") |>
    flextable::border_inner_h(border = officer::fp_border(width = 1), part = "all") |>
    flextable::border_inner_v(border = officer::fp_border(width = 1), part = "all") |>
    flextable::hline_top(border = officer::fp_border(width = 2), part = "header")

  if (KEY) {
    ft <- ft |> flextable::color(j = 2, color = "red", part = "body")
  }

  return(ft)
}


#' @rdname create_regwt_interp_table
#' @param ... Arguments passed to [create_regwt_interp_table()].
#' @export
create_regwt_interp_ft <- function(...) {
  .Deprecated("create_regwt_interp_table")
  create_regwt_interp_table(...)
}


###############################################################################
# REGRESSION TABLES - WEBEXERCISE / TINYTABLE INTERACTIVE CHECKERS
###############################################################################

#' Category Legend
#'
#' Returns a markdown-formatted legend explaining the bivariate/multivariate
#' results categories and significance key.
#'
#' @return A character string with markdown formatting.
#'
#' @examples
#' cat(regression_category_legend())
#'
#' @export
regression_category_legend <- function() {
  legend_text <- paste0(
    "**Bivariate/Multivariate Results Categories:**\n\n",
    "- **a.** Neither r nor b significant\n",
    "- **b.** r & b both significant & same sign\n",
    "- **c.** r significant but not b\n",
    "- **d.** suppressor effect\n\n",
    "**Significance Key:** ns = p > .05, * = p < .05, ** = p < .01, *** = p < .001\n"
  )
  return(legend_text)
}

