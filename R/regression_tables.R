###############################################################################
# REGRESSION TABLES - FLEXTABLE-BASED WORKSHEET TABLES
###############################################################################

#' Model Statistics Line
#'
#' Returns a formatted text string with overall model statistics (R, R-squared,
#' F, df, p) as either a filled answer key or blank worksheet.
#'
#' @param reg_results_list Output from \code{regression_answers()}.
#' @param KEY Logical. If \code{TRUE} (default), show filled values.
#'   If \code{FALSE}, show blanks.
#' @param highlight Logical. If \code{TRUE} (default) and \code{KEY = TRUE},
#'   wrap values in highlight markup for Quarto/RMarkdown.
#'
#' @return A character string.
#'
#' @examples
#' data(superman)
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- regression_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' cat(regression_model_statistics(result, KEY = TRUE, highlight = FALSE))
#'
#' @export
regression_model_statistics <- function(reg_results_list,
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
      "R2 = ", hl(r_sq), "     ",
      "F = ", hl(f_stat), "     ",
      "df = ", hl(df1), ", ", hl(df2), "     ",
      "p ", hl(p_val_formatted), "\n"
    )
  } else {
    output <- paste0(
      "R = ______     ",
      "R2 = ______     ",
      "F = ______     ",
      "df = ____, ____     ",
      "p = ______\n"
    )
  }

  return(output)
}


#' Model Evaluation Text
#'
#' Returns formatted text for evaluating the multiple regression model:
#' (a) does the model work, and (b) how well does it work.
#' Either filled (answer key) or blank (worksheet).
#'
#' @param reg_results_list Output from \code{regression_answers()}.
#' @param KEY Logical. If \code{TRUE} (default), show filled answers.
#'   If \code{FALSE}, show blanks.
#' @param highlight Logical. If \code{TRUE} (default) and \code{KEY = TRUE},
#'   wrap values in highlight markup.
#'
#' @return A character string with markdown formatting.
#'
#' @examples
#' data(superman)
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- regression_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' cat(regression_model_evaluation(result, KEY = TRUE, highlight = FALSE))
#'
#' @export
regression_model_evaluation <- function(reg_results_list,
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
    model_works <- ifelse(p_val < 0.05, "Yes", "No")
    p_comparison <- ifelse(p_val < 0.05, "< .05", ">= .05")

    output <- paste0(
      "**a. Does the multiple regression model work? Where did you look to decide?**\n\n",
      hl(model_works), ", p ", hl(p_comparison),
      ", significant ANOVA (F = ", hl(f_stat),
      ", df = ", hl(df1), ", ", hl(df2),
      ", p ", hl(p_val_formatted), ")\n\n",
      "**b. How well does the multiple regression model work?**\n\n",
      "Accounts for ", hl(var_explained), "% of ", criterion_label,
      " variance (R2 = ", hl(r_sq), ")\n"
    )
  } else {
    output <- paste0(
      "**a. Does the multiple regression model work? Where did you look to decide?**\n\n",
      "\n\n",
      "**b. How well does the multiple regression model work?**\n\n",
      "\n\n"
    )
  }

  return(output)
}


#' Combined Predictor Results Table (Flextable)
#'
#' Creates a flextable with one row per predictor showing variable type,
#' bivariate correlation r (with p), regression weight b (with p), and
#' bivariate/multivariate interpretation category (a-d).
#'
#' @param reg_results_list Output from \code{regression_answers()}.
#' @param KEY Logical. If \code{TRUE} (default), fill with computed values.
#'   If \code{FALSE}, create a blank template (column widths are preserved).
#'
#' @return A \code{\link[flextable]{flextable}} object.
#'
#' @examples
#' data(superman)
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- regression_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_regression_combined_table(result, KEY = TRUE)
#' create_regression_combined_table(result, KEY = FALSE)
#'
#' @export
create_regression_combined_table <- function(reg_results_list, KEY = TRUE) {

  predictors <- reg_results_list$Labels$predictors
  predictor_labels <- reg_results_list$Labels$predictor_labels
  predictor_types <- reg_results_list$Labels$predictor_types

  if (KEY) {
    data_rows <- data.frame(
      Predictor = predictor_labels,
      Type = predictor_types,
      r_p = sapply(predictors, function(p) {
        bivar <- reg_results_list$Bivariate[[p]]
        paste0(sprintf("%.3f", bivar$r), " (", bivar$p_value_formatted, ")")
      }),
      b_p = sapply(predictors, function(p) {
        regwt <- reg_results_list$Regression_Weights[[p]]
        paste0(sprintf("%.3f", regwt$b), " (", regwt$p_value_formatted, ")")
      }),
      Result = sapply(predictors, function(p) {
        reg_results_list$Regression_Weights[[p]]$category
      }),
      stringsAsFactors = FALSE
    )
  } else {
    data_rows <- data.frame(
      Predictor = predictor_labels,
      Type = rep("", length(predictors)),
      r_p = rep("", length(predictors)),
      b_p = rep("", length(predictors)),
      Result = rep("", length(predictors)),
      stringsAsFactors = FALSE
    )
  }

  # Get criterion label for header
  criterion_label <- reg_results_list$Labels$criterion_label

  colnames(data_rows) <- c(
    "Predictor",
    "Is variable\nquant or binary?",
    paste0("r (p)\nWith ", criterion_label),
    "b (p)",
    paste0("**Bivariate/Multivariate Results Categories:**\n",
           "a. Neither r nor b significant\n",
           "b. r & b both sig & same sign\n",
           "c. r sig but not b\n",
           "d. suppressor effect")
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


#' Regression Table Legend
#'
#' Returns a formatted markdown string with the bivariate/multivariate
#' results category legend.
#'
#' @return A character string with markdown formatting.
#'
#' @examples
#' cat(regression_table_legend())
#'
#' @export
regression_table_legend <- function() {
  legend_text <- paste0(
    "**\\*\\*\\*Bivariate/Multivariate Results Categories:**  ",
    "a. Neither r nor b significant    ",
    "b. r & b both sig & same sign    ",
    "c. r sig but not b    ",
    "d. suppressor effect\n"
  )
  return(legend_text)
}


#' Correlation Interpretation Table (Flextable)
#'
#' Creates a flextable with one row per predictor showing the bivariate
#' correlation interpretation. Auto-generates interpretations or uses
#' custom-provided ones. Answer key uses red text.
#'
#' @param reg_results_list Output from \code{regression_answers()}.
#' @param interpretations Named list or \code{NULL}. Custom interpretations
#'   keyed by predictor variable name. If \code{NULL}, auto-generated.
#' @param KEY Logical. If \code{TRUE} (default), show filled interpretations
#'   (red text). If \code{FALSE}, blank cells.
#'
#' @return A \code{\link[flextable]{flextable}} object.
#'
#' @examples
#' data(superman)
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- regression_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_correlation_interp_ft(result, KEY = TRUE)
#' create_correlation_interp_ft(result, KEY = FALSE)
#'
#' @export
create_correlation_interp_ft <- function(reg_results_list,
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


#' Regression Weight Interpretation Table (Flextable)
#'
#' Creates a flextable with one row per predictor showing the regression
#' weight (b) interpretation. Auto-generates interpretations or uses
#' custom-provided ones. Answer key uses red text.
#'
#' @param reg_results_list Output from \code{regression_answers()}.
#' @param interpretations Named list or \code{NULL}. Custom interpretations
#'   keyed by predictor variable name. If \code{NULL}, auto-generated.
#' @param KEY Logical. If \code{TRUE} (default), show filled interpretations
#'   (red text). If \code{FALSE}, blank cells.
#'
#' @return A \code{\link[flextable]{flextable}} object.
#'
#' @examples
#' data(superman)
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- regression_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_regwt_interp_ft(result, KEY = TRUE)
#' create_regwt_interp_ft(result, KEY = FALSE)
#'
#' @export
create_regwt_interp_ft <- function(reg_results_list,
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


#' Interactive Model Summary Checker (Webexercise)
#'
#' Creates a tinytable with fill-in-the-blank and multiple choice inputs
#' for checking overall model statistics. Requires the \code{tinytable}
#' and \code{webexercises} packages.
#'
#' @param reg_results_list Output from \code{regression_answers()}.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' data(superman)
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- regression_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_model_summary_checker(result)
#' }
#'
#' @export
create_model_summary_checker <- function(reg_results_list) {

  if (!requireNamespace("tinytable", quietly = TRUE)) {
    stop("Package 'tinytable' is required. Install with install.packages('tinytable')")
  }
  if (!requireNamespace("webexercises", quietly = TRUE)) {
    stop("Package 'webexercises' is required. Install with install.packages('webexercises')")
  }

  r <- reg_results_list$Model$R
  r_sq <- reg_results_list$Model$R_squared
  f_stat <- reg_results_list$Model$F
  df1 <- reg_results_list$Model$df1
  df2 <- reg_results_list$Model$df2
  p_val_formatted <- reg_results_list$Model$p_value_formatted

  # MCQ for model significance
  if (reg_results_list$Model$p_value < 0.05) {
    model_works_mcq <- webexercises::mcq(c(answer = "Yes", "No"))
  } else {
    model_works_mcq <- webexercises::mcq(c("Yes", answer = "No"))
  }

  model_table <- tibble::tibble(
    ` ` = "Model Summary",
    `R` = webexercises::fitb(r),
    `R2` = webexercises::fitb(r_sq),   # temporary name
    `F` = webexercises::fitb(f_stat),
    `df1, df2` = paste0(webexercises::fitb(df1), ", ", webexercises::fitb(df2)),
    `p` = webexercises::fitb(p_val_formatted),
    `Does the model work?` = model_works_mcq
  )

  result_table <- tinytable::tt(model_table) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(j = 5,
                        bootstrap_css = "min-width: 120px; white-space: nowrap;") |>
    tinytable::style_tt(
      bootstrap_class = "table table-bordered table-sm",
      bootstrap_css_rule = "width: 95%; margin-left: auto; margin-right: auto;"
    )

  return(result_table)
}


#' Interactive Predictor Results Checker (Webexercise)
#'
#' Creates a tinytable with fill-in-the-blank inputs for r and b values,
#' multiple choice for significance levels and variable type, and
#' category selection for each predictor. Requires \code{tinytable}
#' and \code{webexercises}.
#'
#' @param reg_results_list Output from \code{regression_answers()}.
#' @param show_legend Logical. If \code{TRUE} (default), print a collapsible
#'   significance and category legend before the table.
#'
#' @return A tinytable object with embedded webexercise elements.
#'
#' @examples
#' \dontrun{
#' data(superman)
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- regression_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_predictor_checker(result)
#' }
#'
#' @export
create_predictor_checker <- function(reg_results_list, show_legend = TRUE) {

  if (!requireNamespace("tinytable", quietly = TRUE)) {
    stop("Package 'tinytable' is required. Install with install.packages('tinytable')")
  }
  if (!requireNamespace("webexercises", quietly = TRUE)) {
    stop("Package 'webexercises' is required. Install with install.packages('webexercises')")
  }

  predictors <- reg_results_list$Labels$predictors
  predictor_labels <- reg_results_list$Labels$predictor_labels
  predictor_types <- reg_results_list$Labels$predictor_types

  # Build predictor rows
  predictor_rows <- tibble::tibble(
    `Predictor` = character(),
    `Type` = character(),
    `r` = character(),
    `r sig` = character(),
    `b` = character(),
    `b sig` = character(),
    `Result` = character()
  )

  for (i in seq_along(predictors)) {
    p <- predictors[i]
    bivar <- reg_results_list$Bivariate[[p]]
    regwt <- reg_results_list$Regression_Weights[[p]]

    # Get the type for this predictor
    p_type <- predictor_types[i]
    if (is.na(p_type)) p_type <- predictor_types[p]

    # MCQ for variable type
    if (p_type == "Binary") {
      type_mcq <- webexercises::mcq(c(answer = "Binary", "Quant"))
    } else {
      type_mcq <- webexercises::mcq(c("Binary", answer = "Quant"))
    }

    # MCQ for r significance
    r_sig_mcq <- .sig_mcq(bivar$p_value)

    # MCQ for b significance
    b_sig_mcq <- .sig_mcq(regwt$p_value)

    # MCQ for interpretation category
    cat_choice <- regwt$category
    if (cat_choice == "a") {
      category_mcq <- webexercises::mcq(c(answer = "a", "b", "c", "d"))
    } else if (cat_choice == "b") {
      category_mcq <- webexercises::mcq(c("a", answer = "b", "c", "d"))
    } else if (cat_choice == "c") {
      category_mcq <- webexercises::mcq(c("a", "b", answer = "c", "d"))
    } else {
      category_mcq <- webexercises::mcq(c("a", "b", "c", answer = "d"))
    }

    new_row <- tibble::tibble(
      `Predictor` = predictor_labels[i],
      `Type` = type_mcq,
      `r` = webexercises::fitb(bivar$r),
      `r sig` = r_sig_mcq,
      `b` = webexercises::fitb(regwt$b),
      `b sig` = b_sig_mcq,
      `Result` = category_mcq
    )

    predictor_rows <- dplyr::bind_rows(predictor_rows, new_row)
  }

  # Print collapsible legend if requested
  if (show_legend) {
    cat(webexercises::hide("Click here for significance key"))
    cat("\n\n**Significance Key:**\n\n")
    cat("- **ns** = p > .05 (not significant)\n")
    cat("- **\\*** = p < .05\n")
    cat("- **\\*\\*** = p < .01\n")
    cat("- **\\*\\*\\*** = p < .001\n\n")
    cat("**Result Categories:**\n\n")
    cat("- **a** = Neither r nor b significant\n")
    cat("- **b** = r & b both significant & same sign\n")
    cat("- **c** = r significant but not b\n")
    cat("- **d** = suppressor effect\n")
    cat(webexercises::unhide())
    cat("\n\n")
  }

  result_table <- tinytable::tt(predictor_rows) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class = "table table-striped table-bordered table-sm",
      bootstrap_css_rule = "width: 95%; margin-left: auto; margin-right: auto;"
    )

  return(result_table)
}


#' Interactive Correlation Interpretations (Webexercise)
#'
#' Creates a tinytable showing correlation interpretation for each predictor,
#' either filled (answer key with red HTML) or blank. Requires \code{tinytable}.
#'
#' @param reg_results_list Output from \code{regression_answers()}.
#' @param interpretations Named list or \code{NULL}. Custom interpretations.
#' @param KEY Logical. If \code{TRUE} (default), show filled. If \code{FALSE}, blank.
#'
#' @return A tinytable object.
#'
#' @examples
#' \dontrun{
#' data(superman)
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- regression_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_correlation_interpretations(result, KEY = TRUE)
#' }
#'
#' @export
create_correlation_interpretations <- function(reg_results_list,
                                               interpretations = NULL,
                                               KEY = TRUE) {

  if (!requireNamespace("tinytable", quietly = TRUE)) {
    stop("Package 'tinytable' is required. Install with install.packages('tinytable')")
  }

  predictors <- reg_results_list$Labels$predictors
  predictor_labels <- reg_results_list$Labels$predictor_labels
  predictor_types <- reg_results_list$Labels$predictor_types
  criterion_label <- reg_results_list$Labels$criterion_label

  # Auto-generate interpretations if not provided
  if (is.null(interpretations)) {
    interpretations <- list()

    for (i in seq_along(predictors)) {
      p <- predictors[i]
      bivar <- reg_results_list$Bivariate[[p]]
      p_type <- predictor_types[i]

      if (bivar$significant) {
        if (p_type == "Binary") {
          if (bivar$r > 0) {
            interpretations[[p]] <- paste0("Higher coded group tends to have higher ",
                                           criterion_label, " scores")
          } else {
            interpretations[[p]] <- paste0("Higher coded group tends to have lower ",
                                           criterion_label, " scores")
          }
        } else {
          if (bivar$r > 0) {
            interpretations[[p]] <- paste0("As ", predictor_labels[i],
                                           " increases, ", criterion_label,
                                           " scores tend to increase")
          } else {
            interpretations[[p]] <- paste0("As ", predictor_labels[i],
                                           " increases, ", criterion_label,
                                           " scores tend to decrease")
          }
        }
      } else {
        interpretations[[p]] <- paste0(predictor_labels[i],
                                       " is not correlated with ", criterion_label)
      }
    }
  }

  if (KEY) {
    table_data <- tibble::tibble(
      `Predictor` = predictor_labels,
      `Interpretation` = sapply(predictors, function(p) {
        paste0('<span style="color: red;">', interpretations[[p]], '</span>')
      })
    )
  } else {
    table_data <- tibble::tibble(
      `Predictor` = predictor_labels,
      `Interpretation` = rep("", length(predictors))
    )
  }

  result_table <- tinytable::tt(table_data) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )

  return(result_table)
}


#' Interactive Regression Weight Interpretations (Webexercise)
#'
#' Creates a tinytable showing regression weight interpretation for each
#' predictor, either filled (answer key with red HTML) or blank.
#' Requires \code{tinytable}.
#'
#' @param reg_results_list Output from \code{regression_answers()}.
#' @param interpretations Named list or \code{NULL}. Custom interpretations.
#' @param KEY Logical. If \code{TRUE} (default), show filled. If \code{FALSE}, blank.
#'
#' @return A tinytable object.
#'
#' @examples
#' \dontrun{
#' data(superman)
#' sm <- superman[!is.na(superman$rt_critics_score) &
#'                     !is.na(superman$rt_audience_score), ]
#' result <- regression_answers(
#'   data = sm,
#'   criterion = "rt_critics_score",
#'   quant_predictors = c("clark_height_in", "rt_audience_score"),
#'   quant_labels = c("Height", "Audience"),
#'   criterion_label = "Critics Score"
#' )
#' create_regression_weight_interpretations(result, KEY = TRUE)
#' }
#'
#' @export
create_regression_weight_interpretations <- function(reg_results_list,
                                                     interpretations = NULL,
                                                     KEY = TRUE) {

  if (!requireNamespace("tinytable", quietly = TRUE)) {
    stop("Package 'tinytable' is required. Install with install.packages('tinytable')")
  }

  predictors <- reg_results_list$Labels$predictors
  predictor_labels <- reg_results_list$Labels$predictor_labels
  predictor_types <- reg_results_list$Labels$predictor_types
  criterion_label <- reg_results_list$Labels$criterion_label

  # Auto-generate interpretations if not provided
  if (is.null(interpretations)) {
    interpretations <- list()

    for (i in seq_along(predictors)) {
      p <- predictors[i]
      regwt <- reg_results_list$Regression_Weights[[p]]
      p_type <- predictor_types[i]

      if (!regwt$significant) {
        interpretations[[p]] <- paste0(predictor_labels[i],
                                       " does not contribute to the model")
      } else {
        if (p_type == "Binary") {
          if (regwt$b > 0) {
            interpretations[[p]] <- paste0(
              "Higher coded group has ", criterion_label,
              " scores ", abs(regwt$b),
              " higher than lower coded group, ",
              "after controlling for all other variables")
          } else {
            interpretations[[p]] <- paste0(
              "Higher coded group has ", criterion_label,
              " scores ", abs(regwt$b),
              " lower than lower coded group, ",
              "after controlling for all other variables")
          }
        } else {
          direction <- ifelse(regwt$b > 0, "increase", "decrease")
          interpretations[[p]] <- paste0(
            "For each 1-unit increase in ",
            predictor_labels[i], ", ", criterion_label,
            " is expected to ", direction, " by ",
            abs(regwt$b),
            ", after controlling for all other variables")
        }
      }
    }
  }

  if (KEY) {
    table_data <- tibble::tibble(
      `Predictor` = predictor_labels,
      `Interpretation` = sapply(predictors, function(p) {
        paste0('<span style="color: red;">', interpretations[[p]], '</span>')
      })
    )
  } else {
    table_data <- tibble::tibble(
      `Predictor` = predictor_labels,
      `Interpretation` = rep("", length(predictors))
    )
  }

  result_table <- tinytable::tt(table_data) |>
    tinytable::format_tt(escape = FALSE) |>
    tinytable::style_tt(
      bootstrap_class = "table table-striped table-hover table-sm",
      bootstrap_css_rule = "width: 90%; margin-left: auto; margin-right: auto;"
    )

  return(result_table)
}


###############################################################################
# INTERNAL HELPERS
###############################################################################

#' Convert p-value to significance stars (internal)
#'
#' @param p Numeric p-value.
#' @return Character string: "***", "**", "*", or "ns".
#' @noRd
.p_to_stars <- function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return("ns")
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

  if (stars == "ns") {
    return(webexercises::mcq(c(answer = "ns", "*", "**", "***")))
  } else if (stars == "*") {
    return(webexercises::mcq(c("ns", answer = "*", "**", "***")))
  } else if (stars == "**") {
    return(webexercises::mcq(c("ns", "*", answer = "**", "***")))
  } else {
    return(webexercises::mcq(c("ns", "*", "**", answer = "***")))
  }
}
