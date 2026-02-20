# =============================================================================
# Answer Key Table & APA Descriptive Table (Blank / Filled)
# =============================================================================

#' Descriptive Statistics Answer Key Table
#'
#' Creates a flextable showing the correct Mean, SD, SEM, and interpretability
#' for each variable. Used as a printable answer key for the worksheet
#' (Word/PDF output). For the interactive HTML checker version, use
#' \code{create_stats_table()} instead.
#'
#' @param vars Character vector. Variable names to include, in display order.
#' @param stats_data A data frame with columns `variable`, `mean`, `sd`, and
#'   `sem` — typically the output of [compute_summary_stats()].
#' @param label Character vector or `NULL`. Variables that are IDs/labels.
#' @param quantitative Character vector or `NULL`. Continuous variables.
#' @param binary Character vector or `NULL`. Dichotomous variables.
#' @param multi_category Character vector or `NULL`. Nominal variables (3+ levels).
#' @param Key Logical. If `TRUE` (default), fill with computed values.
#'   If `FALSE`, create a blank template with empty cells.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman)
#' library(dplyr)
#'
#' my_data <- superman |>
#'   select(num, year, type, clark_height_in, clark_grp,
#'          height_diff, height_gap) |>
#'   filter(year > 1975)
#'
#' stats <- compute_summary_stats(my_data)
#'
#' # Answer key
#' create_answer_table(
#'   vars = names(my_data), stats_data = stats,
#'   label = "num", binary = "clark_grp",
#'   multi_category = c("type", "height_gap")
#' )
#'
#' # Blank version
#' create_answer_table(
#'   vars = names(my_data), stats_data = stats, Key = FALSE
#' )
#'
#' @export
create_answer_table <- function(vars,
                                stats_data,
                                label          = NULL,
                                quantitative   = NULL,
                                binary         = NULL,
                                multi_category = NULL,
                                Key            = TRUE) {

  # Build type lookup
  answer_text <- c(
    label          = "No - this is a label, not a variable",
    quantitative   = "Yes - this is a quantitative variable",
    binary         = "Yes - this is a binary variable",
    multi_category = "No - this is a multiple-category variable"
  )

  var_type_map <- c(
    stats::setNames(rep("label",          length(label)),          label),
    stats::setNames(rep("quantitative",   length(quantitative)),   quantitative),
    stats::setNames(rep("binary",         length(binary)),         binary),
    stats::setNames(rep("multi_category", length(multi_category)), multi_category)
  )

  if (Key) {
    table_data <- stats_data |>
      dplyr::filter(.data$variable %in% vars) |>
      dplyr::slice(match(vars, .data$variable)) |>
      dplyr::mutate(
        Interpretable = purrr::map_chr(.data$variable, function(v) {
          vtype <- if (v %in% names(var_type_map)) var_type_map[v] else "quantitative"
          answer_text[vtype]
        })
      ) |>
      dplyr::select(
        Variable         = "variable",
        Mean             = "mean",
        SD               = "sd",
        SEM              = "sem",
        `Interpretable?` = "Interpretable"
      )
  } else {
    table_data <- data.frame(
      Variable         = vars,
      Mean             = rep("", length(vars)),
      SD               = rep("", length(vars)),
      SEM              = rep("", length(vars)),
      `Interpretable?` = rep("", length(vars)),
      check.names      = FALSE
    )
  }

  apa_table <- table_data |>
    flextable::flextable() |>
    flextable::theme_zebra() |>
    flextable::autofit() |>
    flextable::align(align = "left", part = "all") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::set_table_properties(layout = "autofit", width = 0.8)

  return(apa_table)
}


#' APA Descriptive Statistics Table (Table 1 Style)
#'
#' Creates an APA-formatted descriptive statistics table showing M(SD) for
#' continuous variables and frequency counts with percentages for categorical
#' variables. Can produce either a filled answer key or a blank student
#' worksheet.
#'
#' @param data A data frame containing the raw data. Required when
#'   \code{Key = TRUE} to compute frequencies for categorical variables.
#'   Can be \code{NULL} when \code{Key = FALSE}.
#' @param continuous Character vector or \code{NULL}. Names of continuous
#'   variables. These show M(SD) and n.
#' @param categorical Character vector or \code{NULL}. Names of categorical
#'   variables. These show level labels and counts with percentages.
#' @param stats_data A data frame with columns \code{variable}, \code{mean},
#'   \code{sd}, \code{n}. Typically the output of
#'   \code{compute_summary_stats()}. Required when \code{Key = TRUE}.
#' @param var_labels Named character vector or \code{NULL}. Optional display
#'   labels, e.g. \code{c(height_diff = "Height Difference")}.
#' @param Key Logical. If \code{TRUE} (default), fill with computed values.
#'   If \code{FALSE}, create a blank template.
#' @param table_title Character or \code{NULL}. Optional title printed above
#'   the table.
#' @param footnote Character or \code{NULL}. Footnote text. If \code{NULL},
#'   a default APA-style note is used.
#' @param digits Integer. Decimal places for M and SD. Default is 2.
#'
#' @return A \code{\link[flextable]{flextable}} object.
#'
#' @examples
#' data(superman)
#'
#' my_data <- superman[!is.na(superman$height_diff),
#'   c("clark_height_in", "height_diff", "clark_grp", "height_gap")]
#'
#' stats <- compute_summary_stats(my_data)
#'
#' # Filled answer key
#' create_descriptive_table(
#'   data        = my_data,
#'   continuous  = c("height_diff"),
#'   categorical = c("clark_grp", "height_gap"),
#'   stats_data  = stats,
#'   Key = TRUE
#' )
#'
#' # Blank worksheet
#' create_descriptive_table(
#'   data        = my_data,
#'   continuous  = c("height_diff"),
#'   categorical = c("clark_grp", "height_gap"),
#'   Key = FALSE
#' )
#'
#' @export
create_descriptive_table <- function(data           = NULL,
                                     continuous     = NULL,
                                     categorical    = NULL,
                                     stats_data     = NULL,
                                     var_labels     = NULL,
                                     Key            = TRUE,
                                     table_title    = NULL,
                                     footnote       = NULL,
                                     digits         = 2) {

  # Default footnote
  if (is.null(footnote)) {
    footnote <- paste0(
      "Note. M = Mean; SD = Standard Deviation; ",
      "N = Sample Size. See APA manual p. 210."
    )
  }

  # Helper: get display label for a variable
  get_label <- function(v) {
    if (!is.null(var_labels) && v %in% names(var_labels)) {
      return(unname(var_labels[v]))
    }
    return(v)
  }

  table_rows <- list()

  if (Key) {
    # ==== FILLED TABLE ====
    if (is.null(data)) {
      stop("`data` is required when Key = TRUE.")
    }
    if (is.null(stats_data)) {
      stop("`stats_data` is required when Key = TRUE.")
    }

    # -- Continuous variables: M(SD) | n --
    if (!is.null(continuous)) {
      cont_stats <- stats_data |>
        dplyr::filter(.data$variable %in% continuous) |>
        dplyr::slice(match(continuous, .data$variable))

      for (i in seq_len(nrow(cont_stats))) {
        m_val  <- format(round(cont_stats$mean[i], digits), nsmall = digits)
        sd_val <- format(round(cont_stats$sd[i], digits), nsmall = digits)

        table_rows[[length(table_rows) + 1]] <- data.frame(
          Variable = get_label(cont_stats$variable[i]),
          M_SD     = paste0(m_val, " (", sd_val, ")"),
          N_Pct    = as.character(cont_stats$n[i]),
          stringsAsFactors = FALSE
        )
      }
    }

    # -- Categorical variables: header row + level rows with n(%) --
    if (!is.null(categorical)) {
      for (var in categorical) {
        # Variable header row
        table_rows[[length(table_rows) + 1]] <- data.frame(
          Variable = get_label(var),
          M_SD     = "",
          N_Pct    = "",
          stringsAsFactors = FALSE
        )

        # Compute frequencies and percentages
        freq_data <- data |>
          dplyr::count(!!rlang::sym(var), name = "freq") |>
          dplyr::filter(!is.na(!!rlang::sym(var))) |>
          dplyr::mutate(
            pct = round((.data$freq / sum(.data$freq)) * 100, 1)
          )

        # One row per level
        for (j in seq_len(nrow(freq_data))) {
          table_rows[[length(table_rows) + 1]] <- data.frame(
            Variable = "
",
M_SD     = paste0("Level ", freq_data[[var]][j]),
N_Pct    = paste0(freq_data$freq[j],
                  " (", freq_data$pct[j], "%)"),
stringsAsFactors = FALSE
          )
        }
      }
    }

  } else {
    # ==== BLANK TABLE ====

    # -- Continuous: placeholder rows --
    if (!is.null(continuous)) {
      for (var in continuous) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
          Variable = get_label(var),
          M_SD     = "xx(xx)",
          N_Pct    = "freq",
          stringsAsFactors = FALSE
        )
      }
    }

    # -- Categorical: header + generic level rows --
    if (!is.null(categorical)) {
      for (var in categorical) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
          Variable = get_label(var),
          M_SD     = "",
          N_Pct    = "",
          stringsAsFactors = FALSE
        )

        # Determine number of level placeholder rows
        if (!is.null(data) && var %in% names(data)) {
          n_levels <- length(unique(stats::na.omit(data[[var]])))
        } else {
          n_levels <- 3
        }

        for (j in seq_len(n_levels)) {
          table_rows[[length(table_rows) + 1]] <- data.frame(
            Variable = "",
            M_SD     = paste0("Level ", j),
            N_Pct    = "freq(%)",
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  # ==== COMBINE ROWS ====
  table_data <- dplyr::bind_rows(table_rows)

  # ==== BUILD FLEXTABLE ====
  apa_table <- table_data |>
    flextable::flextable() |>
    flextable::set_header_labels(
      Variable = "Variable",
      M_SD     = "M(SD)",
      N_Pct    = "N(%)"
    ) |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::align(j = 2:3, align = "center", part = "body") |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(j = 1, align = "left", part = "header") |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 12, part = "all") |>
    flextable::italic(j = 2, italic = TRUE, part = "header") |>
    flextable::border_remove() |>
    flextable::hline_top(
      part   = "header",
      border = officer::fp_border(width = 2)
    ) |>
    flextable::hline_bottom(
      part   = "header",
      border = officer::fp_border(width = 1)
    ) |>
    flextable::hline_bottom(
      part   = "body",
      border = officer::fp_border(width = 2)
    ) |>
    flextable::width(j = 1, width = 2.5) |>
    flextable::width(j = 2, width = 1.5) |>
    flextable::width(j = 3, width = 1.5) |>
    flextable::padding(padding = 3, part = "all") |>
    flextable::set_table_properties(layout = "autofit", width = 1) |>
    flextable::add_footer_lines(footnote) |>
    flextable::italic(italic = TRUE, part = "footer") |>
    flextable::align(align = "left", part = "footer") |>
    flextable::fontsize(size = 12, part = "footer")

  return(apa_table)
}
