# =============================================================================
# Answer KEY Table & APA Descriptive Table (Blank / Filled)
# =============================================================================

#' Descriptive Statistics Answer KEY Table
#'
#' Creates a flextable showing the correct Mean, SD, SEM, and interpretability
#' for each variable. Used as a printable answer KEY for the worksheet
#' (Word/PDF output). For the interactive HTML checker version, use
#' \code{create_stats_table()} instead.
#'
#' @param vars Character vector. Variable names to include, in display order.
#' @param stats_data A data frame with columns `variable`, `mean`, `sd`, and
#'   `sem` -- typically the output of [compute_summary_stats()].
#' @param label Character vector or `NULL`. Variables that are IDs/labels.
#' @param quantitative Character vector or `NULL`. Continuous variables.
#' @param binary Character vector or `NULL`. Dichotomous variables.
#' @param multi_category Character vector or `NULL`. Nominal variables (3+ levels).
#' @param KEY Logical. If `TRUE` (default), fill with computed values.
#'   If `FALSE`, create a blank template with empty cells.
#'
#' @return A [flextable::flextable()] object.
#'
#' @examples
#' data(superman, package = "psych350data")
#' library(dplyr)
#'
#' my_data <- superman |>
#'   select(num, year, type, clark_height_in, clark_grp,
#'          height_diff, height_gap) |>
#'   filter(year > 1975)
#'
#' stats <- compute_summary_stats(my_data)
#'
#' # Answer KEY
#' create_answer_table(
#'   vars = names(my_data), stats_data = stats,
#'   label = "num", binary = "clark_grp",
#'   multi_category = c("type", "height_gap")
#' )
#'
#' # Blank version
#' create_answer_table(
#'   vars = names(my_data), stats_data = stats, KEY = FALSE
#' )
#'
#' @export
create_answer_table <- function(vars,
                                stats_data,
                                label          = NULL,
                                quantitative   = NULL,
                                binary         = NULL,
                                multi_category = NULL,
                                KEY            = TRUE) {

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

  if (KEY) {
    table_data <- stats_data |>
      dplyr::filter(.data$variable %in% vars) |>
      dplyr::slice(match(vars, .data$variable)) |>
      dplyr::mutate(
        Interpretable = purrr::map_chr(variable, \(v) {
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


#' APA Descriptive Statistics Table
#'
#' Creates an APA-formatted descriptive statistics table. Continuous variables
#' show M(SD) and n; categorical variables show level frequencies and
#' percentages. Produces either a filled answer KEY or blank student worksheet.
#'
#' @param data A data frame. Required when `KEY = TRUE`.
#' @param show_sem Logical or `NULL`. If `TRUE`, adds a SEM column for
#'   continuous variables. Default `NULL` (no SEM column).
#' @param continuous Character vector or `NULL`. Continuous variable names.
#' @param categorical Character vector or `NULL`. Categorical variable names.
#' @param stats_data Output from [compute_summary_stats()]. Required when
#'   `KEY = TRUE`.
#' @param var_labels Named character vector or `NULL`. Display labels,
#'   e.g. `c(height_diff = "Height Difference")`.
#' @param KEY Logical. If `TRUE` (default), fill with computed values.
#'   If `FALSE`, create a blank worksheet template.
#' @param table_title Character or `NULL`. Optional caption.
#' @param footnote Character or `NULL`. Footer note. Defaults to APA-style
#'   M/SD/N explanation.
#' @param digits Integer. Decimal places for M and SD. Default 2.
#'
#' @return A flextable object.
#' @export
create_descriptive_table <- function(data        = NULL,
                                     show_sem    = NULL,
                                     continuous  = NULL,
                                     categorical = NULL,
                                     stats_data  = NULL,
                                     var_labels  = NULL,
                                     KEY         = TRUE,
                                     table_title = NULL,
                                     footnote    = NULL,
                                     digits      = 2) {

  if (KEY && is.null(data))       stop("`data` is required when KEY = TRUE.")
  if (KEY && is.null(stats_data)) stop("`stats_data` is required when KEY = TRUE.")

  if (is.null(footnote)) {
    footnote <- "Note. M = Mean; SD = Standard Deviation; N = Sample Size."
  }

  get_label <- function(v) {
    if (!is.null(var_labels) && v %in% names(var_labels)) unname(var_labels[v]) else v
  }

  # Helper: resolve display levels for a categorical variable.
  # Prefers _label column (from prep_data keep_labels = TRUE), then
  # factor levels, then sorted unique values.
  get_levels <- function(data, var) {
    label_col <- paste0(var, "_label")
    if (!is.null(data) && label_col %in% names(data)) {
      sort(unique(stats::na.omit(data[[label_col]])))
    } else if (!is.null(data) && is.factor(data[[var]])) {
      levels(data[[var]])
    } else if (!is.null(data) && var %in% names(data)) {
      sort(unique(stats::na.omit(data[[var]])))
    } else {
      paste("Level", 1:3)
    }
  }

  fmt <- function(x) format(round(x, digits), nsmall = digits)

  table_rows <- list()

  if (KEY) {

    # Continuous: M(SD) | n
    if (!is.null(continuous)) {
      cont_stats <- stats_data |>
        dplyr::filter(.data$variable %in% continuous) |>
        dplyr::slice(match(continuous, .data$variable))

      for (i in seq_len(nrow(cont_stats))) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
          Variable = get_label(cont_stats$variable[i]),
          `M(SD)`  = paste0(fmt(cont_stats$mean[i]), " (", fmt(cont_stats$sd[i]), ")"),
          `N(%)`   = as.character(cont_stats$n[i]),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }
    }

    # Categorical: header row + one row per level with n(%)
    if (!is.null(categorical)) {
      for (var in categorical) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
          Variable = get_label(var), `M(SD)` = "", `N(%)` = "",
          check.names = FALSE, stringsAsFactors = FALSE
        )

        freq_data <- data |>
          dplyr::count(.data[[var]], name = "freq") |>
          dplyr::filter(!is.na(.data[[var]])) |>
          dplyr::mutate(pct = round((freq / sum(freq)) * 100, 1))

        # Use label column for display if available
        label_col <- paste0(var, "_label")
        if (label_col %in% names(data)) {
          level_labels <- data |>
            dplyr::distinct(.data[[var]], .data[[label_col]]) |>
            dplyr::filter(!is.na(.data[[var]])) |>
            dplyr::arrange(.data[[var]])
          freq_data <- freq_data |>
            dplyr::left_join(level_labels, by = var)
          display_col <- label_col
        } else {
          display_col <- var
        }

        for (j in seq_len(nrow(freq_data))) {
          table_rows[[length(table_rows) + 1]] <- data.frame(
            Variable = paste0("  ", freq_data[[display_col]][j]),
            `M(SD)`  = "",
            `N(%)`   = paste0(freq_data$freq[j], " (", freq_data$pct[j], "%)"),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }
      }
    }

  } else {

    # Blank continuous rows
    if (!is.null(continuous)) {
      for (var in continuous) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
          Variable = get_label(var), `M(SD)` = "___", `N(%)` = "___",
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }
    }

    # Blank categorical rows
    if (!is.null(categorical)) {
      for (var in categorical) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
          Variable = get_label(var), `M(SD)` = "", `N(%)` = "",
          check.names = FALSE, stringsAsFactors = FALSE
        )

        levels_vec <- get_levels(data, var)

        for (lv in levels_vec) {
          table_rows[[length(table_rows) + 1]] <- data.frame(
            Variable = paste0("  ", lv), `M(SD)` = "", `N(%)` = "___",
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  table_data <- dplyr::bind_rows(table_rows)

  ft <- flextable::flextable(table_data) |>
    apa_style_table() |>
    flextable::align(j = 2:3, align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::width(j = 1, width = 2.5) |>
    flextable::width(j = 2, width = 1.5) |>
    flextable::width(j = 3, width = 1.5) |>
    flextable::add_footer_lines(footnote) |>
    flextable::italic(italic = TRUE, part = "footer") |>
    flextable::align(align = "left", part = "footer")

  if (!is.null(table_title)) {
    ft <- flextable::set_caption(ft, caption = table_title)
  }

  ft
}
