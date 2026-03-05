# =============================================================================
# Functions that print out APA-Style Write-Ups based on RH and results_list variables for CORR, CHI-SQ, ANOVA
# =============================================================================


# =============================================================================
# CORRELATION WRITE-UP
# =============================================================================

#' APA Write-Up for Pearson Correlation
#'
#' Generates an APA-style paragraph reporting a Pearson correlation result,
#' with hypothesis-driven conclusions that compare expected vs. actual results.
#'
#' @param corr_results_list Output from [corr_answers()].
#' @param var1_label Character string. Descriptive label for variable 1
#'   (e.g., "age of Clark Kent").
#' @param var2_label Character string. Descriptive label for variable 2
#'   (e.g., "age of Lois Lane").
#' @param hypothesis List with hypothesis details:
#'   \describe{
#'     \item{direction}{Expected direction: "positive", "negative", or "none"}
#'     \item{rh_text}{Optional. Full RH statement for custom phrasing.}
#'   }
#'   If NULL, generates a generic non-directional APA writeup.
#' @param alpha Significance level. Default 0.05.
#' @param include_descriptives Logical. Include M and SD. Default TRUE.
#' @param table_number Integer or NULL. Table reference if descriptives excluded.
#'
#' @return A character string with the APA write-up.
#'
#' @examples
#' \dontrun{
#' result <- corr_answers(superman, "clark_age", "lois_age")
#'
#' # With directional hypothesis
#' writeup <- apa_corr_writeup(
#'   result,
#'   var1_label = "age of Clark Kent",
#'   var2_label = "age of Lois Lane",
#'   hypothesis = list(
#'     direction = "negative",
#'     rh_text = "media with a younger Clark Kent would have older Lois Lane"
#'   )
#' )
#' cat(writeup)
#' }
#'
#' @export
apa_corr_writeup <- function(corr_results_list,
                             var1_label,
                             var2_label,
                             hypothesis = NULL,
                             alpha = 0.05,
                             include_descriptives = TRUE,
                             table_number = NULL) {

  # Extract raw values
  r <- corr_results_list$Correlation$r
  p <- corr_results_list$Correlation$p_value
  df <- corr_results_list$Correlation$df

  desc_stats <- corr_results_list$Descriptives

  # Get descriptives by position (more reliable than name matching)
  var1_stats <- desc_stats[1, ]
  var2_stats <- desc_stats[2, ]

  # Format statistics
  r_text <- format_r(r)
  p_text <- format_p_value(p)
  df_text <- format_df(df)

  # Determine actual results
  is_significant <- p < alpha
  actual_direction <- if (r > 0) "positive" else if (r < 0) "negative" else "none"

  # Build descriptives phrase
  if (include_descriptives) {
    m1 <- format_mean(var1_stats$mean)
    sd1 <- format_sd(var1_stats$sd)
    m2 <- format_mean(var2_stats$mean)
    sd2 <- format_sd(var2_stats$sd)

    desc_phrase <- glue::glue(
      "{var1_label} (*M* = {m1}, *SD* = {sd1}) and {var2_label} (*M* = {m2}, *SD* = {sd2})"
    )
  } else {
    desc_phrase <- glue::glue("{var1_label} and {var2_label}")
    if (!is.null(table_number)) {
      desc_phrase <- paste0(desc_phrase, " (see Table ", table_number, ")")
    }
  }

  # Build the main result sentence
  if (is_significant) {
    sig_phrase <- "a significant"
    direction_word <- if (r > 0) "positive" else "negative"
    main_sentence <- glue::glue(
      "There was {sig_phrase} {direction_word} linear relationship between {desc_phrase}, ",
      "*r*({df_text}) = {r_text}, *p* = {p_text}."
    )
  } else {
    main_sentence <- glue::glue(
      "There was no significant linear relationship between {desc_phrase}, ",
      "*r*({df_text}) = {r_text}, *p* = {p_text}."
    )
  }

  # Build hypothesis conclusion
  if (!is.null(hypothesis)) {
    expected_direction <- hypothesis$direction
    rh_text <- hypothesis$rh_text

    # Determine if hypothesis is supported
    hypothesis_supported <- FALSE

    if (expected_direction == "none") {
      # Hypothesis predicts no relationship
      hypothesis_supported <- !is_significant
    } else {
      # Hypothesis predicts a directional relationship
      # Supported only if significant AND in the expected direction
      hypothesis_supported <- is_significant && (actual_direction == expected_direction)
    }

    # Build conclusion text
    if (hypothesis_supported) {
      if (!is.null(rh_text)) {
        conclusion <- glue::glue(
          "This result supports the research hypothesis that {rh_text}."
        )
      } else {
        if (expected_direction == "none") {
          conclusion <- "This result supports the research hypothesis that there would be no significant relationship between these variables."
        } else {
          conclusion <- glue::glue(
            "This result supports the research hypothesis that there would be a significant {expected_direction} relationship between these variables."
          )
        }
      }
    } else {
      if (!is.null(rh_text)) {
        conclusion <- glue::glue(
          "This result does not support the research hypothesis that {rh_text}."
        )
      } else {
        if (expected_direction == "none") {
          conclusion <- "This result does not support the research hypothesis that there would be no significant relationship between these variables."
        } else {
          conclusion <- glue::glue(
            "This result does not support the research hypothesis that there would be a significant {expected_direction} relationship between these variables."
          )
        }
      }
    }

    writeup <- paste(main_sentence, conclusion)
  } else {
    writeup <- as.character(main_sentence)
  }

  writeup
}


