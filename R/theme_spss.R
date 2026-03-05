#' SPSS-Style ggplot2 Theme
#'
#' A complete ggplot2 theme that mimics the appearance of SPSS charts,
#' optimized for HTML output. Supports both modern (v24+) and legacy
#' (<v24) SPSS styles.
#'
#' @param base_size Numeric. Base font size in points. Default 14.
#' @param base_family Character. Base font family. Default `"sans"` (web-safe).
#' @param base_line_size Numeric. Base line width. Default 0.75.
#' @param base_rect_size Numeric. Base rectangle border width. Default 0.75.
#' @param version Character. `"modern"` (default) for SPSS v24+ style with
#'   white background and horizontal grid lines, or `"legacy"` for older SPSS
#'   style with grey background and box border.
#' @param scales Character. `"continuous"` (default) or `"discrete"` for both
#'   axes. Controls axis text sizing.
#' @param scale.x Character. Override scale type for x-axis only.
#' @param scale.y Character. Override scale type for y-axis only.
#'
#' @return A ggplot2 [ggplot2::theme()] object (complete theme).
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Basic usage with modern style
#' ggplot(superman, aes(x = clark_height, y = lois_height)) +
#'   geom_point() +
#'   theme_SPSS()
#'
#' # Legacy SPSS style
#' ggplot(superman, aes(x = clark_height, y = lois_height)) +
#'   geom_point() +
#'   theme_SPSS(version = "legacy")
#'
#' # With discrete x-axis
#' ggplot(superman, aes(x = factor(type), y = lois_height)) +
#'   geom_boxplot() +
#'   theme_SPSS(scale.x = "discrete")
#' }
#' @import ggplot2
#' @export
theme_SPSS <- function(base_size = 14,
                       base_family = "sans",
                       base_line_size = 0.75,
                       base_rect_size = 0.75,
                       version = "modern",
                       scales = "continuous",
                       scale.x = scales,
                       scale.y = scales) {

  # Validate arguments
  version <- match.arg(version, choices = c("modern", "legacy"))
  scales <- match.arg(scales, choices = c("continuous", "discrete"))
  scale.x <- match.arg(scale.x, choices = c("continuous", "discrete"))
  scale.y <- match.arg(scale.y, choices = c("continuous", "discrete"))

  # Useful measurements

  half_line <- base_size / 2
  quarter_line <- half_line / 2
  tiny <- 0.8
  small <- 0.9
  large <- 1.2

  # Version-specific elements
  if (version == "legacy") {
    ticks_length <- unit(half_line, "pt")
    axis_text_margin <- tiny * quarter_line
    panel_background <- element_rect(fill = "#F0F0F0", color = NA)
    panel_border <- element_rect(fill = NA, color = "black", linewidth = base_rect_size)
    grid_lines <- element_blank()
  } else {
    ticks_length <- unit(0, "pt")
    axis_text_margin <- quarter_line
    panel_background <- element_rect(fill = "white", color = NA)
    panel_border <- element_blank()
    grid_lines <- element_line(color = "#AEAEAE", linewidth = base_line_size)
  }

  # Define complete theme
  theme(
    # -----------------------------------------------
    # Basic elements
    # -----------------------------------------------
    line = element_line(color = "black", linewidth = base_line_size,
                        linetype = "solid", lineend = "butt"),
    rect = element_rect(fill = "white", color = "black",
                        linewidth = base_rect_size, linetype = "solid"),
    text = element_text(family = base_family, face = "plain", color = "black",
                        size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                        lineheight = 0.9, margin = margin(), debug = FALSE),
    title = element_text(face = "bold"),
    aspect.ratio = NULL,

    # -----------------------------------------------
    # Axis labels (titles)
    # -----------------------------------------------
    axis.title = NULL,
    axis.title.x = element_text(vjust = 1,
                                margin = margin(t = half_line)),
    axis.title.y = element_text(vjust = 1, angle = 90,
                                margin = margin(r = half_line)),
    axis.title.x.top = element_text(vjust = 0,
                                    margin = margin(b = half_line)),
    axis.title.x.bottom = NULL,
    axis.title.y.left = NULL,
    axis.title.y.right = element_text(vjust = 0, angle = -90,
                                      margin = margin(l = half_line)),

    # -----------------------------------------------
    # Axis tick labels (text)
    # -----------------------------------------------
    axis.text = element_text(color = "black",
                             size = if (scales != "discrete") rel(tiny)),
    axis.text.x = element_text(size = if (scale.x == "discrete") base_size,
                               vjust = 1,
                               margin = margin(t = axis_text_margin)),
    axis.text.y = element_text(size = if (scale.y == "discrete") base_size,
                               hjust = 1,
                               margin = margin(r = axis_text_margin)),
    axis.text.x.top = element_text(vjust = 0,
                                   margin = margin(b = axis_text_margin)),
    axis.text.x.bottom = NULL,
    axis.text.y.left = NULL,
    axis.text.y.right = element_text(hjust = 0,
                                     margin = margin(l = axis_text_margin)),

    # -----------------------------------------------
    # Axis tick marks
    # -----------------------------------------------
    axis.ticks = NULL,
    axis.ticks.length = ticks_length,

    # -----------------------------------------------
    # Axis lines
    # -----------------------------------------------
    axis.line = if (version == "legacy") element_blank(),
    axis.line.x = NULL,
    axis.line.x.top = if (version == "modern") element_blank(),
    axis.line.x.bottom = if (version == "modern") element_line(),
    axis.line.y = NULL,
    axis.line.y.left = if (version == "modern") element_line(),
    axis.line.y.right = if (version == "modern") element_blank(),

    # -----------------------------------------------
    # Legend
    # -----------------------------------------------
    legend.background = element_rect(color = NA),
    legend.spacing = unit(base_size, "pt"),
    legend.key = element_rect(color = NA),
    legend.key.size = NULL,
    legend.key.height = unit(base_size, "pt"),
    legend.key.width = unit(2 * base_size, "pt"),
    legend.text = element_text(size = rel(small)),
    legend.title = NULL,
    legend.position = "right",
    legend.justification = "top",
    legend.box.margin = margin(),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(base_size, "pt"),

    # -----------------------------------------------
    # Panel (plotting area)
    # -----------------------------------------------
    panel.background = panel_background,
    panel.border = panel_border,
    panel.spacing = unit(half_line, "pt"),

    # -----------------------------------------------
    # Grid lines
    # -----------------------------------------------
    panel.grid = NULL,
    panel.grid.major = NULL,
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = grid_lines,
    panel.ontop = FALSE,

    # -----------------------------------------------
    # Plot background
    # -----------------------------------------------
    plot.background = element_rect(fill = "white", color = "white"),

    # -----------------------------------------------
    # Plot titles and caption
    # -----------------------------------------------
    plot.title = element_text(size = rel(large), vjust = 1,
                              margin = margin(b = large * half_line)),
    plot.subtitle = element_text(size = rel(large), vjust = 1,
                                 margin = margin(b = large * half_line)),
    plot.title.position = "plot",
    plot.caption = element_text(face = "plain", size = rel(small), vjust = 1,
                                margin = margin(t = large * half_line)),
    plot.caption.position = "plot",

    # -----------------------------------------------
    # Plot margins (slightly larger for HTML)
    # -----------------------------------------------
    plot.margin = margin(half_line * 1.5, half_line * 1.5,
                         half_line * 1.5, half_line * 1.5),

    # -----------------------------------------------
    # Facet labels
    # -----------------------------------------------
    strip.background = element_rect(fill = "#E4E4E4"),
    strip.placement = "inside",
    strip.switch.pad.grid = unit(quarter_line, "pt"),
    strip.switch.pad.wrap = unit(quarter_line, "pt"),

    # Indicate that this is a complete theme
    complete = TRUE
  )
}


#' SPSS Color Palette
#'
#' Returns a vector of colors matching SPSS chart color defaults.
#' The modern palette matches SPSS v24+ and the legacy palette matches
#' older versions.
#'
#' @param n Integer or `NULL`. Number of colors to return. If `NULL`,
#'   returns all 30 colors.
#' @param version Character. `"modern"` (default) or `"legacy"`.
#'
#' @return A character vector of hex color codes.
#'
#' @examples
#' # Get first 5 modern colors
#' palette_SPSS(5)
#'
#' # Get first 3 legacy colors
#' palette_SPSS(3, version = "legacy")
#'
#' # Get all modern colors
#' palette_SPSS()
#'
#' @export
palette_SPSS <- function(n = NULL, version = "modern") {
  version <- match.arg(version, choices = c("modern", "legacy"))

  if (version == "legacy") {
    colors <- c(
      "#3E58AC", "#2EB848", "#D3CE97", "#79287D", "#FBF873",
      "#EF3338", "#48C2C5", "#CCCCCC", "#7AAAD5", "#0A562C",
      "#F8981D", "#DDBAF1", "#1A5F76", "#CCFFCC", "#BB3F7F",
      "#999999", "#000000", "#B6E7E8", "#FFFFFF", "#797AA7",
      "#70DC84", "#333333", "#ACD0EE", "#A21619", "#5D61FF",
      "#E4E4E4", "#278BAC", "#B89BC9", "#666666", "#0D8D46"
    )
  } else {
    colors <- c(
      "#1192E8", "#005D5D", "#9F1853", "#FA4D56", "#570408",
      "#198038", "#002D9C", "#EE538B", "#B28600", "#009D9A",
      "#012749", "#8A3800", "#A56EFF", "#ECE6D0", "#454647",
      "#5CCA88", "#D05334", "#CC7FE4", "#E1BC1D", "#ED4B4B",
      "#1CCDCD", "#5C7148", "#E18B0E", "#092672", "#5A645E",
      "#9B0000", "#CFACE3", "#969191", "#3FEB7C", "#6929C4"
    )
  }

  if (!is.null(n) && is.numeric(n)) {
    n <- max(1, min(n, length(colors)))
    colors <- colors[seq_len(n)]
  }

  colors
}


#' SPSS Palette Function (for ggplot2 Scales)
#'
#' Returns a palette function suitable for use with ggplot2 discrete scales.
#' This is the function factory used internally by [scale_color_SPSS()] and
#' [scale_fill_SPSS()].
#'
#' @param version Character. `"modern"` (default) or `"legacy"`.
#' @param direction Integer. `1` (default) for normal order, `-1` for reversed.
#'
#' @return A function that takes integer `n` and returns a character vector
#'   of `n` hex color codes.
#'
#' @examples
#' pal_fn <- SPSS_pal()
#' pal_fn(3)
#'
#' pal_fn_rev <- SPSS_pal(direction = -1)
#' pal_fn_rev(3)
#'
#' @export
SPSS_pal <- function(version = "modern", direction = 1) {
  function(n) {
    pal <- palette_SPSS(n, version)
    if (direction == -1) pal <- rev(pal)
    pal
  }
}


#' SPSS Color Scale for ggplot2
#'
#' Applies the SPSS color palette as a discrete color scale for points,
#' lines, and other geoms that use the `color` aesthetic.
#'
#' @param ... Additional arguments passed to [ggplot2::discrete_scale()].
#' @param version Character. `"modern"` (default) or `"legacy"`.
#' @param direction Integer. `1` for normal order, `-1` for reversed.
#'
#' @return A ggplot2 discrete scale object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(superman, aes(x = clark_height, y = lois_height, color = factor(type))) +
#'   geom_point(size = 3) +
#'   scale_color_SPSS() +
#'   theme_SPSS()
#' }
#'
#' @export
scale_color_SPSS <- function(..., version = "modern", direction = 1) {
  ggplot2::discrete_scale("color", "SPSS",
                          SPSS_pal(version, direction), ...)
}


#' SPSS Fill Scale for ggplot2
#'
#' Applies the SPSS color palette as a discrete fill scale for bar charts,
#' histograms, and other geoms that use the `fill` aesthetic.
#'
#' @param ... Additional arguments passed to [ggplot2::discrete_scale()].
#' @param version Character. `"modern"` (default) or `"legacy"`.
#' @param direction Integer. `1` for normal order, `-1` for reversed.
#'
#' @return A ggplot2 discrete scale object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(superman, aes(x = factor(type), fill = factor(type))) +
#'   geom_bar() +
#'   scale_fill_SPSS() +
#'   theme_SPSS(scale.x = "discrete")
#' }
#'
#' @export
scale_fill_SPSS <- function(..., version = "modern", direction = 1) {
  ggplot2::discrete_scale("fill", "SPSS",
                          SPSS_pal(version, direction), ...)
}


#' Format Numbers SPSS-Style
#'
#' Formats numbers without thousands separators, matching SPSS axis label
#' defaults. Useful for axis labels in combination with [theme_SPSS()].
#'
#' @param x Numeric vector to format.
#' @param big.mark Character. Thousands separator. Default `""` (no separator).
#' @param ... Additional arguments passed to [scales::number()].
#'
#' @return A character vector of formatted numbers.
#'
#' @examples
#' number_SPSS(c(1000, 25000, 100))
#' # Returns: "1000" "25000" "100"
#'
#' \dontrun{
#' # Use with ggplot2 axis labels
#' library(ggplot2)
#' ggplot(superman, aes(x = clark_height, y = lois_height)) +
#'   geom_point() +
#'   scale_x_continuous(labels = number_SPSS) +
#'   theme_SPSS()
#' }
#'
#' @export
number_SPSS <- function(x, big.mark = "", ...) {
  scales::number(x, big.mark = big.mark, ...)
}
