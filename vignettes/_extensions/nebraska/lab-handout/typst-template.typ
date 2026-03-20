// ============================================================================
// Color scheme
// ============================================================================
#let COLOR-BACKGROUND = rgb("#fafafa")
#let COLOR-FOREGROUND = rgb("#333333")
#let colors = (foreground: COLOR-FOREGROUND, background: COLOR-BACKGROUND)

// ============================================================================
// Margin state
// ============================================================================
#let page-margin = (top: 4cm, left: 2.5cm, right: 2.5cm, bottom: 2.5cm)

// ============================================================================
// Header constants
// ============================================================================
#let HEADER-TITLE-SIZE = 2em
#let HEADER-SUBTITLE-SIZE = 1.25em
#let HEADER-TITLE-WEIGHT = 600
#let HEADER-SUBTITLE-OPACITY = 90%
#let HEADER-LOGO-HEIGHT = 1.5cm
#let HEADER-PADDING-VERTICAL = 0.5cm
#let HEADER-PADDING-HORIZONTAL = 0cm

// ============================================================================
// Footer constants
// ============================================================================
#let FOOTER-TEXT-SIZE = 8pt
#let FOOTER-LAB-PAGE-WEIGHT = "bold"
#let FOOTER-LAB-PADDING-VERTICAL = 0.5cm
#let FOOTER-LAB-PADDING-HORIZONTAL = 0cm

// ============================================================================
// Path helpers
// ============================================================================
/// Check if a value is a valid file path (not none, not empty, looks like a path)
#let is-valid-path(val) = {
  val != none and val != "" and (val.contains("/") or val.contains("."))
}

/// Clean backslash escapes from Quarto/Pandoc path strings
#let clean-path(val) = {
  if val != none { val.replace("\\", "") } else { none }
}

// ============================================================================
// Header function
// ============================================================================
#let emmarshall-header-lab(
  title: none,
  subtitle: none,
  logo: none,
  logo-alt: none,
  colors: none,
  show-logo: true,
) = {
  let left-margin = page-margin.left
  let right-margin = page-margin.right
  let total-horizontal = left-margin + right-margin
  let symmetric-margin = calc.min(left-margin, right-margin)

  // Clean the logo path (removes Pandoc backslash escaping)
  let header-logo = if is-valid-path(logo) {
    clean-path(logo)
  } else {
    none
  }

  place(
    top + left,
    dx: -left-margin,
    dy: 0cm,
    block(
      width: 100% + total-horizontal,
      fill: colors.foreground,
      inset: (
        left: symmetric-margin + HEADER-PADDING-HORIZONTAL,
        right: symmetric-margin + HEADER-PADDING-HORIZONTAL,
        top: HEADER-PADDING-VERTICAL,
        bottom: HEADER-PADDING-VERTICAL,
      ),
      {
        grid(
          columns: (1fr, auto),
          align: (left + horizon, right + horizon),
          column-gutter: 3em,
          {
            stack(
              dir: ttb,
              spacing: 0.5em,
              {
                if title != none {
                  text(
                    size: HEADER-TITLE-SIZE,
                    weight: HEADER-TITLE-WEIGHT,
                    fill: colors.background,
                    title,
                  )
                }
              },
              {
                if subtitle != none {
                  text(
                    size: HEADER-SUBTITLE-SIZE,
                    fill: colors.background.transparentize(
                      100% - HEADER-SUBTITLE-OPACITY,
                    ),
                    subtitle,
                  )
                }
              },
            )
          },
          {
            if show-logo and header-logo != none {
              image(
                header-logo,
                fit: "contain",
                height: HEADER-LOGO-HEIGHT,
                alt: if logo-alt != none { logo-alt } else { "" },
              )
            }
          },
        )
      },
    ),
  )
  v(HEADER-PADDING-VERTICAL * 2 + HEADER-LOGO-HEIGHT)
}

// ============================================================================
// Footer function
// ============================================================================
#let emmarshall-footer-lab(
  lab: none,
  dataset: none,
  colors: none,
) = {
  let left-margin = page-margin.left
  let right-margin = page-margin.right
  let total-horizontal = left-margin + right-margin
  let symmetric-margin = calc.min(left-margin, right-margin)

  place(
    bottom + left,
    dx: -left-margin,
    dy: 0cm,
    block(
      width: 100% + total-horizontal,
      fill: colors.foreground,
      inset: (
        left: symmetric-margin + FOOTER-LAB-PADDING-HORIZONTAL,
        right: symmetric-margin + FOOTER-LAB-PADDING-HORIZONTAL,
        top: FOOTER-LAB-PADDING-VERTICAL,
        bottom: FOOTER-LAB-PADDING-VERTICAL,
      ),
      grid(
        columns: (1fr, auto, 1fr),
        align: (left + horizon, center + horizon, right + horizon),
        gutter: 0em,
        {
          if lab != none {
            text(
              size: FOOTER-TEXT-SIZE,
              fill: colors.background,
            )[#lab]
          }
        },
        text(
          size: FOOTER-TEXT-SIZE,
          weight: FOOTER-LAB-PAGE-WEIGHT,
          fill: colors.background,
        )[
          #context counter(page).display("1 / 1", both: true)
        ],
        {
          if dataset != none {
            text(
              size: FOOTER-TEXT-SIZE,
              fill: colors.background,
            )[#dataset]
          }
        },
      ),
    ),
  )
}

// ============================================================================
// Title page function
// ============================================================================
#let emmarshall-title-page(
  title: none,
  subtitle: none,
  dataset: none,
  course: none,
  colors: none,
) = {
  show heading.where(level: 1): set text(size: 28pt, weight: "bold")

  // Title block
  align(center)[
    #v(1.5em)
    #block(
      radius: 12pt,
      inset: 2em,
      stroke: 2pt + colors.foreground,
    )[
      #heading(level: 1, outlined: false, bookmarked: true)[#title]
      #if subtitle != none [
        #v(0.5em)
        #text(size: 20pt, weight: "semibold", fill: colors.foreground.transparentize(20%))[#subtitle]
      ]
    ]
    #v(2em)
  ]

  // Assignment Overview box with two columns
  block(
    radius: 8pt,
    fill: colors.foreground.transparentize(95%),
    stroke: 1pt + colors.foreground.transparentize(60%),
    inset: 1.5em,
    width: 100%,
  )[
    #align(center)[
      #heading(level: 2, outlined: false, bookmarked: true)[
        #text(size: 16pt, weight: "bold", fill: colors.foreground)[
          Assignment Overview
        ]
      ]
    ]
    #v(0.75em)
    #grid(
      columns: (1fr, 1fr),
      column-gutter: 2em,
      // Left column: Document Contents (TOC)
      [
        #text(weight: "bold", size: 11pt)[Document Contents:]
        #v(0.5em)
        #outline(
          title: none,
          indent: auto,
        )
      ],
      // Right column: Assignment Details
      [
        #text(weight: "bold", size: 11pt)[Assignment Details:]
        #v(0.5em)
        #table(
          columns: (auto, 1fr),
          stroke: none,
          inset: (x: 0pt, y: 3pt),
          ..if course != none {(
            [• *Course:*], [#course],
          )} else {()},
          ..if dataset != none {(
            [• *Dataset:*], [#dataset],
          )} else {()},
        )
      ],
    )
  ]

  pagebreak()
}

// ============================================================================
// Main document function
// ============================================================================
#let project(
  title: none,
  subtitle: none,
  logo: none,
  logo-alt: none,
  lab: none,
  dataset: none,
  course: none,
  instructor: none,
  due-date: none,
  body,
) = {
  set document(title: title)
  set text(font: "Atkinson Hyperlegible")

  // Title page - no header or footer
  set page(
    fill: COLOR-BACKGROUND,
    margin: page-margin,
    header: none,
    footer: none,
  )

  emmarshall-title-page(
    title: title,
    subtitle: subtitle,
    lab: lab,
    dataset: dataset,
    course: course,
    instructor: instructor,
    due-date: due-date,
    colors: colors,
  )

  // Content pages - with header and footer
  set page(
    header: emmarshall-header-lab(
      title: title,
      subtitle: subtitle,
      logo: logo,
      logo-alt: logo-alt,
      colors: colors,
    ),
    header-ascent: 0%,
    footer: emmarshall-footer-lab(
      lab: lab,
      dataset: dataset,
      colors: colors,
    ),
    footer-descent: 0%,
  )

  body
}
