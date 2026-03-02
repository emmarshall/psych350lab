# ============================================================================
# Superman Actor Dataset (dplyr 1.2.0+)
# ============================================================================
library(dplyr)
library(stringr)
library(readxl)
library(lubridate)
library(usethis)

superman <- read_excel("data-raw/superman/superman_raw.xlsx") |>
  mutate(
    # Participant number
    num = row_number(),

    # Standardize type values
    type = str_trim(type),

    # Parse dates
    release_date = ymd(release_date),
    clark_birth = ymd(clark_birth),
    lois_birth = ymd(lois_birth),

    # Calculate ages at release
    clark_age = round(time_length(interval(clark_birth, release_date), "years"), 2),
    lois_age = round(time_length(interval(lois_birth, release_date), "years"), 2),

    # Age difference
    age_diff = abs(clark_age - lois_age),

    # Age groups - use cut() for threshold-based binning
    age_grp = cut(
      age_diff,
      breaks = c(-Inf, 2, 5, Inf),
      labels = c("Minimal", "Average", "Big"),
      right = FALSE
    ) |> as.character(),

    # Height conversions
    clark_height_in = clark_height * 39.37,
    lois_height_in = lois_height * 39.37,

    # Height difference
    height_diff = clark_height_in - lois_height_in,

    # Height gap category - cut() for thresholds
    height_gap = cut(
      height_diff,
      breaks = c(-Inf, 6, 8, Inf),
      labels = c("Minimal", "Average", "Big"),
      right = FALSE
    ) |> as.character(),

    # Clark height group - if_else() for binary
    clark_grp = if_else(
      clark_height_in < 72,
      "Under 6ft",
      "6ft or taller",
      missing = NA_character_
    ),

    # Tomatometer - if_else() for binary
    tomatometer = if_else(
      rt_critics_score < 60,
      "Rotten",
      "Fresh",
      missing = NA_character_
    ),

    # RT average
    rt_avg = (rt_critics_score + rt_audience_score) / 2,

    # Popularity category - cut() for thresholds
    popular = cut(
      ldb_likes,
      breaks = c(-Inf, 1000, 100000, Inf),
      labels = c("Low", "Mid", "High"),
      right = FALSE
    ) |> as.character()
  ) |>
  select(-release_date, -clark_birth, -lois_birth)

usethis::use_data(superman, overwrite = TRUE)
cat("Created: superman\n")
