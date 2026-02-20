# ============================================================================
# Superman Actor Data
# ============================================================================

library(dplyr)
library(usethis)

superman <- tibble::tibble(
  num = 1:11,
  media = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10),
  year = c(2025, 1978, 2001, 2006, 1951, 2013, 1948, 2021, 1993, 1988, 1989),
  type = c(1, 1, 2, 1, 3, 1, 3, 2, 2, 2, 2),
  clark_height = c(1.93, 1.93, 1.90, 1.89, 1.86, 1.85, 1.85, 1.82, 1.81, 1.83, 1.83),
  lois_height = c(1.60, 1.72, 1.71, 1.65, 1.63, 1.63, 1.62, 1.68, 1.68, NA, NA),
  rt_critics_score = c(83, 88, 78, 72, NA, 57, 83, 88, 86, NA, NA),
  rt_critic_count = c(484, 121, 111, 290, NA, 340, 484, 55, 20, NA, NA),
  rt_audience_score = c(90, 86, 72, 60, 79, 75, 90, 84, 86, NA, NA),
  rt_audience_count = c(25000, 250000, 2500, 250000, 250, 250000, 25000, 1000, 100, NA, NA),
  ldb_likes = c(1105511, 99115, NA, 26076, 744, 204463, NA, NA, NA, NA, NA),
  ldb_scores = c(3.9, 3.7, NA, 2.7, 2.6, 3.0, NA, NA, NA, NA, NA)
) |>
  mutate(
    clark_height_in = clark_height * 39.37,
    lois_height_in = lois_height * 39.37,
    clark_grp = case_when(
      is.na(clark_height_in) ~ NA_real_,
      clark_height_in < 72 ~ 1,
      clark_height_in >= 72 ~ 2
    ),
    height_diff = clark_height_in - lois_height_in,
    height_gap = case_when(
      is.na(height_diff) ~ NA_real_,
      height_diff < 6 ~ 1,
      height_diff >= 6 & height_diff <= 8 ~ 2,
      height_diff > 8 ~ 3
    ),
    tomatometer = case_when(
      is.na(rt_critics_score) ~ NA_real_,
      rt_critics_score < 60 ~ 1,
      rt_critics_score >= 60 ~ 2
    ),
    rt_avg = (rt_critics_score + rt_audience_score) / 2,
    rt_diff = (rt_critics_score * rt_critic_count - rt_audience_score * rt_audience_count) /
      (rt_critic_count + rt_audience_count),
    popular = case_when(
      is.na(ldb_likes) ~ NA_real_,
      ldb_likes < 1000 ~ 1,
      ldb_likes >= 1000 & ldb_likes <= 100000 ~ 2,
      ldb_likes > 100000 ~ 3
    )
  )

usethis::use_data(superman, overwrite = TRUE)
