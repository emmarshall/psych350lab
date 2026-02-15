## code to prepare `superman_data` dataset goes here

library(dplyr)

# Create the initial Superman dataset
superman_raw <- tibble::tibble(
  num = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
  year = c(2025, 1978, 2001, 2006, 1951, 2013, 1948, 2021, 1993, 1988, 1989),
  type = c(1, 1, 2, 1, 3, 1, 3, 2, 2, 2, 2),
  clark_height = c(1.93, 1.93, 1.90, 1.89, 1.86, 1.85, 1.85, 1.82, 1.81, 1.83, 1.83),
  lois_height = c(1.60, 1.72, 1.71, 1.65, 1.63, 1.63, 1.62, 1.68, 1.68, -99.00, -99.00),
  rt_critics_score = c(83, 88, 78, 72, -99, 57, 83, 88, 86, -99, -99),
  rt_critic_count = c(484, 121, 111, 290, 7, 340, 484, 55, 20, 0, 0),
  rt_audience_score = c(90, 86, 72, 60, 79, 75, 90, 84, 86, -99, -99),
  rt_audience_count = c(25000, 250000, 2500, 250000, 250, 250000, 25000, 1000, 100, -99, -99),
  lbd_likes = c(1105511, 99115, -99, 26076, 744, 204463, -99, -99, -99, -99, -99),
  lbd_scores = c(3.9, 3.7, -99, 2.7, 2.6, 3.0, -99, -99, -99, -99, -99)
)

# Replace -99 and 0 with NA
superman_data <- superman_raw |>
  mutate(
    clark_height = ifelse(clark_height == -99, NA, clark_height),
    lois_height = ifelse(lois_height == -99, NA, lois_height),
    rt_critics_score = ifelse(rt_critics_score == -99, NA, rt_critics_score),
    rt_critic_count = ifelse(rt_critic_count == -99 | rt_critic_count == 0, NA, rt_critic_count),
    rt_audience_score = ifelse(rt_audience_score == -99, NA, rt_audience_score),
    rt_audience_count = ifelse(rt_audience_count == -99, NA, rt_audience_count),
    lbd_likes = ifelse(lbd_likes == -99, NA, lbd_likes),
    lbd_scores = ifelse(lbd_scores == -99, NA, lbd_scores)
  )

# Convert heights from meters to inches (1 meter = 39.37 inches)
superman_data <- superman_data |>
  mutate(
    clark_height_in = clark_height * 39.37,
    lois_height_in = lois_height * 39.37
  )

# Create clark_grp (1 = under 6ft/72 inches, 2 = over 6ft/72 inches)
superman_data <- superman_data |>
  mutate(
    clark_grp = case_when(
      is.na(clark_height_in) ~ NA_real_,
      clark_height_in < 72 ~ 1,
      clark_height_in >= 72 ~ 2
    )
  )

# Calculate height difference
superman_data <- superman_data |>
  mutate(
    height_diff = clark_height_in - lois_height_in
  )

# Create height_gap categories
superman_data <- superman_data |>
  mutate(
    height_gap = case_when(
      is.na(height_diff) ~ NA_real_,
      height_diff < 6 ~ 1,
      height_diff >= 6 & height_diff <= 8 ~ 2,
      height_diff > 8 ~ 3
    )
  )

# Create tomatometer (1 = rotten < 60, 2 = fresh >= 60)
superman_data <- superman_data |>
  mutate(
    tomatometer = case_when(
      is.na(rt_critics_score) ~ NA_real_,
      rt_critics_score < 60 ~ 1,
      rt_critics_score >= 60 ~ 2
    )
  )

# Calculate rt_avg
superman_data <- superman_data |>
  mutate(
    rt_avg = (rt_critics_score + rt_audience_score) / 2
  )

# Calculate rt_diff (weighted difference)
superman_data <- superman_data |>
  mutate(
    rt_diff = (rt_critics_score * rt_critic_count - rt_audience_score * rt_audience_count) /
      (rt_critic_count + rt_audience_count)
  )

# Create popular variable
superman_data <- superman_data |>
  mutate(
    popular = case_when(
      is.na(lbd_likes) ~ NA_real_,
      lbd_likes < 1000 ~ 1,
      lbd_likes >= 1000 & lbd_likes <= 100000 ~ 2,
      lbd_likes > 100000 ~ 3
    )
  )

# Save to package
usethis::use_data(superman_data, overwrite = TRUE)

usethis::use_data(superman_data, overwrite = TRUE)
