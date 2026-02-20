# ============================================================================
# Superman SMES Data (Simulated)
# ============================================================================

library(dplyr)
library(usethis)
library(here)

load(here::here("data", "superman.rda"))

set.seed(123)
base_gaps <- superman |> filter(!is.na(height_gap)) |> pull(height_gap)
target_n <- 47
height_gap_sample <- sample(base_gaps, target_n, replace = TRUE)

superman_smes <- tibble::tibble(
  num = 1:target_n,
  height_gap = height_gap_sample,
  emotional_impact = sapply(height_gap_sample, function(gap) {
    mu <- c(11, 12, 14)[gap]
    pmin(pmax(round(rnorm(1, mu, 3)), 4), 20)
  }),
  aesthetic_appeal = sapply(height_gap_sample, function(gap) {
    mu <- c(9, 9.5, 10)[gap]
    pmin(pmax(round(rnorm(1, mu, 2.5)), 3), 15)
  }),
  cognitive_engagement = sapply(height_gap_sample, function(gap) {
    mu <- c(3.8, 4.0, 4.5)[gap]
    pmin(pmax(round(rnorm(1, mu, 1.2), 1), 0), 7)
  })
)

usethis::use_data(superman_smes, overwrite = TRUE)
