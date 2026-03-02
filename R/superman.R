#' Superman Actor Data
#'
#' Physical characteristics and ratings data for actors who have played
#' Superman across various films and TV shows.
#'
#' @format A tibble with 11 rows and variables:
#' \describe{
#'   \item{num}{Participant id number}
#'   \item{type}{Media type (Film, TV Show, Serial)}
#'   \item{title}{Title of the production}
#'   \item{year}{Release year}
#'   \item{clark_actor}{Actor playing Clark Kent/Superman}
#'   \item{clark_height}{Clark Kent/Superman actor's height in meters}
#'   \item{clark_age}{Clark Kent/Superman actor's age at debut
#'   #'(years)}
#'   \item{lois_actor}{Actor playing Lois Lane}
#'   \item{lois_height}{Lois Lane actor's height in meters}
#'   \item{lois_age}{Lois Lane actor's age at debut (years)}
#'   \item{clark_height_in}{Clark Kent/Superman actor's height in inches}
#'   \item{lois_height_in}{Lois Lane actor's height in inches}
#'   \item{clark_grp}{Clark Height group: 1 = under 72 inches, 2 = 72+ inches}
#'   \item{height_diff}{Height difference between Lois and Clark in inches (Clark - Lois)}
#'   \item{age_diff}{Age difference between Lois and Clark in years}

#'   \item{age_gap}{Height gap category: 1 = <6in, 2 = 6-8in, 3 = >8in}
#'   \item{rt_critics_score}{Rotten Tomatoes critics score}
#'   \item{rt_audience_score}{Rotten Tomatoes audience score}
#'   \item{tomatometer}{Critics rating: 1 = Rotten (<60), 2 = Fresh (60+)}
#'   \item{rt_avg}{Average of critics and audience scores}
#'   \item{ldb_likes}{Letterboxd likes}
#'   \item{ldb_scores}{Letterboxd score}
#'   \item{popular}{Popularity category based on Letterboxd likes}
#' }
#'
#' @source Compiled from the internet including Rotten Tomatoes, Letterboxd, and IMDb.
"superman"

#' Superman SMES Data
#'
#' Simulated data for 47 participants rating Superman media on the
#' Subjective Media Experience Scale (SMES), grouped by height gap
#' between the Superman and Lois Lane actors.
#'
#' @format A data frame with 47 rows and 5 variables:
#' \describe{
#'   \item{num}{Unique participant number}
#'   \item{height_gap}{Height gap category: Minimal (1), Average (2), Big (3)}
#'   \item{emotional_impact}{Emotional Impact subscale (sum of 4 items, range 4-20)}
#'   \item{aesthetic_appeal}{Aesthetic Appeal subscale (sum of 3 items, range 3-15)}
#'   \item{cognitive_engagement}{Cognitive Engagement subscale (mean of 4 items, range 0-7)}
#' }
"superman_smes"
