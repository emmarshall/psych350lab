#' Superman Media Dataset
#'
#' A dataset containing information about Superman media appearances across
#' film, television, and animation, including actor heights, critical
#' reception, and audience engagement metrics.
#'
#' @format A tibble with 11 rows and 21 variables:
#' \describe{
#'   \item{num}{Numeric identifier for each media entry}
#'   \item{media}{Media identifier}
#'   \item{year}{Year of release}
#'   \item{type}{Media type (1 = Film, 2 = TV, 3 = Serial)}
#'   \item{clark_height}{Actor height in meters}
#'   \item{lois_height}{Actress height in meters}
#'   \item{rt_critics_score}{Rotten Tomatoes critics score (0-100)}
#'   \item{rt_critic_count}{Number of RT critic reviews}
#'   \item{rt_audience_score}{Rotten Tomatoes audience score (0-100)}
#'   \item{rt_audience_count}{Number of RT audience ratings}
#'   \item{lbd_likes}{Letterboxd likes count}
#'   \item{lbd_scores}{Letterboxd average score (0-5)}
#'   \item{clark_height_in}{Actor height in inches}
#'   \item{lois_height_in}{Actress height in inches}
#'   \item{clark_grp}{Height group (1 = under 6ft, 2 = 6ft+)}
#'   \item{height_diff}{Height difference in inches (Clark - Lois)}
#'   \item{height_gap}{Height gap category (1 = <6in, 2 = 6-8in, 3 = >8in)}
#'   \item{tomatometer}{Tomatometer (1 = Rotten, 2 = Fresh)}
#'   \item{rt_avg}{Average of critics and audience scores}
#'   \item{rt_diff}{Weighted difference between critics and audience}
#'   \item{popular}{Popularity category (1 = low, 2 = mid, 3 = high)}
#' }
#'
#' @examples
#' data(superman)
#' head(superman)
#' mean(superman$clark_height_in, na.rm = TRUE)
#'
#' @source Rotten Tomatoes, Letterboxd, and public biographical data.
#' @name superman
#' @docType data
#' @keywords datasets
NULL


#' Superman SMES Dataset (Simulated)
#'
#' A simulated dataset based on the Superman media data, designed for
#' use in between-groups ANOVA exercises with three height gap groups.
#'
#' @format A tibble with 47 rows and 5 variables:
#' \describe{
#'   \item{num}{Participant number}
#'   \item{height_gap}{Height gap category (1, 2, or 3)}
#'   \item{emotional_impact}{Emotional impact score (4-20)}
#'   \item{aesthetic_appeal}{Aesthetic appeal score (3-15)}
#'   \item{cognitive_engagement}{Cognitive engagement score (0-7)}
#' }
#'
#' @examples
#' data(superman_smes)
#' head(superman_smes)
#'
#' @source Simulated from psych350lab::superman height_gap distribution.
"superman_smes"

