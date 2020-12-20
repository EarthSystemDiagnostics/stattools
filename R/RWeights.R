#' Draw random number from distribution given the weights
#'
#' Draw a given number of random integer values with replacement from a
#' distribution given by its weights, where the values can range between 1 and
#' the length of the weights vector.
#'
#' @param n integer; number of random integers to be drawn.
#' @param weights numeric vector of weights which define the distribution.
#' @return Vector of \code{n} random numbers between 1 and the length of
#'   \code{weights}.
#' @author Thomas Laepple
#' @examples
#' weights <- c(rep(1,10), 1:10, 10:1)
#' hist(RWeights(100000, weights), breaks = 501)
#' @export
RWeights <- function(n = 1000, weights) {

  base::sample.int(length(weights), n, prob = weights, replace = TRUE)
}
