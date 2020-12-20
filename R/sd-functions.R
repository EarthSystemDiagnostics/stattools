#' Population standard deviation
#'
#' Calculate the population standard deviation.
#'
#' @param x numeric; vector of values.
#' @return The population standard deviation value of \code{x}.
#' @author Andrew Dolman <andrew.dolman@awi.de>
#' @examples
#'
#' # analytical solution to SD of uniform distribution:
#' sqrt(1/12 * 10^2)
#'
#' sd(1:10)
#'
#' PopSD(1:10)
#' @export
PopSD <- function(x) {

  n <- length(x)
  adj <- sqrt((n - 1) / n)
  adj * stats::sd(x)
}

#' Standard deviation (error) of standard deviation
#'
#' Calculate the standard deviation (i.e. standard error) of a standard
#' deviation, given n, the number of samples from which the standard deviation
#' was estimated.
#'
#' If n <= 300, an exact method is used, otherwise Stirling's approximation is
#' used to avoid numerical errors calculating the gamma function for large
#' values of n. Formulae taken from https://stats.stackexchange.com/q/28567.
#'
#' @param s numeric; estimated standard deviation.
#' @param n integer; number of samples from which \code{s} was estimated.
#' @return Numeric value giving the standard error of the estimated standard
#'   deviation \code{s}.
#' @author Andrew Dolman <andrew.dolman@awi.de>
#' @examples
#' \dontrun{
#' if(interactive()){
#'  SDofSD(1, 12)
#'  library(ggplot2)
#' df <- expand.grid(SD = 1:10, n = seq(2, 10, length.out = 100))
#' df$SDofSD = SDofSD(df$SD, df$n)
#' ggplot(df, aes(x = n, y = SDofSD, group = SD, colour = SD)) +
#'   geom_line()
#'  }
#' }
#' @export
SDofSD <- function(s, n) {

  if (any(n < 2, na.rm = TRUE))
    warning("n must be greater than or equal to 2")
  if (any(s < 0, na.rm = TRUE))
    warning("s must be >= 0")
  if(length(s) > 1 & length(n) == 1)
    n <- rep(n, length(s))
  if(length(n) > 1 & length(s) == 1)
    s <- rep(s, length(n))
  if(length(s) != length(n))
    stop("'s' and 'n' should be vectors of either the same length, ",
         "or one of them should be length == 1.")

  ifelse(n <= 300, {
    message("Method: Exact")
    g.a <- gamma((n - 1) / 2)
    g.b <- gamma(n / 2)
    s * (g.a / g.b) * sqrt((n - 1) / 2 - (g.b / g.a) ^ 2)
  }, {
    message("Method: Stirling's approximation")
    s * sqrt(exp(1) * (1 - 1 / n) ^ (n - 1) - 1)
  })
}

#' Standard deviation of numerical probability density function
#'
#' Calculates the standard deviation for a given numerical probability density
#' function, i.e. a vector of values and corresponding vector of densities. It
#' is important that the range of x values covers a sufficient fraction of the
#' total density. For continuous distributions it should extend well into the
#' tails. The approximation will be better the finer the resolution of x.
#'
#' @param x numeric; vector of values.
#' @param d numeric; vector of densities, do not need to sum to 1.
#' @return Numeric value giving the standard deviation of the given probability
#'   density function.
#' @author Andrew Dolman <andrew.dolman@awi.de>
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # Example 1
#'  x <- seq(1, 100, by = 0.01)
#'  d <- dnorm(x, 60, 5)
#'  plot(x, d, type = "l")
#'  SDofNumDist(x, d)
#'
#'  # Example 2
#'  a <- 45
#'  b <- 55
#'  d2 <- dunif(x, a, b)
#'  plot(x, d2, type = "l")
#'  SDofNumDist(x, d2)
#'
#'  # analytical solution for uniform distribution
#'  sqrt(1/12 * (b-a)^2)
#'  }
#' }
#' @export
SDofNumDist <- function(x, d) {

  d <- d / sum(d)
  sqrt(sum(d * x^2) - sum(d * x)^2)
}
