#' Summarise an Empirical Probability Distribution Function.
#'
#' @param x A vector of values of empirical PDF
#' @param p A vector of probabilities
#' @details Calculation of the mode is naive. For a multimodal distribution only
#' the highest is returned, in the case of 2 or more modes with exactly the same
#'  probability, the first is returned.
#' @return Returns a named vector with the mean, median, mode, and standard
#' deviation of the empirical PDF
#' @export
#' @examples
#' df <- data.frame(x = 1:10)
#' df$p <- dnorm(df$x, 5, 2)
#' SummariseEmpiricalPDF(df$x, df$p)
SummariseEmpiricalPDF <- function(x, p){

  # Ensure x and p are sorted
  p <- p[order(x)]
  x <- sort(x)

  # Ensure p sum to 1
  p <- p / sum(p)

  # Mean
  w.mean <- sum(x * p)

  # SD
  M <- sum(p > 0)
  w.sd <- sqrt(sum(p * (x-w.mean)^2) / ((M-1)/M * sum(p)))

  # Median
  csum.p <- cumsum(p)
  med.ind <- which.min(abs(csum.p - 0.5))
  w.median <- x[med.ind]

  # Mode
  max.wt <- max(p)
  n.max <- sum(p == max.wt)
  if (n.max > 1) warning(paste0(n.max,
                                " x with equal maximum probability. Returning the first"))
  mode <- x[which.max(p)]

  return(c("mean" = w.mean, "median" = w.median, "mode" = mode, "sd" = w.sd))
}

#' Expected range
#'
#' Calculates the expected range of n observations drawn from a distibution with standard deviation sd
#' @param sd standard deviation
#' @param n number of observations
#'
#' @return numeric
#' @export
#'
#' @examples
#'
#' ExpectedRange(5, 15)
#'
#' \dontrun{
#' library(tidyverse)
#' f <- function(x, n) n * x * pnorm(x)^(n - 1) * dnorm(x)
#' ests <- expand.grid(mean = 0, sd = 2, n = 15, rep = 1:1000, i = 1:15) %>%
#'   mutate(rdv = rnorm(n(), mean, sd)) %>%
#'   group_by(mean, sd, n, rep) %>%
#'   summarise(sd_hat = sd(rdv),
#'             obs.range = diff(range(rdv))) %>%
#'   group_by(mean, sd, n, rep) %>%
#'   mutate(sd.range = 2*sd_hat*integrate(f,-Inf,Inf, n = n)$value,
#'          exp.range = ExpectedRange(sd, n))
#' ests %>%
#'   gather(estimate, value, -mean, -sd, -n, -rep, -sd_hat, -exp.range) %>%
#'   ggplot(aes(x = estimate, y = value)) +
#'   geom_boxplot() +
#'   geom_hline(aes(yintercept = exp.range)) +
#'   facet_wrap(~n)
#'   }
ExpectedRange <- function(sd, n) {

  f <- function(x, n) n * x * pnorm(x)^(n - 1) * dnorm(x)

  exp.range <- 2 * sd * integrate(f, -Inf, Inf, n = n)$value

  return(exp.range)
}
