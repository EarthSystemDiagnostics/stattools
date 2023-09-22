#' Effective degrees of freedom
#'
#' Calculate the effective degrees of freedom of an autocorrelated dataset
#' consisting of \code{n} observations, i.e. the nuber of independent
#' obervations given an autocorrelation parameter for the data estimated at lag
#' 1.
#'
#' @param n number of observations.
#' @param a1 lag 1 autocorrelation parameter.
#' @return the effective degrees of freedom.
#' @author Thomas Laepple, Thomas Münch
#' @examples
#' getEffectiveDOF(10, 0.6)
#' @source
#' Xu, X.: Methods in Hypothesis Testing, Markov Chain Monte Carlo and
#'   Neuroimaging Data Analysis, PhD thesis, Harvard University, Cambridge,
#'   Massachusetts, 2013, \url{https://dash.harvard.edu/handle/1/11108711}.
#' @export
#'
getEffectiveDOF <- function(n, a1) {

  a1 <- abs(a1)
  denom <- (1 + a1) / (1 - a1) - (2 * a1 / n) * (1 - a1^n) / (1 - a1)^2

  n / denom

}
