# Code adapted from base stats:::t.test.default as of R version 4.3.1:
# GPL-2 | GPL-3
# Copyright (C) 2023 R Core Team

# Changes to original code by Thomas Muench in 2023/11/27:
# - introduced function parameters `a1.x` and `a1.y` to allow input of AR1
#   autocorrelation parameters for data x and/or y;
# - use these parameters to reset the number of x/y data points to the effective
#   number of degrees of freedom using stattools::getEffectiveDOF

#' Student's t-Test for autocorrelated data
#'
#' Perform a one-sample or a two-sample t-test accounting for autocorrelation of
#' the input data. This function is a modified version of the base R's default
#' \code{\link[stats]{t.test}}, which treats autocorrelation of the input data
#' by resetting the number of observations to the effective degrees of freedom
#' according to user-supplied estimates of the data's autocorrelation parameters
#' at lag one. Note that this code is adapted from the default method of the
#' t.test function, thus it cannot be used with class \code{formula}.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param y an optional (non-empty) numeric vector of data values.
#' @param a1.x autocorrelation parameter at lag one of the data in \code{x}.
#' @param a1.y autocorrelation parameter at lag one of the data in \code{y}.
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of \code{"two.sided"} (default), \code{"greater"} or
#'   \code{"less"}.  You can specify just the initial letter.
#' @param mu a number indicating the true value of the mean (or difference in
#'   means if you are performing a two sample test).
#' @param paired a logical indicating whether you want a paired t-test.
#' @param var.equal a logical variable indicating whether to treat the two
#'   variances as being equal. If ‘TRUE’ then the pooled variance is used to
#'   estimate the variance otherwise the Welch (or Satterthwaite) approximation
#'   to the degrees of freedom is used.
#' @param conf.level confidence level of the interval.
#' @param ... further arguments to be passed to or from methods.
#' @return see \code{?t.test}.
#' @seealso \code{\link[stats]{t.test}}, \code{\link{getEffectiveDOF}}
#' @source \code{stats:::t.test.default}
#' @author R Core Team, Thomas Münch
#' @examples
#'
#' # Check the performance of the test on autocorrelated data
#'
#' # This function Monte Carlo samples autocorrelated data series from an AR1
#' # process and runs the default t test as well as the stattools t_test to
#' # estimate the tests' rejection rates, i.e. the relative number of false
#' # positives where the tests reject the null hypothesis
#' estimateRejectionRate <- function(nobs, nr, a1.x, a1.y = a1.x,
#'                                   method = c("one.sample", "two.sample")) {
#'
#'  method <- match.arg(method)
#'
#'  # wrapper for random generation of AR1 series
#'  redNoise <- function(a1, n) {c(arima.sim(list(ar = a1), n))}
#'
#'  # MC sampling of p values
#'  pval <- array(dim = c(2, nr))
#'  for (i in 1 : nr) {
#'
#'    x <- redNoise(a1.x, nobs)
#'    if (method == "two.sample") y <- redNoise(a1.y, nobs)
#'
#'    if (method == "one.sample") {
#'
#'      tmp1 <- t.test(x)
#'      tmp2 <- t_test(x, a1.x = a1.x)
#'
#'    } else {
#'
#'      tmp1 <- t.test(x, y)
#'      tmp2 <- t_test(x, y, a1.x = a1.x, a1.y = a1.y)
#'
#'    }
#'
#'    # count false positives
#'    if (tmp1$p.value <= 0.05) pval[1, i] <- 1 else pval[1, i] <- 0
#'    if (tmp2$p.value <= 0.05) pval[2, i] <- 1 else pval[2, i] <- 0
#'
#'  }
#'
#'  # return null hypothesis rejection rates (type I error)
#'  c(t.test.default = sum(pval[1, ]) / nr,
#'    t_test = sum(pval[2, ]) / nr)
#'
#' }
#'
#' # The expectation is a rejection rate of around 0.05; in the following
#' # examples we see that the default t.test shows higher than expected
#' # rejection rates for autocorrelated data while the t_test remedies this
#' # issue.
#'
#' estimateRejectionRate(1000, 1000, 0.5)
#' estimateRejectionRate(1000, 1000, 0.9)
#' estimateRejectionRate(1000, 1000, 0.5, method = "two.sample")
#' estimateRejectionRate(1000, 1000, 0.5, 0.7, method = "two.sample")
#' estimateRejectionRate(1000, 1000, 0.2, 0.9, method = "two.sample")
#'
#' @export
#'
t_test <- function (x, y = NULL, a1.x = 0, a1.y = 0,
    alternative = c("two.sided", "less", "greater"),
    mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,  
    ...) {

  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu))) 
    stop("'mu' must be a single number")
  if (!missing(conf.level) && (length(conf.level) != 1 ||
                               !is.finite(conf.level) || 
                               conf.level < 0 || conf.level > 1)) 
    stop("'conf.level' must be a single number between 0 and 1")
  if (!is.null(y)) {
    dname <- paste(deparse1(substitute(x)), "and", deparse1(substitute(y)))
    if (paired) 
      xok <- yok <- complete.cases(x, y)
    else {
      yok <- !is.na(y)
      xok <- !is.na(x)
    }
    y <- y[yok]
  }
  else {
    dname <- deparse1(substitute(x))
    if (paired) 
      stop("'y' is missing for paired test")
    xok <- !is.na(x)
    yok <- NULL
  }
  x <- x[xok]
  if (paired) {
    x <- x - y
    y <- NULL
  }
  ## tmuench, 2023/11/27 ->
  nx <- stattools::getEffectiveDOF(length(x), a1.x)
  ## <-
  mx <- mean(x)
  vx <- var(x)
  if (is.null(y)) {
    if (nx < 2) 
      stop("not enough 'x' observations")
    df <- nx - 1
    stderr <- sqrt(vx/nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx)) 
      stop("data are essentially constant")
    tstat <- (mx - mu)/stderr
    method <- if (paired) 
                "Paired t-test"
              else "One Sample t-test"
    estimate <- setNames(mx, if (paired) 
                               "mean difference"
                             else "mean of x")
  }
  else {
    ## tmuench, 2023/11/27 ->
    ny <- stattools::getEffectiveDOF(length(y), a1.y)
    ## <-
    if (nx < 1 || (!var.equal && nx < 2)) 
      stop("not enough 'x' observations")
    if (ny < 1 || (!var.equal && ny < 2)) 
      stop("not enough 'y' observations")
    if (var.equal && nx + ny < 3) 
      stop("not enough observations")
    my <- mean(y)
    vy <- var(y)
    method <- paste(if (!var.equal) 
                      "Welch", "Two Sample t-test")
    estimate <- c(mx, my)
    names(estimate) <- c("mean of x", "mean of y")
    if (var.equal) {
      df <- nx + ny - 2
      v <- 0
      if (nx > 1) 
        v <- v + (nx - 1) * vx
      if (ny > 1) 
        v <- v + (ny - 1) * vy
      v <- v/df
      stderr <- sqrt(v * (1/nx + 1/ny))
    }
    else {
      stderrx <- sqrt(vx/nx)
      stderry <- sqrt(vy/ny)
      stderr <- sqrt(stderrx^2 + stderry^2)
      df <- stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 
                                                      1))
    }
    if (stderr < 10 * .Machine$double.eps * max(abs(mx), 
                                                abs(my))) 
      stop("data are essentially constant")
    tstat <- (mx - my - mu)/stderr
  }
  if (alternative == "less") {
    pval <- pt(tstat, df)
    cint <- c(-Inf, tstat + qt(conf.level, df))
  }
  else if (alternative == "greater") {
    pval <- pt(tstat, df, lower.tail = FALSE)
    cint <- c(tstat - qt(conf.level, df), Inf)
  }
  else {
    pval <- 2 * pt(-abs(tstat), df)
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    cint <- tstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(tstat) <- "t"
  names(df) <- "df"
  names(mu) <- if (paired) 
                 "mean difference"
               else if (!is.null(y)) 
                 "difference in means"
               else "mean"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, parameter = df, p.value = pval, 
               conf.int = cint, estimate = estimate, null.value = mu, 
               stderr = stderr, alternative = alternative, method = method, 
               data.name = dname)
  class(rval) <- "htest"
  rval
}

# Code adapted from base stats:::ks.test.default as of R version 4.3.1:
# GPL-2 | GPL-3
# Copyright (C) 2023 R Core Team

# Changes to original code by Thomas Muench in 2023/11/27:
# - introduced function parameters `a1.x` and `a1.y` to allow input of AR1
#   autocorrelation parameters for data x and y;
# - use these parameters to reset the number of x/y data points to the effective
#   number of degrees of freedom using Xu's formula neff = n * (1 - a).

#' Kolmogorov-Smirnov Test for autocorrelated data
#'
#' Perform a two-sample Kolmogorov-Smirnov test accounting for autocorrelation
#' of the input data. This function is a modified version of the base R's
#' default two-sample \code{\link[stats]{ks.test}}, which treats autocorrelation
#' after Xu (2013) by resetting the number of observations to \code{n * (1 -a)},
#' where \code{n} are the original number of observations and \code{a} is the
#' user-supplied estimate of the data's autocorrelation parameter at lag
#' one. Note that this code is adapted from the default method of the ks.test
#' function, thus it can only test whether the data are from the same or from
#' different distributions and not test the data against a given cumulative
#' distribution function, nor can it be used with class \code{formula}.
#'
#' @param x a numeric vector of data values.
#' @param y a numeric vector of data values.
#' @param a1.x autocorrelation parameter at lag one of the data in \code{x}.
#' @param a1.y autocorrelation parameter at lag one of the data in \code{y}.
#' @inheritParams stats::ks.test
#' @return see \code{?ks.test}.
#' @seealso \code{\link[stats]{ks.test}}
#' @source \code{stats:::ks.test.default}
#' @references
#'   Xu, X.: Methods in Hypothesis Testing, Markov Chain Monte Carlo and
#'   Neuroimaging Data Analysis, PhD thesis, Harvard University, Cambridge,
#'   Massachusetts, \url{http://nrs.harvard.edu/urn-3:HUL.InstRepos:11108711},
#'   2013.
#' @author R Core Team, Thomas Münch
#' @examples
#'
#' # Check the performance of the test on autocorrelated data
#'
#' # This function Monte Carlo samples autocorrelated data series from an AR1
#' # process and runs the default ks test as well as the stattools ks_test to
#' # estimate the tests' rejection rates, i.e. the relative number of false
#' # positives where the tests reject the null hypothesis
#' estimateRejectionRate <- function(nobs, nr, a1.x, a1.y = a1.x) {
#'
#'  # wrapper for random generation of AR1 series
#'  redNoise <- function(a1, n) {c(arima.sim(list(ar = a1), n))}
#'
#'  # MC sampling of p values
#'  pval <- array(dim = c(2, nr))
#'  for (i in 1 : nr) {
#'
#'    x <- redNoise(a1.x, nobs)
#'    y <- redNoise(a1.y, nobs)
#'
#'    tmp1 <- ks.test(x, y)
#'    tmp2 <- ks_test(x, y, a1.x = a1.x, a1.y = a1.y)
#'
#'    # count false positives
#'    if (tmp1$p.value <= 0.05) pval[1, i] <- 1 else pval[1, i] <- 0
#'    if (tmp2$p.value <= 0.05) pval[2, i] <- 1 else pval[2, i] <- 0
#'
#'  }
#'
#'  # return null hypothesis rejection rates (type I error)
#'  c(ks.test.default = sum(pval[1, ]) / nr,
#'    ks_test = sum(pval[2, ]) / nr)
#'
#' }
#'
#' # The expectation is a rejection rate of around 0.05; in the following
#' # examples we see that the default ks.test shows higher than expected
#' # rejection rates for autocorrelated data while the ks_test remedies this
#' # issue for identical autocorrelation...
#'
#' estimateRejectionRate(1000, 1000, 0.5)
#' estimateRejectionRate(1000, 1000, 0.9)
#'
#' # ..., however, it shows rejection rates > 0.05 if the autoccorelation is
#' # very different, for which the reason is unclear at the moment, with better
#' # rejection rates for small sample sizes
#'
#' estimateRejectionRate(1000, 1000, 0.5, 0.6)
#' estimateRejectionRate(1000, 1000, 0.5, 0.8)
#' estimateRejectionRate(99, 1000, 0.5, 0.8)
#'
#' @export
#'
ks_test <- function (x, y, a1.x = 0, a1.y = 0,
                     alternative = c("two.sided", "less", "greater"), 
                     exact = NULL, simulate.p.value = FALSE, B = 2000) {

  alternative <- match.arg(alternative)
  DNAME <- paste(deparse1(substitute(x)), "and", deparse1(substitute(y)))
  x <- x[!is.na(x)]
  n.x <- length(x)
  if (n.x < 1L) 
    stop("not enough 'x' data")
  PVAL <- NULL
  if (is.ordered(y)) 
    y <- unclass(y)
  y <- y[!is.na(y)]
  n.y <- length(y)
  if (n.y < 1L) 
    stop("not enough 'y' data")
  n.x <- as.double(n.x)
  if (is.null(exact)) 
    exact <- (n.x * n.y < 10000)
  if (!simulate.p.value) {
    METHOD <- paste(c("Asymptotic", "Exact")[exact + 1L],
                    "two-sample Kolmogorov-Smirnov test")
  }
  else {
    METHOD <- "Monte-Carlo two-sample Kolmogorov-Smirnov test"
  }
  TIES <- FALSE
  w <- c(x, y)
  z <- cumsum(ifelse(order(w) <= n.x, 1/n.x, -1/n.y))
  if (length(unique(w)) < (n.x + n.y)) {
    z <- z[c(which(diff(sort(w)) != 0), n.x + n.y)]
    TIES <- TRUE
    if (!exact && !simulate.p.value) 
      warning("p-value will be approximate in the presence of ties")
  }
  STATISTIC <- switch(alternative, two.sided = max(abs(z)), 
                      greater = max(z), less = -min(z))
  nm_alternative <- switch(alternative, two.sided = "two-sided", 
                           less = "the CDF of x lies below that of y",
                           greater = "the CDF of x lies above that of y")
  z <- NULL
  if (TIES) 
    z <- w
  ## tmuench, 2023/11/27 ->
  n.x.eff <- n.x * (1 - a1.x)
  n.y.eff <- n.y * (1 - a1.y)
  PVAL <- switch(alternative,
                 two.sided = psmirnov(STATISTIC, sizes = c(n.x.eff, n.y.eff),
                                      z = w, exact = exact,
                                      simulate = simulate.p.value, B = B,
                                      lower.tail = FALSE),
                 less = psmirnov(STATISTIC, sizes = c(n.x.eff, n.y.eff), ,
                                 z = w, exact = exact,
                                 simulate = simulate.p.value, B = B,
                                 two.sided = FALSE, lower.tail = FALSE),
                 greater = psmirnov(STATISTIC, sizes = c(n.x.eff, n.y.eff), ,
                                    z = w, exact = exact,
                                    simulate = simulate.p.value, B = B,
                                    two.sided = FALSE, lower.tail = FALSE))
  ## <-
  if (simulate.p.value)
    PVAL <- (1 + (PVAL * B))/(B + 1)

  names(STATISTIC) <- switch(alternative, two.sided = "D", 
                             greater = "D^+", less = "D^-")
  PVAL <- min(1, max(0, PVAL))
  RVAL <- list(statistic = STATISTIC, p.value = PVAL,
               alternative = nm_alternative, method = METHOD, data.name = DNAME,
               data = list(x = x, y = y), exact = exact)
  class(RVAL) <- c("ks.test", "htest")
  return(RVAL)
}
