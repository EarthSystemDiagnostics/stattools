#' Lag correlation analysis
#'
#' Find the optimal shift (positive or negative) between two data vectors from
#' maximising the correlation within their common overlap.
#'
#' @param y1 numeric vector of first data series.
#' @param y2 numeric vector of second data series which is shifted relative to
#'   \code{y1}; needs to have the same length as \code{y1}.
#' @param shifts integer vector of shifts (can be positive, negative, or
#'   both). For each value, \code{y2} is shifted the corresponding number of
#'   elements earlier or later, and the correlation with \code{y1} is computed.
#' @param unit optional value for the unit which matches the shifts to the
#'   physical scale on which the data is tabulated.
#' @param plot logical; plot an optional graph of correlation versus tried
#'   shifts.
#' @param xlab optional x axis label for the plot.
#' @param ylab optional y axis label for the plot.
#' @return Data frame with one row with the optimal shift and the corresponding
#'   correlation value; if a \code{unit} is provided, it includes an additional
#'   column with the optimal shift in the real units.
#' @author Thomas Münch
#' @examples
#' y1 <- rnorm(100)
#' y2 <- rnorm(100)
#'
#' ShiftCorrelation(y1, y2, shifts = (-10 : 10))
#'
#' # display optimal shift in the real units of the data:
#' ShiftCorrelation(y1, y2, shifts = (-10 : 10), unit = 3)
#' @export
ShiftCorrelation <- function(y1, y2, shifts, unit = NULL,
                             plot = FALSE, xlab = NULL, ylab = NULL) {

  if (length(y1) != length(y2)) {
    stop("'y1' and 'y2' must have the same length.", call. = FALSE)
  }

  res <- data.frame(
    i = shifts,
    cor = sapply(shifts, function(k) {
      stats::cor(y1, Hmisc::Lag(y2, k), use = "pairwise.complete.obs")})
  )

  optim <- res[which.max(res$cor), ]

  if (plot) {

    pnt <- optim
    if (length(unit)) {
      res$i <- res$i * unit
      pnt$i <- pnt$i * unit
      if (!length(xlab)) xlab <- "Shift in real units"
    }

    if (!length(xlab)) xlab <- "Bin shift"
    if (!length(ylab)) ylab <- "Correlation"

    graphics::plot(res, type = "b",
                   xlab = xlab, ylab = ylab, pch = 19, lwd = 1.5)
    graphics::points(pnt, pch = 19, col = "red", cex = 1.25)

  }

  if (length(unit)) {
    optim$shift <- optim$i * unit
    optim <- optim[, c(1, 3, 2)]
  }

  return(optim)

}
