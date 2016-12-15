#' Plot actual and forecasted values of an univariate time series
#'
#' Takes an univariate time series and a vector with forecasted values.
#' @param data Input univariate time series (in either time series (ts) or vector format). All values should be numeric.
#' @param predictions A vector with already forecasted values.
#' @param cycle The number of values that conform a cycle in the time series (e.g. 24 hours per day, 12 month per year, and so on). Only used when input data is not in time series (ts) format.
#' @param \dots Additional graphical parameters given to plot function.
#' @export
#' @examples
#' ## Forecast the next 12 values of the univariate time series: nottem (package:datasets).
#' res <- psf(nottem, 12)
#'
#' ## Plot forecasted values from PSF.
#' psf_plot(nottem, res$predictions)
psf_plot <- function (data, predictions, cycle = 24, ...) {

  if (is.ts(data))
    all = ts(c(data, predictions), start=start(data), frequency = frequency(data))
  else
    all = ts(c(data, predictions), start=1, frequency = cycle)

  args <- list(...)

  if (length(args) == 0)
  {
    plot(window(all, end = time(all)[length(all) - length(predictions)]), xlim = c(time(all)[1], time(all)[length(all)]), xlab = "Time", ylab = "Value")
    points(window(all, start = time(all)[length(all) - length(predictions) + 1]), type = "o", col = "blue", lty = 3, pch = 16, cex = 0.4)
  }
  else
  {
    plot(window(all, end = time(all)[length(all) - length(predictions)]), xlim = c(time(all)[1], time(all)[length(all)]), ...)
    points(window(all, start = time(all)[length(all) - length(predictions) + 1]), type = "o", col = "blue", ...)
  }

}
