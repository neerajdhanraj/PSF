#' Forecasting of univariate time series using the PSF algorithm
#'
#' Takes an univariate time series and the prediction horizon as inputs.
#' @param data Input univariate time series, in any format (time series (ts), vector, matrix, list, data frame).
#' @param n.ahead The number of predicted values to be obtained.
#' @param k The number of clusters, or a vector of candidate values to search for the optimum automatically.
#' @param w The window size, or a vector of candidate values to search for the optimum automatically.
#' @param cycle The number of values that conform a cycle in the time series (e.g. 24 hours per day). Only used when input data is not in time series format.
#' @return A list with 3 elements:
#' \item{predictions}{Vector with the resulting predictions}
#' \item{k}{Number of clusters used}
#' \item{w}{Window size used}
#' @export
#' @examples
#' ## Forecast the next 12 values of the univariate time series: nottem (package:datasets).
#' psf(nottem, 12)
#'
#' ## Forecast the next 48 values of the univariate time series: sunspots (package:datasets).
#' psf(sunspots, 48)
psf <- function (data, n.ahead, k = seq(2,10), w = seq(1,10), cycle = 24) {

  # Step 1. Convert input data to internal format (data.table).
  series = convert_datatype(data)
  if (is.ts(data)) cycle = frequency(data)

  # Step 2. Check integrity of data (both its size and n.ahead must be multiple of cycle).
  fit = nrow(series) %% cycle
  if (fit > 0) {
    warning(paste("Time series length is not multiple of", cycle, ". Cutting last", fit, "values!"))
    series = series[1:(.N - fit)]
  }
  original.n.ahead = n.ahead
  fit = n.ahead %% cycle
  if (fit > 0) {
    n.ahead = cycle * ceiling(n.ahead / cycle)
    warning(paste("Prediction horizon is not multiple of", cycle, ". Using", n.ahead, "as prediction horizon!"))
  }

  # Step 3. Normalize data.
  dmin = series[, min(data)]; dmax = series[, max(data)]
  series[, data := (data - dmin) / (dmax - dmin)]

  # Step 4. Reshape data according to the cycle of the time series.
  dataset = as.data.table(t(matrix(series[,data], nrow = cycle)))

  # Step 5. Find optimal number (K) of clusters (or use the value specified by the user).
  if (length(k) > 1)
    k = optimum_k(dataset, k)

  # Step 6. Find optimal window size (W) (or use the value specified by the user).
  if (length(w) > 1)
    w = optimum_w(dataset, k, w, cycle)

  # Step 7. Predict the 'n.ahead' next values for the time series.
  preds = as.vector(t(psf_predict(dataset, k, w, n.ahead, cycle)))[1:original.n.ahead]

  # Step 8. Denormalize predicted data.
  preds = preds * (dmax - dmin) + dmin

  # Step 9. Return both the predictions and the model.
  res = list(predictions = preds, k = k, w = w)
  return(res)

}
