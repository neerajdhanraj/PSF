optimum_w <- function (dataset, k, w_values, cycle) {

  # Step 1. Take validation test out from training.
  test = dataset[.N]
  training = dataset[1:.N-1]
  n = training[, .N]

  # Step 2. Find the window size (W) that minimizes the error.
  min.err = Inf
  for (w in w_values) {
    if (w > 0 & w < n) {

      # 2.1 Perform prediction with the current 'w' value.
      pred = psf_predict(training, k, w, cycle, cycle)

      # 2.2 Evaluate error and update the minimum.
      err = sum(abs(pred - test)) / cycle
      # cat("w =", w, " ; err =", err, "\n") # To show debug info.
      if (err < min.err) {
        min.err = err; best.w = w
      }

    }
  }

  return(best.w)
}
