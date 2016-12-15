psf_predict <- function (dataset, k, w, n.ahead, cycle) {
  temp = copy(dataset)
  n.ahead.cycles = n.ahead / cycle  # Assuming that n.ahead.cycles >= 1

  n = 1; cw = w
  while (n <= n.ahead.cycles) {

    # Step 1. Dataset clustering (if window size was not reduced).
    if (cw == w) clusters = as.data.table(kmeans(temp, k)$cluster)

    # Step 2. Take the cluster pattern of the test.
    pattern = clusters[(.N - cw + 1):.N]

    # Step 3. Find the pattern in training data (neighbors).
    neighbors = which(sapply(1:(clusters[,.N] - cw), function(x) all(clusters[x:(x + cw - 1)] == pattern)))

    # Step 4. Check for patterns found.
    if (length(neighbors) == 0) { # If no patterns were found,
      cw = cw - 1                 # decrease the window size.

      if (cw == 0) { # If any window size produce neighbors,
        # use the last training instance as the prediction.
        temp = rbindlist(list(temp, dataset[.N]))

        # Set the current window to its initial value and take next horizon.
        cw = w; n = n + 1

        warning("No patterns were found in training for any window size.\nUsing last training as the prediction!")
      }
    }
    else { # If some patterns were found.

      # cat("Number of patterns found:", length(neighbors), "\n") # To show debug info.

      # Step 5. Assess the average of the neighbors classes.
      pred = as.data.table(colMeans(temp[neighbors + cw]))

      # Step 6. Append prediction to produce the following ones.
      temp = rbindlist(list(temp, as.data.table(t(pred))))

      # Step 7. Set the current window to its initial value and take next horizon.
      cw = w; n = n + 1

    }
  }
  return(temp[(.N - n.ahead.cycles + 1):.N])
}
