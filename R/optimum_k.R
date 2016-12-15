optimum_k <- function (dataset, k_values) {

  # Step 1. Store number of instances and their distance matrix.
  n = nrow(dataset); d = dist(dataset)

  # Step 2. Find best number of clusters.
  best.s = -Inf
  for (k in k_values) {
    if (k > 1 & k < n) {

      # 2.1a Using algorithm kMeans for clustering.
      cl = kmeans(dataset, k)
      clusters = cl$cluster

      # 2.1b Using algorithm PAM for clustering.
      # cl = pam(dataset, k)
      # clusters = cl$clustering

      # 2.2 Evaluate clustering using silhouette index.
      s = sum(silhouette(clusters, d)[,3]) / n

      # 2.3 Store best k value so far.
      # cat("k =", k, " ; s =", s, "\n") # To show debug info.
      if (s > best.s) {
        best.s = s; best.k = k
      }

    }
  }

  return(best.k)
}
