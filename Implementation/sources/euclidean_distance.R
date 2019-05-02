euclidean_distance <- function(dataset_configs, force = F) {
  for (config in dataset_configs) {
    if (!force && util_exists(config, "euclidean_distance")) {
      next
    }
    osl <- util_read(config, "dataset")
    TT <- config$T
    I <- config$I

    m <- matrix(0, nrow = I, ncol = I)
    colnames(m) <- names(osl)
    rownames(m) <- names(osl)

    for (i in seq(I - 1)) {
      print(i)
      for (j in seq(i + 1, I)) {
        DD <- sqrt(sum((osl[[i]]$ts - osl[[j]]$ts)**2))
        m[i, j] <- m[j, i] <- DD
      }
    }

    util_save(config, "euclidean_distance", m)
  }
}
