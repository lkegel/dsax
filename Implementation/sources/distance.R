distance_run <- function(method, repr) {
  idx_list <- vector("list", length(repr) * (length(repr) + 1) / 2)
  i <- 1
  for (irow in seq(length(repr))) {
    for (icol in seq(irow, length(repr))) {
      idx_list[[i]] <- c(irow, icol)
      i <- i + 1
    }
  }

  tmp <- tempfile()
  print(paste("Distance temp file: ", tmp))
  cl <- makeCluster(detectCores(), outfile = tmp)
  clusterExport(cl, "method", environment())
  clusterExport(cl, "repr", environment())
  clusterEvalQ(cl, library(idxrepr))
  entries <- parallel::parLapply(cl, idx_list, function(x) {
    if (x[1] == x[2] && x[1] %% 10 == 0) print(x)
    mgr_distance(method, repr[[x[1]]], repr[[x[2]]])
  })
  stopCluster(cl)

  # entries <- lapply(idx_list[100], function(x) {
  #   print(x)
  #   if (x[1] == x[2] && x[1] %% 10 == 0) print(x)
  #   idxrepr::mgr_distance(method, repr[[x[1]]], repr[[x[2]]])
  # })

  m <- matrix(NA, nrow = length(repr), length(repr))
  rownames(m) <- colnames(m) <- names(repr)
  for (i in seq_along(idx_list)) {
    m[idx_list[[i]][1], idx_list[[i]][2]] <- entries[[i]]
    m[idx_list[[i]][2], idx_list[[i]][1]] <- entries[[i]]
  }

  return(m)
}

distance_lower_bounding <- function(dataset_config, rd) {
  ed <- util_read(dataset_config, "euclidean_distance")
  lb <- rd / ed
  
  rd_0 <- which(rd == 0, arr.ind = T)
  ed_0 <- which(ed == 0, arr.ind = T)
  m <- merge(rd_0, ed_0, by = c("row", "col"))

  for (i in seq(nrow(m))) {
    lb[m[i, 1], m[i, 2]] <- 1
  }
  
  diag(lb) <- 1
  
  return(lb)
}
