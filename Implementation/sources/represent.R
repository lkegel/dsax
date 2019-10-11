represent_run <- function(dataset, method) {
  cl <- makeCluster(detectCores() - 1)
  suppressPackageStartupMessages(library(idxrepr))
  clusterExport(cl, "method", environment())
  repr <- parallel::parLapply(cl, dataset, function(x) {
    idxrepr::mgr_represent(method, as.numeric(x$ts))
  })
  stopCluster(cl)

  return(repr)
}

represent_run_large <- function(dataset_config, fp, method) {
  I <- dataset_config$I
  TT <- dataset_config$T
  
  tmp <- tempfile()
  print(tmp)
  cl <- makeCluster(detectCores() - 1, outfile = tmp)
  clusterEvalQ(cl, "suppressPackageStartupMessages(library(idxrepr))")
  clusterExport(cl, "method", environment())
  clusterExport(cl, "util_read_c", environment())
  clusterExport(cl, "I", environment())
  clusterExport(cl, "TT", environment())
  i <- 0
  repr <- parallel::parLapply(cl, as.list(seq(I)), function(i) {
    print(i)
    x <- util_read_c(fp, TT, type = "float8", pos1 = i - 1, pos2 = TT)
    idxrepr::mgr_represent(method, x)})
  stopCluster(cl)
  unlink(tmp)
  return(repr)
}