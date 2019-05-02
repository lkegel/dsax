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