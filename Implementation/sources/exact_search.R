exact_search_run <- function(dataset_config, method_config) {
  method <- intermediate_read(dataset_config, method_config, "method")
  ed <- util_read(dataset_config, "euclidean_distance")
  repr <- intermediate_read(dataset_config, method_config, "represent")
  distance <- intermediate_read(dataset_config, method_config, "distance")
  name_c <- names(repr)
  
  df <- NA
  
  n_raw_c <- c()
  I_query <- seq_along(repr)
  tmp <- tempfile()
  print(paste("Exact search temp file: ", tmp))
  cl <- makeCluster(detectCores(), outfile = tmp)
  clusterExport(cl, "exact_search", environment())
  clusterExport(cl, "method", environment())
  clusterExport(cl, "ed", environment())
  clusterExport(cl, "name_c", environment())
  clusterExport(cl, "distance", environment())
  clusterEvalQ(cl, suppressPackageStartupMessages(library(idxrepr)))
  
  es_list <- parallel::parLapply(cl, as.list(I_query), function(x) {
    return(exact_search(method, ed, name_c[x], name_c[-x], distance))
  })
  stopCluster(cl)
  
  return(es_list)
}

exact_search <- function(method, ed, query.name, repr.names, distance, debug = F) {
  # query.repr <- idxrepr::mgr_represent(method, query.raw)
  print(query.name)
  dist.repr <- distance[query.name, repr.names]
  
  dist.ord <- order(unlist(dist.repr))
  nn.name <- NA
  nn.dist <- Inf
  n_raw <- 0
  
  # Debug
  if (isTRUE(debug)) dist.true <- c()
  
  for (j in seq_along(dist.ord)) {
    ord <- dist.ord[j]
    cand.name <- names(dist.repr)[ord]
    cand.dist.repr <- dist.repr[[cand.name]]
    if(cand.dist.repr > nn.dist)
      break
    cand.dist <- ed[query.name, cand.name]
    # idxrepr::mgr_distance(ed, query.raw, osl_[[]]$ts)
    if (isTRUE(debug)) dist.true <- c(dist.true, cand.dist)
    n_raw <- n_raw + 1
    if (cand.dist < nn.dist) {
      nn.name <- cand.name
      nn.dist <- cand.dist
    }
  }
  
  # Debug
  if (isTRUE(debug)) {
    mima <- c(min(dist.repr[dist.ord], dist.true),
              max(dist.repr[dist.ord], dist.true))
    plot(dist.repr[dist.ord], ylim = mima)
    lines(dist.true)
    lines(rep(min(dist.true), I), col = "green")
  }
  
  
  result <- list(
    name = nn.name,
    raw = nn.dist,
    repr = dist.repr[nn.name],
    n_repr = length(repr.names),
    n_raw = n_raw
  )
  
  result
}
