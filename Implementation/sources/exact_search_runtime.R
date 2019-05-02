exact_search_runtime_run <- function(dataset_config, method_config) {
  method <- intermediate_read(dataset_config, method_config, "method")
  dataset <- util_read(dataset_config, "dataset")
  repr <- intermediate_read(dataset_config, method_config, "represent")
  name_c <- names(repr)

  df <- NA

  n_raw_c <- c()
  I_query <- seq_along(repr)

  overall <- tic()
  if (class(method) == "ed") {
    es_list <- lapply(as.list(I_query), function(x) {
      return(exact_search_runtime_ed(method, dataset, repr, name_c[x], name_c[-x]))
    })
  } else {
    ed <- mgr_init("ed")
    es_list <- lapply(as.list(I_query), function(x) {
      return(exact_search_runtime(method, ed, dataset, repr, name_c[x], name_c[-x]))
    })
  }
  elapsed <- toc(overall)
  return(list(es_list, elapsed = elapsed))
}

prepare_exact_search_runtime_dataset <- function(dataset_config) {
  dataset <- util_read(dataset_config, "dataset")
  dat <- (unname(unlist(lapply(dataset, function(x) as.numeric(x$ts)))))
  fp <- util_get_filepath(dataset_config, "dataset", ext = "tbl")
  write.table(dat, fp, col.names = F, row.names = F, dec = ".")
  print(paste("Dataset exported: ", fp))
}

prepare_exact_search_runtime_method <- function(dataset_config, method_config_na) {
  method_config <- method_derive(dataset_config, method_config_na)
  method <- intermediate_read(dataset_config, method_config, "method")
  repr <- intermediate_read(dataset_config, method_config, "represent")

  if (class(method) %in% "sax") {
    lut <- method$lut
    file_name <- intermediate_file_name(method_config)
    ext <- ".tbl"
    prepare_exact_search_runtime_lut(dataset_config, lut, file_name, ext)
  } else if (class(method) %in% "seassaxres") {
    lut <- method$sax$lut
    file_name <- intermediate_file_name(method_config)
    ext <- "res.tbl"
    prepare_exact_search_runtime_lut(dataset_config, lut, file_name, ext)
    
    lut <- method$seassax$sax$lut
    file_name <- intermediate_file_name(method_config)
    ext <- "seas.tbl"
    prepare_exact_search_runtime_lut(dataset_config, lut, file_name, ext)
  } else {
    stop("n/a")
  }
}

prepare_exact_search_runtime_lut <- function(dataset_config, lut, file_name, ext) {
  dat <- as.vector(lut)
  fp <- util_get_filepath(dataset_config, file_name, "method", ext)
  write.table(dat, fp, col.names = F, row.names = F, dec = ".")
  print(paste("Lookup table exported: ", fp))
}

prepare_exact_search_runtime_repr <- function(dataset_config, method_config_na) {
  method_config <- method_derive(dataset_config, method_config_na)
  repr <- intermediate_read(dataset_config, method_config, "represent")
  dat <- (unname(unlist(lapply(repr, function(x) as.numeric(x)))))
  file_name <- intermediate_file_name(method_config)
  fp <- util_get_filepath(dataset_config, file_name, "represent", ext = "tbl")
  write.table(dat, fp, col.names = F, row.names = F, dec = ".")
  print(paste("Representation table exported: ", fp))
  
}

exact_search_runtime <- function(method, ed, dataset, repr, query.name, repr.names) {
  print(query.name)
  query.repr <- repr[[query.name]]

  el_repr <- 0
  dist.repr <- sapply(repr.names, function(repr.name) {
    res <- mgr_distance(method, query.repr, repr[[repr.name]], tic = T)
    el_repr <<- el_repr + res$elapsed
    return(res$distance)
  })

  dist.ord <- order(unlist(dist.repr))
  nn.name <- NA
  nn.dist <- Inf
  n_raw <- 0

  el_raw <- 0
  for (j in seq_along(dist.ord)) {
    ord <- dist.ord[j]
    cand.name <- names(dist.repr)[ord]
    cand.dist.repr <- dist.repr[[cand.name]]
    if(cand.dist.repr > nn.dist)
      break

    query.raw <- dataset[[query.name]]$ts
    cand.raw <- dataset[[cand.name]]$ts
    res <- mgr_distance(ed, query.raw, cand.raw, tic = T)
    el_raw <- el_raw + res$elapsed
    cand.dist <- res$distance

    n_raw <- n_raw + 1
    if (cand.dist < nn.dist) {
      nn.name <- cand.name
      nn.dist <- cand.dist
    }
  }

  result <- list(
    name = nn.name,
    raw = nn.dist,
    repr = dist.repr[nn.name],
    n_repr = length(repr.names),
    n_raw = n_raw,
    el_repr = el_repr,
    el_raw = el_raw
  )

  result
}

exact_search_runtime_ed <- function(method, dataset, repr, query.name, repr.names) {
  print(query.name)
  query.repr <- repr[[query.name]]

  el_repr <- 0
  dist.repr <- sapply(repr.names, function(repr.name) {
    cand.repr <- repr[[repr.name]]
    res <- mgr_distance(method, query.repr, cand.repr, tic = T)
    el_repr <<- el_repr + res$elapsed
    return(res$distance)
  })

  dist.ord <- order(unlist(dist.repr))
  ord <- dist.ord[1]
  nn.name <- names(dist.repr)[ord]
  nn.dist <- dist.repr[ord]
  n_raw <- length(repr.names)

  result <- list(
    name = nn.name,
    raw = nn.dist,
    repr = dist.repr[nn.name],
    n_repr = length(repr.names),
    n_raw = n_raw,
    el_repr = el_repr,
    el_raw = 0
  )

  result
}
