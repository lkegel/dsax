run_method <- function(dataset_configs, method_configs, force = T) {
  for (dataset_config in dataset_configs) {
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      
      if (force || 
          !intermediate_exists(dataset_config, method_config, "method")) {
        dc <- util_get_filepath(dataset_config, "foo")
        mc <- intermediate_file_name(method_config)
        print(paste("Run method for", dc, mc))
        method <- method_get(dataset_config, method_config)
        intermediate_save(dataset_config, method_config, method, "method")  
      }
    }
  }
}

run_represent <- function(dataset_configs, method_configs, force = T,
                          large = F) {
  run_method(dataset_configs, method_configs, force)
  
  for (dataset_config in dataset_configs) {
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      method <- intermediate_read(dataset_config, method_config, "method")
      
      if (force || 
          !intermediate_exists(dataset_config, method_config, "represent")) {
        dc <- util_get_filepath(dataset_config, "foo")
        mc <- intermediate_file_name(method_config)
        print(paste("Run represent for", dc, mc))
        
        if (large) {
          fp <- util_get_filepath(dataset_config, "dataset", ext = "dat")
          repr <- represent_run_large(dataset_config, fp, method)
          repr <- unname(unlist(repr))
          filename <- intermediate_file_name(method_config)
          fp_repr <- util_get_filepath(dataset_config, filename,
                                       subdir = "represent", ext = "dat")
          print(fp_repr)
          util_write_c(fp_repr, repr, type = "uint2", append = F)
        } else {
          dataset <- util_read(dataset_config, "dataset")
          repr <- represent_run(dataset, method)
          intermediate_save(dataset_config, method_config, repr, "represent")    
        }
      }
    }
  }
}

run_distance <- function(dataset_configs, method_configs, force = T) {
  run_represent(dataset_configs, method_configs, force)
  
  for (dataset_config in dataset_configs) {
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      method <- intermediate_read(dataset_config, method_config, "method")
      repr <- intermediate_read(dataset_config, method_config, "represent")
      
      if (force || 
          !intermediate_exists(dataset_config, method_config, "distance")) {
        dc <- util_get_filepath(dataset_config, "foo")
        mc <- intermediate_file_name(method_config)
        print(paste("Run distance for", dc, mc))
        distance <- distance_run(method, repr)
        intermediate_save(dataset_config, method_config, distance, "distance")  
      }
    }
  }
}

run_lower_bounding <- function(dataset_configs, method_configs, force = T) {
  run_distance(dataset_configs, method_configs, force)
  
  for (dataset_config in dataset_configs) {
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      distance <- intermediate_read(dataset_config, method_config, "distance")

      if (force || 
          !intermediate_exists(dataset_config, method_config, "lower-bounding"))
        {
        dc <- util_get_filepath(dataset_config, "foo")
        mc <- intermediate_file_name(method_config)
        print(paste("Run lower bounding for", dc, mc))
        lb <- distance_lower_bounding(dataset_config, distance)
        intermediate_save(dataset_config, method_config, lb,
                          "lower-bounding")
      }
    }
  }
}

run_exact_search <- function(dataset_configs, method_configs, force = T) {
  run_lower_bounding(dataset_configs, method_configs, force)
  
  for (dataset_config in dataset_configs) {
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      if (force || 
          !intermediate_exists(dataset_config, method_config, "exact-search"))
      {
        dc <- util_get_filepath(dataset_config, "foo")
        mc <- intermediate_file_name(method_config)
        print(paste("Run exact search for", dc, mc))
        es <- exact_search_run(dataset_config, method_config)
        intermediate_save(dataset_config, method_config, es, "exact-search")
      }
    }
  }
}

run_approximate_search <- function(dataset_configs, method_configs, force = T) {
  run_lower_bounding(dataset_configs, method_configs, force)
  
  for (dataset_config in dataset_configs) {
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      if (force || 
          !intermediate_exists(dataset_config, method_config, "approximate-search"))
      {
        dc <- util_get_filepath(dataset_config, "foo")
        mc <- intermediate_file_name(method_config)
        print(paste("Run approximate search for", dc, mc))
        es <- approximate_search_run(dataset_config, method_config)
        intermediate_save(dataset_config, method_config, es, "approximate-search")
      }
    }
  }
}

run_exact_search_runtime <- function(dataset_configs, method_configs, force = T) {
  run_represent(dataset_configs, method_configs, force)
  
  for (dataset_config in dataset_configs) {
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      if (force || 
          !intermediate_exists(dataset_config, method_config, "exact-search-runtime"))
      {
        dc <- util_get_filepath(dataset_config, "foo")
        mc <- intermediate_file_name(method_config)
        print(paste("Run exact search runtime for", dc, mc))
        es <- exact_search_runtime_run(dataset_config, method_config)
        intermediate_save(dataset_config, method_config, es, "exact-search-runtime")
      }
    }
  }
}

run_exact_search_runtime_c <- function(drive, dataset_configs, method_configs,
                                       I, Q) {
  for (dataset_config in dataset_configs) {
    to <- file.path(Sys.getenv(drive), util_get_path(dataset_config))
    for (method_config_na in method_configs) {
      dc <- util_get_filepath(dataset_config, "foo")
      mc <- method_config_na[[1]]
      print(paste("Run exact search runtime for", dc, mc))
      
      fn_dataset <- "dataset.dat"
      TT <- dataset_config$T
      if (method_config_na[[1]] != "ed") {
        method_config <- method_derive(dataset_config, method_config_na)
        fn <- intermediate_file_name(method_config)
        fn_repr <- file.path("represent", paste0(fn, ".dat"))
        fn_es <- paste(fn, I, "csv", sep = ".")
        if (method_config[[1]] == "sax") {
          fn_res <- file.path("method", paste0(fn, ".dat"))
          fn_det <- "foo"
          dist <- "SAX"
          A_res <- method_config$a
          A_det <- 0
          W <- method_config$w
          L <- 0
        } else if (method_config[[1]] == "seassaxres") {
          fn_res <- file.path("method", paste0(fn, ".res.dat"))
          fn_det <- file.path("method", paste0(fn, ".det.dat"))
          dist <- "sSAX"
          A_res <- method_config$a
          A_det <- method_config$a_seas
          W <- method_config$w
          L <- dataset_config$L_1
        } else stop("N/A")
      } else {
        fn_res <- "foo"
        fn_det <- "foo"
        fn_repr <- "foo"
        fn_es <- paste("ed", I, "csv", sep= ".")
        dist <- "ED"
        A_res <- 0
        A_det <- 0
        W <- 0
        L <- 0
      }
      
      # system("powershell -Command \"Write-VolumeCache d\"")
      
      Qs <- paste(Q, collapse = " ")
      cmd <- file.path(Sys.getenv("EDBT2020"), "Implementation", "c",
                       "exact_search", "Release", "exact_search.exe")
      args <- paste(to, fn_dataset, fn_res, fn_det, fn_repr, fn_es, dist,
                    I, TT, A_res, A_det, W, L, Qs, collapse = " ")
      system(paste(cmd, args))
    }
  }
}
