method_get <- function(dataset_config, method_config) {
  builder <- list()
  
  if (method_config[[1]]  %in% c("paa",
                                 "sax",
                                 "lrres",
                                 "lrrespaa",
                                 "lrressax",
                                 "lrrpaa",
                                 "lrrsaxpaa",
                                 "lrrsaxres",
                                 "lrrressax",
                                 "seaspaa",
                                 "seassaxpaa",
                                 "seassaxres",
                                 "1d_sax")) {
    builder[["paa"]] <- idxrepr::mgr_set_config(idxrepr::mgr_init("paa"),
                                                list(n = dataset_config$T,
                                                     w = method_config$w))
  } else if (method_config[[1]] == "ed") {
    return(idxrepr::mgr_init("ed"))
  }
  
  if (method_config[[1]] %in% c("sax", "lrres", "lrressax", "lrrsaxres",
                                "lrrressax", "seassaxres", "1d_sax")) {
    builder[["sax"]] <- idxrepr::mgr_init("sax")
    config <- list()
    config$a <- floor(method_config$a)
    config$paa <- builder[["paa"]]
    config$dist_type <- method_config$`dist-type`
    if (config$dist_type == "norm") {
      config$dist_param <- list(mean = 0, sd = method_config$sd) 
    } else if (config$dist_type == "user") {
      paa <- method_res(dataset_config, method_config)
      config$dist_param <- list(ecdf = ecdf(unlist(paa)))
    }
    builder[["sax"]] <- idxrepr::mgr_set_config(builder[["sax"]], config)
    
    if (method_config[[1]] == "1d_sax") {
      stopifnot(config$dist_type == "norm")
      config$a <- floor(method_config$a_s)
      config$dist_param$sd = sqrt(0.03 / (dataset_config$T / method_config$w))
      builder[["sax_s"]] <- idxrepr::mgr_set_config(builder[["sax"]], config)
    }
  }

  if (method_config[[1]] == "lrres") {
    builder[["lrres"]] <- idxrepr::mgr_init("lrres")
    builder[["lrres"]]$lr$TT <- dataset_config$T
    builder[["lrres"]]$sax <- builder[["sax"]]
  } else if (method_config[[1]] == "lrrespaa") {
    builder[["lrrespaa"]] <- idxrepr::mgr_init("lrrespaa")
    builder[["lrrespaa"]]$lr$TT <- dataset_config$T
    builder[["lrrespaa"]]$paa <- builder[["paa"]]
  } else if (method_config[[1]] == "lrrsaxres" ||
             method_config[[1]] == "lrrsaxpaa" ||
             method_config[[1]] == "lrrpaa" ||
             method_config[[1]] == "lrrressax") {
    builder[["lrr"]] <- idxrepr::mgr_set_config(idxrepr::mgr_init("lrr"),
                                                list(TT = dataset_config$T))
    
    if (method_config[[1]] == "lrrsaxres" ||
        method_config[[1]] == "lrrsaxpaa") {
      builder[["lrrsax"]] <- idxrepr::mgr_init("lrrsax")
      builder[["lrrsax"]]$lrr <- builder[["lrr"]]
      builder[["lrrsax"]] <- idxrepr::mgr_set_config(builder[["lrrsax"]],
                                                     list(a = floor(method_config$a_lrr))) 
      
      if (method_config[[1]] == "lrrsaxpaa") {
        builder[["lrrsaxpaa"]] <- idxrepr::mgr_init("lrrsaxpaa")
        builder[["lrrsaxpaa"]]$lrrsax <- builder[["lrrsax"]]
        builder[["lrrsaxpaa"]]$paa <- builder[["paa"]]
      } else if (method_config[[1]] == "lrrsaxres") {
        builder[["lrrsaxres"]] <- idxrepr::mgr_init("lrrsaxres")
        builder[["lrrsaxres"]]$lrrsax <- builder[["lrrsax"]]
        builder[["lrrsaxres"]]$sax <- builder[["sax"]]
      } else {
        stop("N/A")
      }
    } else if (method_config[[1]] == "lrrpaa") {
      builder[["lrrpaa"]] <- idxrepr::mgr_init("lrrpaa")
      builder[["lrrpaa"]]$lrr <- builder[["lrr"]]
      builder[["lrrpaa"]]$paa <- builder[["paa"]]
    } else if (method_config[[1]] == "lrrressax") {
      builder[["lrrressax"]] <- idxrepr::mgr_init("lrrressax")
      builder[["lrrressax"]]$lrr <- builder[["lrr"]]
      builder[["lrrressax"]]$sax <- builder[["sax"]]
    } else {
      stop("N/A")
    }
  } else if (method_config[[1]] == "1d_sax") {
    builder[["lr"]] <- idxrepr::mgr_init("lr")
    builder[["lr"]]$TT <- dataset_config$T / method_config$w
  }
  
  # SEAS*
  if (method_config[[1]] == "seassaxres" ||
      method_config[[1]] == "seassaxpaa" ||
      method_config[[1]] == "seaspaa") {
    builder[["seas"]] <- idxrepr::mgr_set_config(idxrepr::mgr_init("seas"),
                                                 list(TT  = dataset_config$T,
                                                      L_1 = dataset_config$L_1,
                                                      w   = dataset_config$L_1))
    
    if (method_config[[1]] == "seaspaa") {
      builder[["seaspaa"]] <- idxrepr::mgr_init("seaspaa")
      builder[["seaspaa"]]$seas <- builder[["seas"]]
      builder[["seaspaa"]]$paa <- builder[["paa"]]
    } else if (method_config[[1]] == "seassaxres" ||
        method_config[[1]] == "seassaxpaa") {
      builder[["seassax"]] <- idxrepr::mgr_init("seassax")
      builder[["seassax"]]$seas <- builder[["seas"]]
      builder[["seassax"]]$sax <- idxrepr::mgr_set_config(
        idxrepr::mgr_init("sax"),
        list(a = floor(method_config$a_seas)))
      
      if (method_config[[1]] == "seassaxpaa") {
        builder[["seassaxpaa"]] <- idxrepr::mgr_init("seassaxpaa")
        builder[["seassaxpaa"]]$seas <- builder[["seas"]]
        builder[["seassaxpaa"]]$seassax <- builder[["seassax"]]
        builder[["seassaxpaa"]]$paa <- builder[["paa"]]
      } else if (method_config[[1]] == "seassaxres") {
        builder[["seassaxres"]] <- idxrepr::mgr_init("seassaxres")
        builder[["seassaxres"]]$seas <- builder[["seas"]]
        builder[["seassaxres"]]$seassax <- builder[["seassax"]]
        builder[["seassaxres"]]$sax <- builder[["sax"]]
        # Needed for lut
        builder[["seassaxres"]] <- idxrepr::mgr_set_config(builder[["seassaxres"]], list())
      }
    }
  }
  
  if (method_config[[1]] == "1d_sax") {
    builder[["1d_paa"]] <- idxrepr::mgr_init("1d_paa")
    builder[["1d_paa"]]$lr <- builder[["lr"]]
    builder[["1d_paa"]]$paa <- builder[["paa"]]
    
    builder[["1d_sax"]] <- idxrepr::mgr_init("1d_sax")
    builder[["1d_sax"]]$`1d_paa` <- builder[["1d_paa"]]
    builder[["1d_sax"]]$sax_a <- builder[["sax"]]
    builder[["1d_sax"]]$sax_s <- builder[["sax_s"]]
  }
  
  
  return(builder[[method_config[[1]]]])
}

method_derive <- function(dataset_config, method_config_na) {
  method_config <- method_config_na
  
  if (method_config_na[[1]] == "lrres" ||
      method_config_na[[1]] == "lrrsaxres" || 
      method_config_na[[1]] == "lrrressax") {
    
    if (method_config$`dist-type` == "norm" && is.na(method_config$sd)) {
      if ("trend-strength" %in% names(dataset_config)) {
        ts <- (dataset_config$`trend-strength`) / 100
      } else {
        ts <- dataset_trend_strength(dataset_config)
      }
      method_config$sd <- sqrt(1 - ts)  
    }
  } else if (method_config[[1]] == "paa" ||
             method_config[[1]] == "sax" ||
             method_config[[1]] == "seaspaa" ||
             method_config[[1]] == "seassaxpaa" ||
             method_config[[1]] == "seassaxres") {
    
    if (is.na(method_config$w)) {
      method_config$w <- dataset_config$T / dataset_config$L_1
    }

  }
  
  if (method_config[[1]] == "seassaxres") {
    if (method_config$`dist-type` == "norm" && is.na(method_config$sd)) {
      if ("season-strength" %in% dataset_config) {
        ss <- (dataset_config$`season-strength`) / 100
      } else {
        ss <- dataset_season_strength(dataset_config)
      }
      method_config$sd <- sqrt(1 - ss)
    }
    
    if (is.na(method_config$a)) {
      w <- method_config$w
      a_seas <- method_config$a_seas
      w_seas <- dataset_config$L_1
      method_config$a <- floor(2**((log2(256) * w - log2(a_seas) * w_seas) / w))
    }
  }
  
  return(method_config)
}

method_correct_sd <- function(sd, acf1, frac) {
  file_name <- "res_symbols_sd-paa-w-acf1"
  file_path <- util_get_top_filepath(file_name, "rw-acf1", "rds")
  if (!file.exists(file_path)) stop("Evaluate ACF1 res symbols sd first")
  
  df <- readRDS(file_path)
  
  frac_min <- df$w[which.min(abs(df$w - frac))]
  acf1_min <- df$acf1[which.min(abs(df$acf1 - acf1))]

  sd_new <- df[with(df, w == frac_min & acf1 == acf1_min)]$Sd
  
  return(sd * sd_new)
}

method_res <- function(dataset_config, method_config) {
  if (method_config[[1]] == "lrrsaxres") {
    mc_paa <- list("lrrpaa", w = method_config$w)
  } else if (method_config[[1]] == "seassaxres") {
    mc_paa <- list("seaspaa", w = method_config$w)
  } else {
    stop("method_res: N/A")
  }
  
  method <- intermediate_read(dataset_config, mc_paa, "method")
  repr <- intermediate_read(dataset_config, mc_paa, "represent")
  res_symbols <- unlist(lapply(repr, mgr_res_symbols, method = method))
  
  return(res_symbols)
}