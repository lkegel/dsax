
util_read <- function(config, file_name, subdir = NA) {
  file_path <- util_get_filepath(config, file_name, subdir)
  readRDS(file_path)
}

util_save <- function(config, file_name, data, subdir = NA) {
  path <- util_get_path(config)
  if (!is.na(subdir)) {
    path <- file.path(path, subdir)
  }
  dir.create(path, showWarnings = F, recursive = T)
  file_path <- file.path(path, paste0(file_name, ".rds"))
  saveRDS(data, file_path)
}

util_get_filepath <- function(config, file_name, subdir = NA, ext = "rds") {
  path <- util_get_path(config)
  if (!is.na(subdir)) {
    path <- file.path(path, subdir)
  }
  file_path <- file.path(path, paste0(file_name, ".", ext))

  return(file_path)
}

util_get_top_filepath <- function(file_name, subdir = NA, ext = "rds") {
  path <- util_get_top_path()
  if (!is.na(subdir)) {
    path <- file.path(path, subdir)
    dir.create(path, showWarnings = F, recursive = T)
  }
  file_path <- file.path(path, paste0(file_name, ".", ext))
  
  return(file_path)
}

util_get_path <- function(config) {
  path_name <- paste(c(config[[1]],
                       paste(names(config[-1]), config[-1], sep = "_")),
                     collapse = "_")
  path <- file.path("Data", path_name)

  return(path)
}

util_get_top_path <- function() {
  path <- file.path("Data")
  
  return(path)
}

util_exists <- function(config, file_name, subdir = NA) {
  file_path <- util_get_filepath(config, file_name, subdir)
  file.exists(file_path)
}

util_subset <- function(config, sel) {
  result <- c()
  for (i in seq_along(config)) {
    ele <- config[[i]]
    all_true <- T
    for (j in seq_along(sel)) {
      cond <- sel[[j]]
      if (names(sel)[j] != "" && !is.null(names(sel)[j])) {
        name <- names(sel)[j]
      } else {
        name <- j
      }
      if (!ele[[name]] %in% cond) {
        all_true <- F
        break
      }
      
    }
    if (all_true) {
      result <- c(result, i)
    }
  }
  
  return(result)
}

util_exclude <- function(config, sel) {
  result <- c()
  for (i in seq_along(config)) {
    ele <- config[[i]]
    all_true <- T
    for (j in seq_along(sel)) {
      cond <- sel[[j]]
      if (ele[[names(sel)[j]]] != cond) {
        all_true <- F
        break
      }
    }
    if (!all_true) {
      result <- c(result, i)
    }
  }
  
  return(result)
}

intermediate_subdirs <- c("method", "represent", "distance", "lower-bounding")

intermediate_file_name <- function(method_config) {
  file_name <- paste(c(method_config[[1]], paste(names(method_config[-1]),
                                                 method_config[-1], sep = "_")),
                     collapse = "_")
  return(file_name)
}

intermediate_save <- function(dataset_config, method_config, data, subdir) {
  file_name <- intermediate_file_name(method_config)
  util_save(dataset_config, file_name, data, subdir = subdir)
}

intermediate_read <- function(dataset_config, method_config, subdir) {
  file_name <- intermediate_file_name(method_config)
  util_read(dataset_config, file_name, subdir = subdir)
}

intermediate_exists <- function(dataset_config, method_config, subdir) {
  file_name <- intermediate_file_name(method_config)
  file_path <- util_get_filepath(dataset_config, file_name, subdir)
  
  return(file.exists(file_path))
}

intermediate_delete <- function(dataset_configs, method_configs,
                                subdir = intermediate_subdirs) {
  for (dataset_config in dataset_configs) {
    for(method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      file_name <- intermediate_file_name(method_config)
      for(sd in match.arg(subdir, intermediate_subdirs, T)) {
        file_path <- util_get_filepath(dataset_config, file_name, sd)
        file.remove(file_path)
      }   
    }
  }
}

method_config_names <- function(method_configs) {
  unlist(lapply(method_configs, function(x) x[[1]]))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data Frame -------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df1_insert_update_minmax <- function(df, dim_x, dim_x_v, val) {
  if (is.data.frame(df)) {
    i <- which(with(df, get(dim_x) == dim_x_v))
    if (length(i) == 0) {
      df <- rbindlist(list(df, list(dim_x_v, val, val)))
    } else if (length(i) == 1) {
      if (df[i, "Min"] > val)
        df[i, "Min"] <- val
      else if(df[i, "Max"] < val) {
        df[i, "Max"] <- val
      }
    } else stop("N/A")
  } else {
    df <- data.frame(Dim1 = dim_x_v, Min = val, Max = val)
    names(df)[1] <- dim_x
  }
  
  return(df)
}

df2_insert_update_max <- function(df, dim_x, dim_y, dim_x_v, dim_y_v, val_name,
                                      val) {
  if (is.data.frame(df)) {
    i <- which(with(df, get(dim_x) == dim_x_v & get(dim_y) == dim_y_v))
    if (length(i) == 0) {
      df <- rbindlist(list(df, list(dim_x_v, dim_y_v, val)))
    } else if (length(i) == 1) {
      if (df[i, 3] < val)
        df[i, 3] <- val
    } else stop("N/A")
  } else {
    df <- data.frame(Dim1 = dim_x_v, Dim2 = dim_y_v, Val = val)
    names(df) <- c(dim_x, dim_y, val_name)
  }
  
  return(df)
}

