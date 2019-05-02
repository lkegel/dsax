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

run_represent <- function(dataset_configs, method_configs, force = T) {
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
        
        dataset <- util_read(dataset_config, "dataset")
        repr <- represent_run(dataset, method)
        intermediate_save(dataset_config, method_config, repr, "represent")  
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
