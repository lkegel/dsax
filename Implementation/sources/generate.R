random_walk <- function(TT, sd = 1) {
  series <- rep(0, TT)
  if (sd == 0) sd <- 0.01
  rn <- rnorm(TT, sd = sd)
  for (i in seq(2, TT)) {
    series[i] <- series[i - 1] + rn[i]
  }

  return(series)
}

create_trend <- function(TT) {
  theta_1 <- runif(1, -1, 1)
  theta_2 <- rnorm(1) / TT
  trend <- theta_2 * seq(0, TT - 1) + theta_1
  return(trend)
}

create_season_mask <- function(L_1) {
  walk <- random_walk(L_1)
  
  return(walk - mean(walk))
}

create_season <- function(TT, L_1) {
  season_mask <- create_season_mask(L_1)
  rep(season_mask, TT / L_1)
}

create_residuals <- function(TT, rs) {
  return(random_walk(TT, rs))
}

create_ar <- function(TT, acf1) {
  return(arima.sim(list(ar = acf1), TT))
}

create_series <- function(TT, L_1, ts, ss, acf1) {
  if (!is.na(acf1)) {
    series <- create_ar(TT, acf1)
  } else {
    if (!is.na(ts)) {
      rs <- max(0.000001, min(0.999999, 1 - ts))
      series <- create_residuals(TT, 1 - ts)
      trend <- create_trend(TT)
      series <- series + trend
    } else if(!is.na(ss)) {
      rs <- max(0.000001, min(0.999999, 1 - ss))
      series <- create_residuals(TT, 1 - ss)
      season <- create_season(TT, L_1)
      series <- series + season
    } else {
      stop("Config not available")
    }
  }

  series <- (series - mean(series)) / sd(series)
  return(series)
}

trend <- function(series, TT) {
  fit <- lm(as.numeric(series) ~ seq(0, TT - 1))
  theta_2 <- as.numeric(coefficients(fit)[2])
  theta_1 <- as.numeric(coefficients(fit)[1])
  
  trend <- theta_2 * seq(0, TT -  1) + theta_1
  
  return(trend)
}

detrend <- function(series, TT) {
  trend <- trend(series, TT)
  res <- series - trend
  
  return(res)
}

trend_strength <- function(series, TT) {
  res <- detrend(series, TT)

  return(1 - (var(res) / var(series)))
}

dataset_trend_strength <- function(dataset_config, plot = F) {
  dataset <- util_read(dataset_config, "dataset")
  TT <- dataset_config$T
  ts <- unlist(lapply(dataset, function(x) trend_strength(x$ts, TT)))
  
  # DEBUG
  if (plot) hist(ts)
  
  mean(ts)
}

season_mask <- function(series, TT, L_1) {
  m <- matrix(series, nrow = TT / L_1, ncol = L_1, byrow = T)
  apply(m, 2, mean)
}

deseason <- function(series, TT, L_1) {
  sm <- season_mask(series, TT, L_1)
  season <- rep(sm, TT / L_1)
  res <- series - season
  
  return(res)
}

season_strength <- function(series, TT, L_1) {
  res <- deseason(series, TT, L_1)
  
  return(1 - (var(res) / var(series)))
}

dataset_season_strength <- function(dataset_config) {
  dataset <- util_read(dataset_config, "dataset")
  TT <- dataset_config$T
  L_1 <- dataset_config$L_1
  
  mean(unlist(lapply(dataset, function(x) season_strength(x$ts, TT, L_1))))
}

acf1 <- function(x) {
 return(acf(x, plot = F, lag.max = 2)$acf[,,1][2])
}

dataset_acf1 <- function(dataset_config, component) {
  dataset <- util_read(dataset_config, "dataset")
  TT <- dataset_config$T
  
  if (component == "season") {
    L_1 <- dataset_config$L_1
  }
  
  mean(unlist(lapply(dataset, function(x) {
    if (component == "trend") {
      res <- detrend(x$ts, TT)
    } else if (component == "season") {
      res <- deseason(x$ts, TT, L_1)
    } else stop("N/A")
    
    acf1(res)})))
}

generate_random_walk <- function(dataset_configs, force = F) {
  for (dataset_config in dataset_configs) {
    if (!force && util_exists(dataset_config, "dataset")) {
      next
    }
    dataset <- list()
    I <- dataset_config$I
    TT <- dataset_config$T
    ts <- NA
    if ("trend-strength" %in% names(dataset_config)) {
      strength <- ts <- dataset_config$`trend-strength` / 100
    }
    ss <- NA
    if ("season-strength" %in% names(dataset_config)) {
      L_1 <- dataset_config$L_1
      strength <- ss <- dataset_config$`season-strength` / 100
    }
    acf1 <- NA
    if ("acf1" %in% names(dataset_config)) {
      strength <- acf1 <- dataset_config$acf1 / 100
    }
    if ("interval" %in% names(dataset_config)) {
      interval <- dataset_config$interval / 100
    } else {
      interval <- 0.005
    }
    
    idx <- 1
    cnt.learn <- 0
    max.learn <- 50
    while(length(dataset) != I) {
      x <- create_series(TT, L_1, ts, ss, acf1)
      if (!is.na(ts))   { curr <- trend_strength(x, TT) }
      if (!is.na(ss))   { curr <- season_strength(x, TT, L_1) }
      if (!is.na(acf1)) { curr <- acf1(x) }
      
      if (curr >= strength - interval && curr <= strength + interval) {
        dataset[[idx]] <- list(name = paste0("N", idx), ts = x)
        print(idx)
        idx <- idx + 1
        cnt.learn <- 0
      } else if (cnt.learn >= max.learn) {
        if (curr < strength - 0.01) {
          if (!is.na(ss))  {
            ss <- ss * 1.1
            if (ss >= 1) {
              ss <- ss / 1.1
              ss <- ss + (1 - ss) / 2
            }
          }
          if (!is.na(ts)) {
            ts <- ts * 1.1 
            if (ts >= 1) {
              ts <- ts / 1.1
              ts <- ts + (1 - ts) / 2
            }
          }
        } else if (curr > strength) {
          if (!is.na(ss) > 0) ss <- ss * 0.9
          if (!is.na(ts) > 0) ts <- ts * 0.9
        }
        print(paste("Adapt strength", ss, ts))
        cnt.learn <- 0
      } else {
        cnt.learn <- cnt.learn + 1
      }
    }
    names(dataset) <- unlist(lapply(dataset, function(x) x$name))

    util_save(dataset_config, "dataset", dataset)
  }
}

check_generate_random_walk <- function(dataset_configs) {
  for (dataset_config in dataset_configs) {
    I <- dataset_config$I
    TT <- dataset_config$T
    
    ts <- NA
    if ("trend-strength" %in% names(dataset_config)) {
      strength <- ts <- dataset_config$`trend-strength` / 100
    }
    ss <- NA
    if ("season-strength" %in% names(dataset_config)) {
      L_1 <- dataset_config$L_1
      strength <- ss <- dataset_config$`season-strength` / 100
    }
    
    if ("interval" %in% names(dataset_config)) {
      interval <- dataset_config$interval / 100
    } else {
      interval <- 0.005
    }
    
    dataset <- util_read(dataset_config, "dataset")
    for (x in dataset) {
      stopifnot(round(mean(x$ts), 6) == 0)
      stopifnot(round(sd(x$ts), 6) == 1)
      if (!is.na(ts)) {
        curr <- trend_strength(x$ts, TT)  
        stopifnot(ts - interval <= curr && curr < ts + interval)
      } else if (!is.na(ss)) {
        curr <- season_strength(x$ts, TT, L_1)  
        stopifnot(ss - interval <= curr && curr < ss + interval)
      }
    }
  }
}
