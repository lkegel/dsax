#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Library ----------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(directlabels))
suppressPackageStartupMessages(library(latticeExtra))
suppressPackageStartupMessages(library(grDevices))

eval_loss <- function(dataset_configs, mc_na) {
  for (dataset_config in dataset_configs) {
    dataset <- util_read(dataset_config, "dataset")

    file_path <- util_get_filepath(dataset_config, "loss", NA, "pdf")
    pdf(file_path)

    loss_list <- list()
    idx_inf <- c()
    for (method_config_na in mc_na) {
      method_config <- method_derive(dataset_config, method_config_na)
      method <- intermediate_read(dataset_config, method_config, "method")
      repr <- intermediate_read(dataset_config, method_config, "represent")
      loss_curr <- sapply(seq_along(dataset), function(i) {
        mgr_loss(method, repr[[i]], dataset[[i]]$ts)
      })
      loss_list[[method_config[[1]]]] <- loss_curr
      idx_inf <- unique(c(idx_inf, which(is.infinite(loss_curr))))
    }

    loss_noninf_list <- lapply(loss_list, function(x) x[-idx_inf])

    # boxplot(loss_list)
    # boxplot(loss_noninf_list)
    myBoxplot(loss_list, names = names(loss_list), meanlabels = T)
    myBoxplot(loss_noninf_list, names = names(loss_list), meanlabels = T)
    dev.off()

    print(lapply(loss_list, median))
    print(lapply(loss_noninf_list, median))
  }
}

eval_det_symbols <- function(dataset_configs, method_configs) {
  for (dataset_config in dataset_configs) {
    file_path <- util_get_filepath(dataset_config, "det_symbols", NA, "pdf")
    pdf(file_path)
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      file_name <- intermediate_file_name(method_config)
      method <- intermediate_read(dataset_config, method_config, "method")
      repr <- intermediate_read(dataset_config, method_config, "represent")
      det_symbols <- unlist(lapply(repr, mgr_det_symbols, method = method))
      hist(det_symbols, main = file_name, sub = paste("sd =", sd(det_symbols)))
    }
    dev.off()
  }
}

eval_res_symbols <- function(dataset_configs, method_configs) {
  for (dataset_config in dataset_configs) {
    file_path <- util_get_filepath(dataset_config, "res_symbols", NA, "pdf")
    cairo_pdf(file_path, onefile = T, width = width_in_2, height = width_in_2)
    par(family = font_family)
    par(mar = c(2, 2, 0.1, 0.1))
    par(cex = 0.6)
    lwd <- 0.6
    par(lwd = lwd)
    df <- NA
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      file_name <- intermediate_file_name(method_config)
      method <- intermediate_read(dataset_config, method_config, "method")
      repr <- intermediate_read(dataset_config, method_config, "represent")
      res_symbols <- unlist(lapply(repr, mgr_res_symbols, method = method))
      freq <- unname(table(res_symbols) / length(res_symbols))
      e <- entropy(freq)
      hist(res_symbols, main = NULL, xlab = NULL)
      if (is.data.frame(df)) {
        df <- rbindlist(list(df, list(file_name, sd(res_symbols), e)))
      } else {
        df <- data.frame(Name = file_name, Sd = sd(res_symbols), Entr = e)
      }
    }
    dev.off()
    file_path <- util_get_filepath(dataset_config, "res_symbols", NA, "txt")
    write.table(df, file_path)
  }
}

entropy <- function(x) {
  stopifnot(sum(x) == 1)
  return(-sum(x * log2(x)))
}

eval_res_symbols_entropy <- function(dataset_configs, method_configs,
                                     dataset_name, name_1, name_2, x_dim,
                                     x_lab, y_lab = "Entropy H",
                                     xlim_max = NA, ylim = NA,
                                     breaks = F, eval_color,
                                     legend.position = "none",
                                     plot.margin = NA,
                                     legend.margin = NA,
                                     legend.box.margin = NA,
                                     legend.box.spacing = NA,
                                     legend.key.height = NA) {
  file_name <- paste("res_symbols_entropy", name_1, name_2, x_dim, sep = "-")
  file_path <- util_get_top_filepath(file_name, dataset_name, "pdf")

  df <- NA
  for (dataset_config in dataset_configs) {
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      name <- method_config[[1]]
      file_name <- intermediate_file_name(method_config)
      method <- intermediate_read(dataset_config, method_config, "method")
      repr <- intermediate_read(dataset_config, method_config, "represent")
      res_symbols <- unlist(lapply(repr, mgr_res_symbols, method = method))
      freq <- unname(table(res_symbols) / length(res_symbols))
      e <- entropy(freq)
      if (x_dim %in% names(dataset_config)) {
        dim <- dataset_config[[x_dim]]
      } else if (x_dim %in% names(method_config)) {
        dim <- method_config[[x_dim]]
      } else {
        stop("x_dim not found")
      }

      if (is.data.frame(df)) {
        df <- rbindlist(list(df, list(name, dim, e)))
      } else {
        df <- data.frame(Name = name, Dim = dim, Entropy = e)
        names(df)[2] <- x_dim
      }
    }
  }
  levels(df$Name)[levels(df$Name)=="sax"] <- "SAX"
  levels(df$Name)[levels(df$Name)=="lrrsaxres"] <- "tSAX"
  levels(df$Name)[levels(df$Name)=="seassaxres"] <- "sSAX"
  scale_color <- scale_color_manual(values = eval_color)

  if (all(legend.position != "none")) {
    pdf(file_path, width = width_in_2, height = height_in_2 + 0.11811, family = font_family)
  } else {
    pdf(file_path, width = width_in_2, height = height_in_2, family = font_family)
  }
  p <- ggplot(data=df, aes(x = get(x_dim), y = Entropy, group = Name, colour = Name)) +
    geom_line(size = 0.3) + geom_point(shape = 4) + scale_color + eval_theme + xlab(x_lab) + ylab(y_lab)
  if (breaks)
    p <- p + scale_x_continuous(breaks = sort(unique(df[[x_dim]])), lim = c(NA, xlim_max),
                                labels = function(x) format(x, big.mark = ",", scientific = FALSE))
  else
    p <- p + xlim(NA, xlim_max)
  
  if (all(!is.na(ylim))) {
    p <- p + ylim(ylim)
  }
  # top, right, bottom, left
  if (all(legend.position != "none")) {
    p <- p + theme(legend.position = legend.position,
                   plot.margin = plot.margin,
                   legend.margin = legend.margin,
                   legend.box.margin = legend.box.margin,
                   legend.box.spacing = legend.box.spacing,
                   legend.key.height = legend.key.height)

  }

  print(p)
  dev.off()
}

eval_res_symbols_sd <- function(dataset_configs, method_configs,
                                dataset_name, name, x_dim, y_dim, x_lab,
                                y_lab) {
  df <- NA
  for (dataset_config in dataset_configs) {
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      name <- method_config[[1]]
      file_name <- intermediate_file_name(method_config)
      method <- intermediate_read(dataset_config, method_config, "method")
      repr <- intermediate_read(dataset_config, method_config, "represent")
      res_symbols <- unlist(lapply(repr, mgr_res_symbols, method = method))
      x_dim_v <- eval_dim_value(dataset_config, method_config, x_dim)
      y_dim_v <- eval_dim_value(dataset_config, method_config, y_dim)
      s <- sd(res_symbols)
      if (is.data.frame(df)) {
        df <- rbindlist(list(df, list(name, x_dim_v, y_dim_v, s)))
      } else {
        df <- data.frame(Name = name, Dim = x_dim_v, Dim2 = y_dim_v, Sd = s)
        names(df)[2:3] <- c(x_dim, y_dim)
      }
    }
  }

  df[[x_dim]] <- 1 / df[[x_dim]]

  file_name <- paste("res_symbols_sd", name, x_dim, y_dim, sep = "-")
  file_path <- util_get_top_filepath(file_name, dataset_name, "rds")
  saveRDS(object = df, file = file_path)

  df$acf1 <- factor(df$acf1)


  file_name <- paste("res_symbols_sd", name, x_dim, y_dim, sep = "-")
  file_path <- util_get_top_filepath(file_name, dataset_name, "pdf")

  cairo_pdf(file_path, width = 2*width_in, height = 2*width_in)
  p <- ggplot(data = df, aes(x = get(x_dim), y = Sd, group = get(y_dim),
                        label = get(y_dim), color = get(y_dim))) +
    geom_line() + xlab(paste0("1 / ", x_lab)) + ylab(y_lab) + labs(color = y_lab)
  print(p)
  dev.off()
}

eval_lower_bounding <- function(dataset_configs, method_configs, avg = mean) {
  for (dataset_config in dataset_configs) {
    df <- NA
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      lb <- intermediate_read(dataset_config, method_config, "lower-bounding")
      if (any(lb > 1)) {
        print(paste("Config not lb:", intermediate_file_name(method_config),
                    "Highest lb:", max(lb),
                    "At: ",
                    paste(which(lb == max(lb), arr.ind = T), collapse = ",")))
      }
      if ("a" %in% names(method_config)) {
        a_char <- as.character(method_config$a)
      } else {
        a_char <- -1
      }
      w_char <- as.character(method_config$w)
      if (is.data.frame(df)) {
        df <- rbindlist(list(df, list(Name = method_config[[1]],
                                      a = a_char,
                                      w = w_char,
                                      TLB = avg(lb))))
      } else {
        df <- data.frame(Name = method_config[[1]],
                         a = a_char,
                         w = w_char,
                         TLB = avg(lb))
      }
    }

    print(util_get_path(dataset_config))
    print(df)
    file_path <- util_get_filepath(dataset_config, "lower_bounding", NA, "pdf")
    pdf(file_path)
    print(cloud(TLB~a+w, df, panel.3d.cloud=panel.3dbars, col.facet='grey',
          xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1),
          par.settings = list(axis.line = list(col = "transparent")))
    )
    dev.off()
  }
}

eval_lower_bounding_boxplot <- function(dataset_configs, method_configs,
                                        ext = "pdf") {
  for (dataset_config in dataset_configs) {
    w_groups <- list()
    file_path <- util_get_filepath(dataset_config, "lower_bounding_boxplot", NA, ext)
    if (ext == "pdf") {
      pdf(file_path)
    } else {
      png(file_path)
    }
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      lb <- intermediate_read(dataset_config, method_config, "lower-bounding")

      w <- as.character(method_config$w)
      a <- as.character(method_config$a)
      name <- method_config[[1]] #intermediate_file_name(method_config)
      if (!w %in% names(w_groups)) {
        w_groups[[w]] <- list()
      }
      w_groups[[w]][[name]] <- lb
    }

    for (i in seq_along(w_groups)) {
      myBoxplot(w_groups[[i]], main = paste0("w=", names(w_groups)[i]),
                names = names(w_groups[[i]]), meanlabels = T)
    }
    dev.off()
  }
}

eval_minmax <- function(dataset_configs, method_configs,
                                       name_1, name_2, ylab,
                                       name_val, fct_val, fct_arg, digit = 0,
                                       diff = T, x_off = 0, y_off = 0,
                                       eval_color = NA,
                                       name_3 = NULL,
                                       ylim, ybreaks) {
  df <- NA
  for (dataset_config in dataset_configs) {
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      interm <- intermediate_read(dataset_config, method_config, name_val)
      if (fct_arg) {
        val <- fct_val(interm, dataset_config, method_config)
      } else {
        val <- fct_val(interm)
      }
      val <- round(100 * val, digit)
      df <- df1_insert_update_minmax(df, "Method", method_config[[1]], val)
    }
    scale_color <- scale_color_manual(values = eval_color)
    df$Method <- factor(c(name_1, name_2, name_3), levels = c(name_1, name_3, name_2))
    fp <- util_get_filepath(dataset_config, paste0(name_val, "_minmax"), NA, "pdf")
    pdf(fp, width = width_in_2, height = height_in_2, family = font_family)
    p <- ggplot()
    p <- p + geom_errorbar(data = df, aes(x = Method, ymin = Min, ymax = Max, color = Method),
                           width = 0.5)
    p <- p + scale_fill_gradient2(space ="Lab")
    p <- p + xlab(NULL) + ylab(ylab)
    p <- p + eval_theme + theme(legend.position="none")
    p <- p + scale_color
    p <- p + scale_y_continuous(breaks = ybreaks, lim = ylim)
                       
    if (isTRUE(diff) && df[1, "Max"] < df[2, "Max"]) {
      y <- c(as.numeric(df[1, "Max"]), as.numeric(df[2, "Max"]))
      y_text <- min(y) + abs(diff(y)) * 0.5 + y_off
      label_text <- round(as.numeric(df[2, "Max"]) - as.numeric(df[1, "Max"]), digit)
      df_line <- data.frame(X = c(1.5, 1.5), Y = y)
      df_text <- data.frame(X = 1.5 + x_off, Y = y_text, Lab = label_text)
      arrow <- arrow(15, unit(0.05, "inches"), "both", "closed")
      p <- p + geom_line(data = df_line, mapping = aes(X, Y), arrow = arrow)
      p <- p + geom_label(data = df_text, aes(X, Y, label = paste(Lab, "pp")), size = theme_size)
    }
    print(p)
    dev.off()
  }
}

eval_heatmap <- function(dataset_configs, method_configs,
                                        dataset_name, name_1, name_2, dim_x,
                                        dim_y, lab_x, lab_y, name_val, fct_val, fct_arg = T, ma_2 = NA,
                                        limits) {
  file_name <- paste(name_val, "heatmap", name_1, name_2, dim_x, dim_y, sep = "-")
  file_path <- util_get_top_filepath(file_name, dataset_name, "pdf")
  df_1 <- NA
  df_2 <- NA
  for (dataset_config in dataset_configs) {
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      val <- intermediate_read(dataset_config, method_config, name_val)
      if (fct_arg) {
        val <- fct_val(val, dataset_config, method_config)
      } else {
        val <- fct_val(val)
      }

      dim_x_v <- eval_dim_value(dataset_config, method_config, dim_x)
      dim_y_v <- eval_dim_value(dataset_config, method_config, dim_y)

      if (method_config[[1]] == name_1) {
        df_1 <- df2_insert_update_max(df_1, dim_x, dim_y, dim_x_v, dim_y_v,
                                      name_val, val)
      } else if (method_config[[1]] == name_2) {
        df_2 <- df2_insert_update_max(df_2, dim_x, dim_y, dim_x_v, dim_y_v,
                                      name_val, val)
      }
    }
  }

  df <- df_1
  df[, 3] <- df_2[, 3] - df[, 3]
  if (is.na(ma_2)) ma_2 <- max(df[, 3]) / 2
  pdf(file_path, width = width_in_2, height = height_in_2_heat, family = font_family)
  p <- ggplot(data = df, aes(x = get(dim_x), y = get(dim_y), fill = get(name_val)))
  p <- p + geom_tile(aes(height = ifelse(get(dim_y) %in% c(1, 99), 17, 19)))
                     #vjust = get(dim_y))
  p <- p + scale_fill_gradient2(space ="Lab", low = (colour = "#d7191c"), high = (colour = "#2b83ba"), limits = limits)
  p <- p + geom_text(aes(label=round(get(name_val) * 100, 2), color = get(name_val) > ma_2), na.rm = T, family = font_family, size = 2)
  p <- p + xlab(lab_x) + ylab(lab_y)
  p <- p + scale_x_continuous(breaks = unique(df[[dim_x]]),
                              labels = function(x) format(x, big.mark = ",", scientific = FALSE))
  # p <- p + facet_grid(.~factor(ceiling(get(dim_x)/2000)), scales = "free_x", space = "free_x")
  p <- p + scale_y_continuous(breaks = unique(df[[dim_y]]))
  p <- p + eval_theme + theme(legend.position="none")
  p <- p + scale_color_manual(guide = FALSE, values = c("black", "white"))
  print(p)
  dev.off()

  print(df)
}

eval_dim_value <- function(dataset_config, method_config, dim_name) {
  if (dim_name %in% names(dataset_config)) {
    dim_value <- dataset_config[[dim_name]]
  } else if (dim_name %in% names(method_config)) {
    dim_value <- method_config[[dim_name]]
  } else {
    stop("dim_name not found")
  }

  return(dim_value)
}

eval_exact_search <- function(dataset_configs, method_configs) {
  for (dataset_config in dataset_configs) {
    w_groups <- list()
    file_path <- util_get_filepath(dataset_config, "exact_search", NA, "pdf")
    pdf(file_path)
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      es <- intermediate_read(dataset_config, method_config, "exact-search")
      n_raw_c <- unlist(lapply(es, function(x) x$n_raw))

      w <- as.character(method_config$w)
      a <- as.character(method_config$a)
      name <- paste0("a_", a) #method_config[[1]] #intermediate_file_name(method_config) #  #
      if (!w %in% names(w_groups)) {
        w_groups[[w]] <- list()
      }
      w_groups[[w]][[name]] <- n_raw_c
    }

    for (i in seq_along(w_groups)) {
      myBoxplot(w_groups[[i]], main = paste0("w=", names(w_groups)[i]),
                names = names(w_groups[[i]]), meanlabels = T)
    }
    dev.off()
  }
}

eval_exact_search_runtime <- function(dataset_configs, method_configs, csv = F) {
  for (dataset_config in dataset_configs) {
    for (method_config_na in method_configs) {
      method_config <- method_derive(dataset_config, method_config_na)
      if (csv) {
        file_name <- intermediate_file_name(method_config)
        fp <- util_get_filepath(dataset_config, file_name = file_name, "exact-search-runtime", "csv")
        dt <- fread(fp)
        es <- list()
        for (i in seq(nrow(dt))) {
          es[[i]] <- as.list(dt[i, ])
        }
        elapsed <- 0
      } else {
        es <- intermediate_read(dataset_config, method_config, "exact-search-runtime")
        elapsed <- es$elapsed
        es <- es[[1]]
      }

      print(elapsed)
      el_repr <- unlist(lapply(es, function(x) x$el_repr / 1000))
      boxplot(el_repr)
      el_raw <- unlist(lapply(es, function(x) x$el_raw / 1000))
      boxplot(el_raw)
      print(paste("Repr:", sum(el_repr)))
      print(paste("ED:", sum(el_raw)))

      n_repr <- unlist(lapply(es, function(x) x$n_repr))
      print(paste("Repr per op:", sum(el_repr) / sum(n_repr)))

      n_raw <- unlist(lapply(es, function(x) x$n_raw))
      print(paste("ED per op:", sum(el_raw) / sum(n_raw)))
    }
  }
}

pruning_power <- function(exact_search,
                          dataset_config,
                          method_config) {
  n_raw_c <- unlist(lapply(exact_search, function(x) x$n_raw))
  mean((dataset_config$I - 1 - n_raw_c) / (dataset_config$I - 1))
}

approximate_accuracy <- function(approximate_search,
                                 dataset_config,
                                 method_config) {
  n <- length(approximate_search)
  exact_search <- intermediate_read(dataset_config, method_config, "exact-search")
  results <- rep(0, n)
  for (i in seq(n)) {
    raw.approximate <- approximate_search[[i]]$raw
    raw.exact <- exact_search[[i]]$raw
    results[i] <- raw.exact / raw.approximate
  }

  mean(results)
}


myBoxplot <- function(x,...,names=NULL, meanlabels=FALSE, ylab=NULL, ylim=NULL, las=NULL, zoom=TRUE, main=''){

  if(is.list(x))
    args <- c(x,...)
  else
    args <- list(x, ...)

  names <- if(!is.null(names)) names else 1L:length(args)

  means <- sapply(args, mean, na.rm=TRUE)

  if(is.null(ylim) & zoom) {
    bp <- boxplot(args, plot = F)
    ylim <- c(min(bp$stats[1, ]) * 0.95, max(bp$stats[5, ]) * 1.05)
  }

  boxplot(args, names = names, ylab=ylab, ylim=ylim, las=las, medlwd=2, main=main)

  lines(x = 1L:length(args), y = means, col = "red", pch = 4, type = 'p', cex=1.5, lwd=2)

  if(meanlabels)
    text(x=1L:length(args), y=means, labels=round(means,3), pos=3, srt=0)

  # text(seq_len(length(names)), rep(ylim[1] - 0.5, length(names)), srt = 90, adj = 1,
  #      labels = names, xpd = TRUE, cex=0.5, srt=45)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Format -----------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
width_in_2 <- 1.69
height_in_2 <- 1.04
height_in_2_heat <- 1.50
width_in <- 3.48
height_in <- 2.15
text_size <- 8
theme_size <- 5/14 * text_size
font_family <- "CMU" #"CMU Serif"
eval_color <- c("#2b83ba", "#d7191c", "#fc8003", "#429537", "#606060")
eval_scale_color <- scale_color_manual(values = eval_color)

fpr <- file.path(Sys.getenv("DSAA2019"), "Implementation", "fonts", "ptmr8a.afm")
fpb <- file.path(Sys.getenv("DSAA2019"), "Implementation", "fonts", "ptmb8a.afm")
fpri <- file.path(Sys.getenv("DSAA2019"), "Implementation", "fonts", "ptmri8a.afm")
fpbi <- file.path(Sys.getenv("DSAA2019"), "Implementation", "fonts", "ptmbi8a.afm")

CMU <- Type1Font("CMU", c(c(fpr, fpb, fpri, fpbi), "CM_symbol_10.afm"),
                 encoding = "WinAnsi.enc")
if (!("CMU" %in% names(pdfFonts())))
  pdfFonts(CMU=CMU)

# serif_file <- file.path("Implementation/fonts/cmunrm.ttf")
# myfonts <- fonts <- list(serif = list(plain = serif_file))
# embedFonts(serif_file, format, outfile = file,
#            fontpaths = character(), options = character())


eval_theme <- theme(
  line = element_line(size = 0.3),
  text = element_text(family=font_family, size=text_size),
  title = element_text(family=font_family, size=text_size),
  legend.title  = element_blank(), #element_text(size=text_size, family=font_family),
  legend.text   = element_text(size=text_size, family=font_family),
  legend.position = "none", #c(1, 1),
  legend.justification = c(0, 0),
  legend.box.margin = margin(0, 0, 0, 0, "mm"),
  legend.box.spacing = unit(c(-1, -1, -1, -1), "mm"),
  legend.background = element_rect(color = "white", fill= "white"),
  legend.box.background = element_rect(color = "white", fill= NA),
  legend.key = element_rect(color = "white", fill= "white"),
  legend.key.height = unit(1, "mm"),
  legend.direction = "horizontal",
  axis.ticks = element_line(colour = "black", size = 0.2),
  axis.text.y   = element_text(size=text_size, color = "black", family=font_family),
  axis.text.x   = element_text(size=text_size, color = "black", family=font_family),
  axis.title.y  = element_text(size=text_size, family=font_family, margin = unit(c(0, 1, 0, 0), "mm")),
  axis.title.x  = element_text(size=text_size, family=font_family),
  panel.background = element_blank(),
  panel.grid.major = element_line(colour = "#AFAFAF", size = 0.1),
  #panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black", size = 0.0),
  panel.border = element_rect(color = "black", fill=NA, size=0.3),
  # top, right, bottom, left
  plot.margin = unit(c(1, 2, 1, 1.7), "mm")
)
