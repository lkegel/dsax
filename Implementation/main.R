#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Library ----------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(distr))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(R.utils))
suppressPackageStartupMessages(library(idxrepr))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Directory --------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
root <- file.path(Sys.getenv("EDBT2020"))
setwd(root)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sources ----------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sourceDirectory(file.path("Implementation", "sources"), modifiedOnly = F)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Dataset and Method Configurations --------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sourceDirectory(file.path("Implementation", "configs"), modifiedOnly = F)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++-------------------------------------------------------
# Generation -------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + RW -------------------------------------------------------------------------
# ++ Trend ---------------------------------------------------------------------
generate_random_walk(dc_rw_trend, force = F)
check_generate_random_walk(dc_rw_trend)
# ++ Season --------------------------------------------------------------------
generate_random_walk(dc_rw_season, force = F)
check_generate_random_walk(dc_rw_season)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++-------------------------------------------------------
# Euclidean Distance -----------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + RW -------------------------------------------------------------------------
# ++ Season --------------------------------------------------------------------
euclidean_distance(dc_rw_season, force = F)
# ++ Trend ---------------------------------------------------------------------
euclidean_distance(dc_rw_trend, force = F)
# + Real -----------------------------------------------------------------------
# ++ Season --------------------------------------------------------------------
euclidean_distance(dc_real_season, force = F)
# ++ Trend ---------------------------------------------------------------------
euclidean_distance(dc_real_trend, force = F)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++-------------------------------------------------------
# Accuracy Experiments ---------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + RW -------------------------------------------------------------------------
# ++ Season --------------------------------------------------------------------
# +++ Symbolic Distribution ----------------------------------------------------
run_represent(dc_rw_season, mc_rw_season_a_fix, force = F)
idx <- util_subset(dc_rw_season, list("T" = 960, "season-strength" = 60))
eval_res_symbols_entropy(dc_rw_season[idx],
                         mc_rw_season_a_fix,
                         "rw-season",
                         name_1 = "sax",
                         name_2 = "seassaxres",
                         x_dim = "w",
                         x_lab = "#PAA Segments W",
                         xlim_max = 100.005, ylim = c(7.595, 8.005),
                         eval_color = eval_color[c(5, 1)])

idx_1 <- util_subset(dc_rw_season, list("T" = 960))
idx_2 <- util_subset(mc_rw_season_a_fix, list("w" = 48))
eval_res_symbols_entropy(dc_rw_season[idx_1],
                         mc_rw_season_a_fix[idx_2],
                         "rw-season",
                         name_1 = "sax",
                         name_2 = "seassaxres",
                         x_dim = "season-strength",
                         x_lab = expression(paste("Season Strength R"[seas]^"2"*" (%)")),
                         xlim_max = 100.005, ylim = c(4.995, 8.005), breaks = T,
                         eval_color = eval_color[c(5, 1)])

idx_1 <- util_subset(dc_rw_season, list("season-strength" = 60))
idx_2 <- util_subset(mc_rw_season_a_fix, list("w" = 48))
eval_res_symbols_entropy(dc_rw_season[idx_1],
                         mc_rw_season_a_fix[idx_2],
                         "rw-season",
                         name_1 = "sax",
                         name_2 = "seassaxres",
                         x_dim = "T",
                         x_lab = "Time Series Length T",
                         xlim_max = 1920.005, ylim = c(7.595, 8.005), breaks = T,
                         eval_color = eval_color[c(5, 1)],
                         legend.position = c(0.1, 1.07),
                         plot.margin = unit(c(4, 2, 1, 1.7), "mm"),
                         legend.margin = margin(0, 0, 0, 0, "mm"),
                         legend.box.margin = margin(-1, 0, 0, 0, "mm"),
                         legend.box.spacing = unit(0, "mm"),
                         legend.key.height = unit(1, "mm"))

# +++ Representation Accuracy --------------------------------------------------
run_lower_bounding(dc_rw_season, mc_rw_season, force = F)
eval_heatmap(dc_rw_season,
             mc_rw_season,
             "rw-season",
             "sax", "seassaxres",
             "T", "season-strength",
             "Time Series Length T",
             expression(paste("Season Strength R"[seas]^"2"*" (%)")),
             "lower-bounding", mean, F, ma_2 = 1,
             limits = c(-1, 1))

# +++ Exact Matching -----------------------------------------------------------
run_exact_search(dc_rw_season, mc_rw_season, force = F)
eval_heatmap(dc_rw_season,
             mc_rw_season,
             "rw-season",
             "sax", "seassaxres",
             "T", "season-strength",
             "Time Series Length T",
             expression(paste("Season Strength R"[seas]^"2"*" (%)")),
             "exact-search", pruning_power, fct_arg = T, ma_2 = 1,
             limits = c(-1, 1))

# +++ Approximate Matching -----------------------------------------------------
run_approximate_search(dc_rw_season, mc_rw_season, force = F)
eval_heatmap(dc_rw_season,
             mc_rw_season,
             "rw-season",
             "sax", "seassaxres",
             "T", "season-strength",
             "Time Series Length T",
             expression(paste("Season Strength R"[seas]^"2"*" (%)")),
             "approximate-search", approximate_accuracy, fct_arg = T, ma_2 = 1,
             limits = c(-1, 1))

# ++ Trend ---------------------------------------------------------------------
# +++ Symbolic Distribution ----------------------------------------------------
run_represent(dc_rw_trend, mc_rw_trend_a_fix, force = F)
idx <- util_subset(dc_rw_trend, list("T" = 960, "trend-strength" = 60))
eval_res_symbols_entropy(dc_rw_trend[idx],
                         mc_rw_trend_a_fix,
                         "rw-trend",
                         name_1 = "sax",
                         name_2 = "lrrsaxres",
                         x_dim = "w",
                         x_lab = "#PAA Segments W",
                         xlim_max = 100.005, ylim = c(7.595, 8.005),
                         eval_color = eval_color[c(5, 2)])

idx_1 <- util_subset(dc_rw_trend, list("T" = 960))
idx_2 <- util_subset(mc_rw_trend_a_fix, list("w" = 48))
eval_res_symbols_entropy(dc_rw_trend[idx_1],
                         mc_rw_trend_a_fix[idx_2],
                         "rw-trend",
                         name_1 = "sax",
                         name_2 = "lrrsaxres",
                         x_dim = "trend-strength",
                         x_lab = expression(paste("Trend Strength R"[tr]^"2"*" (%)")),
                         xlim_max = 100.005, ylim = c(4.995, 8.005), breaks = T,
                         eval_color = eval_color[c(5, 2)])

idx_1 <- util_subset(dc_rw_trend, list("trend-strength" = 60))
idx_2 <- util_subset(mc_rw_trend_a_fix, list("w" = 48))
eval_res_symbols_entropy(dc_rw_trend[idx_1],
                         mc_rw_trend_a_fix[idx_2],
                         "rw-trend",
                         name_1 = "sax",
                         name_2 = "lrrsaxres",
                         x_dim = "T",
                         x_lab = "Time Series Length T",
                         xlim_max = 1920.005, ylim = c(7.595, 8.005), breaks = T,
                         eval_color = eval_color[c(5, 2)],
                         legend.position = c(0.1, 1.07),
                         plot.margin = unit(c(4, 2, 1, 1.7), "mm"),
                         legend.margin = margin(0, 0, 0, 0, "mm"),
                         legend.box.margin = margin(-1, 0, 0, 0, "mm"),
                         legend.box.spacing = unit(0, "mm"),
                         legend.key.height = unit(1, "mm"))

# +++ Representation Accuracy --------------------------------------------------
run_lower_bounding(dc_rw_trend, mc_rw_trend, force = F)
eval_heatmap(dc_rw_trend,
             mc_rw_trend,
             "rw-trend",
             "sax", "lrrsaxres",
             "T", "trend-strength",
             "Time Series Length T",
             expression(paste("Trend Strength R"[tr]^"2"*" (%)")),
             "lower-bounding", mean, F, ma_2 = 1,
             limits = c(-0.05, 0.05))

# +++ Exact Matching -----------------------------------------------------------
run_exact_search(dc_rw_trend, mc_rw_trend, force = F)
eval_heatmap(dc_rw_trend,
             mc_rw_trend,
             "rw-trend",
             "sax", "lrrsaxres",
             "T", "trend-strength",
             "Time Series Length T",
             expression(paste("Trend Strength R"[tr]^"2"*" (%)")),
             "exact-search", pruning_power, fct_arg = T, ma_2 = 1,
             limits = c(-0.05, 0.05))

# +++ Approximate Matching -----------------------------------------------------
run_approximate_search(dc_rw_trend, mc_rw_trend, force = F)
eval_heatmap(dc_rw_trend,
             mc_rw_trend,
             "rw-trend",
             "sax", "lrrsaxres",
             "T", "trend-strength",
             "Time Series Length T",
             expression(paste("Trend Strength R"[tr]^"2"*" (%)")),
             "approximate-search", approximate_accuracy, fct_arg = T, ma_2 = 1,
             limits = c(-0.05, 0.05))

# + Real -----------------------------------------------------------------------
# ++ Season --------------------------------------------------------------------
# +++ Symbolic Distribution ----------------------------------------------------
run_represent(dc_real_season, mc_real_season_a_fix, force = F)
eval_res_symbols(dc_real_season, mc_real_season_a_fix)

# +++ Representation Accuracy --------------------------------------------------
run_lower_bounding(dc_real_season, mc_real_season, force = F)
eval_minmax(dc_real_season,
            mc_real_season,
            "SAX", "sSAX", "Mean TLB (%)",
            "lower-bounding", mean, F, digit = 1,
            eval_color = eval_color[c(5, 1)],
            ylim = c(30, 45), ybreaks = seq(30, 45, 5))

# +++ Exact Matching -----------------------------------------------------------
run_exact_search(dc_real_season, mc_real_season, force = F)
eval_minmax(dc_real_season,
            mc_real_season,
            "SAX", "sSAX", "Mean PP (%)",
            "exact-search", pruning_power, T, digit = 1,
            y_off = 0.0, x_off = -0.1,
            eval_color = eval_color[c(5, 1)],
            ylim = c(1, 7), ybreaks = seq(1, 7, 2))

# +++ Approximate Matching -----------------------------------------------------
run_approximate_search(dc_real_season, mc_real_season, force = F)
eval_minmax(dc_real_season,
            mc_real_season,
            "SAX", "sSAX", "Mean AA (%)",
            "approximate-search", approximate_accuracy, T, digit = 1,
            x_off = 0.50, y_off = -0.35,
            eval_color = eval_color[c(5, 1)],
            ylim = c(86, 92), ybreaks = seq(86, 92, 2))

# ++ Trend ---------------------------------------------------------------------
# +++ Symbolic Distribution ----------------------------------------------------
run_represent(dc_real_trend, mc_real_trend_a_fix, force = F)
eval_res_symbols(dc_real_trend, mc_real_trend_a_fix)

# +++ Representation Accuracy --------------------------------------------------
run_lower_bounding(dc_real_trend, mc_real_trend, force = F)
eval_minmax(dc_real_trend, mc_real_trend, "SAX", "tSAX",
            "Mean TLB (%)", "lower-bounding", mean, F, digit = 1, diff = F,
            eval_color = eval_color[c(5, 4, 2)],
            name_3 = "1d_SAX",
            ylim = c(40, 90), ybreaks = seq(40, 90, 10))

# +++ Exact Matching -----------------------------------------------------------
run_exact_search(dc_real_trend, mc_real_trend, force = F)
eval_minmax(dc_real_trend,
            mc_real_trend,
            "SAX", "tSAX", "Mean PP (%)",
            "exact-search", pruning_power, T, digit = 1,
            eval_color = eval_color[c(5, 4, 2)],
            name_3 = "1d_SAX",
            ylim = c(50, 100), ybreaks = seq(50, 100, 10))

# +++ Approximate Matching -----------------------------------------------------
run_approximate_search(dc_real_trend, mc_real_trend, force = F)
eval_minmax(dc_real_trend,
            mc_real_trend,
            "SAX", "tSAX", "Mean AA (%)",
            "approximate-search", approximate_accuracy, T, digit = 1, diff = F,
            eval_color = eval_color[c(5, 4, 2)],
            name_3 = "1d_SAX",
            ylim = c(70, 100), ybreaks = seq(70, 100, 10))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++-------------------------------------------------------
# Efficiency Experiments (not automatic) ---------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + Generation -----------------------------------------------------------------
# - copy manually to second drive
# - only generate 100Gb and derive smaller datasets automatically
generate_random_walk(dc_rw_season_large_10[4], force = T, dat = T, opt = F)
generate_random_walk(dc_rw_season_large_50[4], force = T, dat = T, opt = F)
generate_random_walk(dc_rw_season_large_90[4], force = T, dat = T, opt = F)
# - correct season strength in the folder names to 10, 50, and 90
# + Represent ------------------------------------------------------------------
run_represent(dc_rw_season_large_10_u[4], mc_rw_season_large_10, force = T, large = T)
run_represent(dc_rw_season_large_50_u[4], mc_rw_season_large_50, force = T, large = T)
run_represent(dc_rw_season_large_90_u[4], mc_rw_season_large_90, force = T, large = T)
# + Export lookup tables -------------------------------------------------------
prepare_exact_search_runtime_method(dc_rw_season_large_10_u, mc_rw_season_large_10)
prepare_exact_search_runtime_method(dc_rw_season_large_50_u, mc_rw_season_large_50)
prepare_exact_search_runtime_method(dc_rw_season_large_90_u, mc_rw_season_large_90)
# + Query Generation -----------------------------------------------------------
# - uncomment and generate and store once
# Q1G <- base::sample(dc_rw_season_large_10_u[[1]]$I, 100)
# Q10G <- base::sample(dc_rw_season_large_10_u[[2]]$I, 100)
# Q50G <- base::sample(dc_rw_season_large_10_u[[3]]$I, 100)
# Q100G <- base::sample(dc_rw_season_large_10_u[[4]]$I, 100)
# util_save(dc_rw_season_large_10_u[[4]], "Q1G", data = Q1G)
# util_save(dc_rw_season_large_10_u[[4]], "Q10G", data = Q10G)
# util_save(dc_rw_season_large_10_u[[4]], "Q50G", data = Q50G)
# util_save(dc_rw_season_large_10_u[[4]], "Q100G", data = Q100G)
# + Query Loading --------------------------------------------------------------
Q1G <- util_read(dc_rw_season_large_10_u[[4]], "Q1G")
Q10G <- util_read(dc_rw_season_large_10_u[[4]], "Q10G")
Q50G <- util_read(dc_rw_season_large_10_u[[4]], "Q50G")
Q100G <- util_read(dc_rw_season_large_10_u[[4]], "Q100G")
# + HDD ------------------------------------------------------------------------
run_exact_search_runtime_c("EDBT2020EXT", dc_rw_season_large_10_u[4], mc_rw_season_large_10, dc_rw_season_large_10_u[[3]]$I, Q50G[1:10])
run_exact_search_runtime_c("EDBT2020EXT", dc_rw_season_large_50_u[4], mc_rw_season_large_50, dc_rw_season_large_50_u[[3]]$I, Q50G[1:10])
run_exact_search_runtime_c("EDBT2020EXT", dc_rw_season_large_90_u[4], mc_rw_season_large_90, dc_rw_season_large_90_u[[3]]$I, Q50G[1:4])
run_exact_search_runtime_c("EDBT2020EXT", dc_rw_season_large_10_u[4], mc_rw_season_large_10, dc_rw_season_large_10_u[[4]]$I, Q100G[1:10])
run_exact_search_runtime_c("EDBT2020EXT", dc_rw_season_large_50_u[4], mc_rw_season_large_50, dc_rw_season_large_50_u[[4]]$I, Q100G[1:5])
run_exact_search_runtime_c("EDBT2020EXT", dc_rw_season_large_90_u[4], mc_rw_season_large_90, dc_rw_season_large_90_u[[4]]$I, Q100G[1])
# + SSD ------------------------------------------------------------------------
# Run SAX and sSAX seperately, avoid buffering
run_exact_search_runtime_c("EDBT2020", dc_rw_season_large_50_u[4], list(list("ed")), dc_rw_season_large_50_u[[3]]$I, Q50G[1:2])
run_exact_search_runtime_c("EDBT2020", dc_rw_season_large_50_u[4], list(list("ed")), dc_rw_season_large_50_u[[4]]$I, Q100G[1:2])
run_exact_search_runtime_c("EDBT2020", dc_rw_season_large_10_u[4], mc_rw_season_large_10, dc_rw_season_large_10_u[[3]]$I, Q50G[1:10])
run_exact_search_runtime_c("EDBT2020", dc_rw_season_large_10_u[4], mc_rw_season_large_10, dc_rw_season_large_10_u[[4]]$I, Q100G[1:10])
run_exact_search_runtime_c("EDBT2020", dc_rw_season_large_50_u[4], mc_rw_season_large_50, dc_rw_season_large_50_u[[3]]$I, Q50G[1:10])
run_exact_search_runtime_c("EDBT2020", dc_rw_season_large_50_u[4], mc_rw_season_large_50, dc_rw_season_large_50_u[[4]]$I, Q100G[1:10])
run_exact_search_runtime_c("EDBT2020", dc_rw_season_large_90_u[4], mc_rw_season_large_90, dc_rw_season_large_90_u[[3]]$I, Q50G[1:20])
run_exact_search_runtime_c("EDBT2020", dc_rw_season_large_90_u[4], mc_rw_season_large_90, dc_rw_season_large_90_u[[4]]$I, Q100G[1:20])
# + Evaluation -----------------------------------------------------------------
eval_exact_search_runtime_c(dc_rw_season_large_90_u[4], mc_rw_season_large_90, "foo",
                            dim_x = "foo", lab_x = "foo", limits = NA, disk = "SSD")

eval_exact_search_runtime_c(dc_rw_season_large_90_u[3], mc_rw_season_large_90, "foo",
                            dim_x = "foo", lab_x = "foo", limits = NA, disk = "HDD")
