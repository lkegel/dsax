#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Util -------------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
mc_read_csv <- function(config_name) {
  # suppressPackageStartupMessages(require(openxlsx))
  fp <- mc_get_path(config_name, "xlsx")
  stopifnot(file.exists(fp))
  
  tbl <- read.table(fp, T, ";", check.names = F, stringsAsFactors = F)
  # tbl <- read.xlsx(fp, sheet = 1, skipEmptyRows = FALSE)
  res <- list()
  for (irow in seq(nrow(tbl))) {
    row <- tbl[irow, ]
    if (row$name == "sax") {
      e <- list("sax", a = row$a, w = row$w, `dist-type` = row$`dist-type`,
                sd = row$sd)
    } else if(row$name == "seassaxres") {
      e <- list("seassaxres", a_seas = row$a_seas, a = row$a, w = row$w,
                `dist-type` = row$`dist-type`, sd = row$sd)
    } else if(row$name == "lrrsaxres") {
      e <- list("lrrsaxres", a_lrr = row$a_lrr, a = row$a, w = row$w,
                `dist-type` = row$`dist-type`, sd = row$sd)
    } else if(row$name == "lrrressax") {
      e <- list("lrrressax", a = row$a, w = row$w,
                `dist-type` = row$`dist-type`, sd = row$sd)
    } else if(row$name == "1d_sax") {
      e <- list("1d_sax", a = row$a, a_s = row$a_lrr, w = row$w,
                `dist-type` = row$`dist-type`, sd = row$sd)
    } else if (row$name %in% c("paa", "lrrpaa", "seaspaa")) {
      e <- list(row$name, w = row$w)
    } else {
      stop("mc_read_csv: type not available")
    }
    
    if ("dist-type" %in% names(e) && e$`dist-type` != "norm") {
      idx <- which(names(e) == "sd")
      e <- e[-idx]
    }
      
    
    res[[irow]] <- e
  }
  
  return(res)
}

mc_get_path <- function(config_name, ext = "csv") {
  path <- file.path("Implementation", "configs", paste0(config_name, ".", ext))
  
  return(path)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RW Season --------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
mc_rw_season_a_fix <- mc_read_csv("mc_rw_season_a_fix")
mc_rw_season <- mc_read_csv("mc_rw_season")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RW Trend ---------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
mc_rw_trend_a_fix <- mc_read_csv("mc_rw_trend_a_fix")
mc_rw_trend <- mc_read_csv("mc_rw_trend")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Real Season ------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
mc_real_season_a_fix <- mc_read_csv("mc_real_season_a_fix")
mc_real_season <- mc_read_csv("mc_real_season")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Real Trend -------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
mc_real_trend_a_fix <- mc_read_csv("mc_real_trend_a_fix")
mc_real_trend <- mc_read_csv("mc_real_trend")