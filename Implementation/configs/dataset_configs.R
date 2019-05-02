#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Season RW --------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dc_rw_season <- list(
  # 480
  list("random-walk",
       I                 = 1000,
       T                 = 480,
       L_1               = 10,
       `season-strength` = 99
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 480,
       L_1               = 10,
       `season-strength` = 80
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 480,
       L_1               = 10,
       `season-strength` = 60
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 480,
       L_1               = 10,
       `season-strength` = 40
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 480,
       L_1               = 10,
       `season-strength` = 20
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 480,
       L_1               = 10,
       `season-strength` = 1
  ),
  
  # 960
  list("random-walk",
       I                 = 1000,
       T                 = 960,
       L_1               = 10,
       `season-strength` = 99
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 960,
       L_1               = 10,
       `season-strength` = 80
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 960,
       L_1               = 10,
       `season-strength` = 60
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 960,
       L_1               = 10,
       `season-strength` = 40
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 960,
       L_1               = 10,
       `season-strength` = 20
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 960,
       L_1               = 10,
       `season-strength` = 1
  ),
  
  # 1440
  list("random-walk",
       I                 = 1000,
       T                 = 1440,
       L_1               = 10,
       `season-strength` = 99
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 1440,
       L_1               = 10,
       `season-strength` = 80
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 1440,
       L_1               = 10,
       `season-strength` = 60
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 1440,
       L_1               = 10,
       `season-strength` = 40
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 1440,
       L_1               = 10,
       `season-strength` = 20
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 1440,
       L_1               = 10,
       `season-strength` = 1
  ),
  
  # 1920
  list("random-walk",
       I                 = 1000,
       T                 = 1920,
       L_1               = 10,
       `season-strength` = 99
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 1920,
       L_1               = 10,
       `season-strength` = 80
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 1920,
       L_1               = 10,
       `season-strength` = 60
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 1920,
       L_1               = 10,
       `season-strength` = 40
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 1920,
       L_1               = 10,
       `season-strength` = 20
  ),
  list("random-walk",
       I                 = 1000,
       T                 = 1920,
       L_1               = 10,
       `season-strength` = 1
  )
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Trend RW -------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dc_rw_trend <- list(
  # T = 480
  list("random-walk",
       I                = 1000,
       T                = 480,
       `trend-strength` = 99),
  list("random-walk",
       I                = 1000,
       T                = 480,
       `trend-strength` = 80),
  list("random-walk",
       I                = 1000,
       T                = 480,
       `trend-strength` = 60),
  list("random-walk",
       I                = 1000,
       T                = 480,
       `trend-strength` = 40),
  list("random-walk",
       I                = 1000,
       T                = 480,
       `trend-strength` = 20),
  list("random-walk",
       I                = 1000,
       T                = 480,
       `trend-strength` = 1),
  
  # T = 960
  list("random-walk",
       I                = 1000,
       T                = 960,
       `trend-strength` = 99),
  list("random-walk",
       I                = 1000,
       T                = 960,
       `trend-strength` = 80),
  list("random-walk",
       I                = 1000,
       T                = 960,
       `trend-strength` = 60),
  list("random-walk",
       I                = 1000,
       T                = 960,
       `trend-strength` = 40),
  list("random-walk",
       I                = 1000,
       T                = 960,
       `trend-strength` = 20),
  list("random-walk",
       I                = 1000,
       T                = 960,
       `trend-strength` = 1),

  # T = 1440
  list("random-walk",
       I                = 1000,
       T                = 1440,
       `trend-strength` = 99),
  list("random-walk",
       I                = 1000,
       T                = 1440,
       `trend-strength` = 80),
  list("random-walk",
       I                = 1000,
       T                = 1440,
       `trend-strength` = 60),
  list("random-walk",
       I                = 1000,
       T                = 1440,
       `trend-strength` = 40),
  list("random-walk",
       I                = 1000,
       T                = 1440,
       `trend-strength` = 20),
  list("random-walk",
       I                = 1000,
       T                = 1440,
       `trend-strength` = 1),

  # T = 1920
  list("random-walk",
       I                = 1000,
       T                = 1920,
       `trend-strength` = 99),
  list("random-walk",
       I                = 1000,
       T                = 1920,
       `trend-strength` = 80),
  list("random-walk",
       I                = 1000,
       T                = 1920,
       `trend-strength` = 60),
  list("random-walk",
       I                = 1000,
       T                = 1920,
       `trend-strength` = 40),
  list("random-walk",
       I                = 1000,
       T                = 1920,
       `trend-strength` = 20),
  list("random-walk",
       I                = 1000,
       T                = 1920,
       `trend-strength` = 1)
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Season Real ------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dc_real_season <- list(
  list("issda",
       I                 = 5958,
       T                 = 21840,
       L_1               = 48)
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Trend Real -------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dc_real_trend <- list(
  list("m4",
       I                = 6400,
       T                = 300)
)