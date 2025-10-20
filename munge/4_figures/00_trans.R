name_colour_interactions <- c(
  "NS"        = gen_col("b"),
  "PSD -1 SD" = gen_col("-r"),
  "PSD Mean"  = gen_col("r"), 
  "PSD +1 SD" = gen_col("+r"))
name_line_interactions <- c(
  "NS"        = "dotted", 
  "PSD -1 SD" = "dashed",
  "PSD Mean"  = "solid", 
  "PSD +1 SD" = "longdash")


c$mw_i <- bayes_coef_intercept(mod.cont$mw, "pnorm")[["estimate"]]
c$mb_i <- bayes_coef_intercept(mod.cont$mb, "pnorm")[["estimate"]]
c$smw_i <- bayes_coef_intercept(mod.cont$smw, "pnorm")[["estimate"]]
