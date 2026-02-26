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

fig_cond <- data.frame(c.Adjusted_Duration.diff.pos = c(
  "NS"            = 0
  , "PSD -1 SD"   = summarised_vals$sleep_m - summarised_vals$sleep_sd
  , "PSD Mean"    = summarised_vals$sleep_m
  , "PSD +1 SD"   = summarised_vals$sleep_m + summarised_vals$sleep_sd
))


default_time_plot <- function(data, x_scale = "z_score"){
  require("rlang")
  
  x_scale = ensym(x_scale)
  
  ggplot(data, aes(!!x_scale, value, col = cond, linetype=cond)) +
    geom_line(linewidth = 1) +
    scale_fill_manual(     values = name_colour_interactions ) +
    scale_color_manual(    values = name_colour_interactions ) +
    scale_linetype_manual( values = name_line_interactions ) +
    theme(legend.position = "top", legend.direction = "horizontal") +
    labs(col = "Condition", fill = "Condition", linetype = "Condition")
}


# Specific for the ---cond figures
default_geoms <- function(data){
  ggplot(data, aes(effect1__, estimate__)) +
    geom_line(aes(col = cond__, linetype = cond__), linewidth = .6) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cond__), alpha = .07) +
    scale_fill_manual(     values = name_colour_interactions ) +
    scale_color_manual(    values = name_colour_interactions ) +
    scale_linetype_manual( values = name_line_interactions ) +
    theme(legend.position = "top", legend.direction = "horizontal") +
    labs(col = "Condition", fill = "Condition", linetype = "Condition")
}

z_score <- function(ylabel = "ylab", xlabel = "xlab", axis = "y", zoom = c(-1, 1)) {
  # Validate axis argument
  axis <- tolower(axis)
  if (!(axis %in% c("x", "y", "xy", "yx"))) {
    stop("'axis' must be one of 'x', 'y', 'xy', or 'yx'")
  }
  
  if (axis == "x") {
    return(list(
      geom_vline(xintercept = 0, linetype = "dotdash", colour = gen_col("B"), alpha = .5)
      , coord_cartesian(xlim = zoom)
      , xlab(xlabel)
    ))
  }
  if (axis == "y") {
    return(list(
      geom_hline(yintercept = 0, linetype = "dotdash", colour = gen_col("B"), alpha = .5)
      , coord_cartesian(ylim = zoom)
      , ylab(ylabel)
    ))
  }
  if (axis == "xy" || axis == "yx") {
    return(list(
      geom_hline(yintercept = 0, linetype = "dotdash", colour = gen_col("B"), alpha = .5)
      , geom_vline(xintercept = 0, linetype = "dotdash", colour = gen_col("B"), alpha = .5)
      , coord_cartesian(ylim = zoom, xlim = zoom)
      , xlab(xlabel) # deparse and retrieve x = "label"
      , ylab(ylabel) # 
    ))
  }
}

probe <- function(label="label", axis = "y"){
  # Validate axis argument
  axis <- tolower(axis)
  if (!(axis %in% c("x", "y", "xy", "yx"))) {
    stop("'axis' must be one of 'x', 'y', 'xy', or 'yx'")
  }
  
  if (axis == "x") {
    return(list(
      coord_cartesian(xlim = c(1, 4))
      , xlab(label)
    ))
  }
  if (axis == "y") {
    return(list(
      coord_cartesian(ylim = c(1, 4))
      , ylab(label)
    ))
  }
}

time <- function(){
  return(list(
    scale_x_continuous(breaks = c(0.04, .2, .4, .6, .8, 1), labels = c(1, seq(5,26,5)))
    , xlab("Probe number (time-on-task)")
  ))
}
probe_length <- time()

mood_scale <- function(mood, axis) {
  # Construct the variable names dynamically
  mean_val <- summarised_vals[[paste0("mood_", mood, "_m")]]
  sd_val <- summarised_vals[[paste0("mood_", mood, "_sd")]]
  mood_label <- if_else(mood=="pos", "Z-score positive mood", "Z-score negative mood")
  
  # Calculate breaks
  breaks <- c(
    mean_val - sd_val,
    mean_val - sd_val / 2,
    mean_val,
    mean_val + sd_val / 2,
    mean_val + sd_val
  )
  # Choose the correct scale function based on the axis
  scale_fn <- if (axis == "x") scale_x_continuous else scale_y_continuous
  
  # Return the scale with breaks and labels
  return(list(
    scale_fn(breaks = breaks, labels = c(-1, -0.5, 0, 0.5, 1))
    , if (axis == "x") xlab(mood_label) else ylab(mood_label)
    , if (axis == "x") coord_cartesian(xlim=c(breaks[[1]],breaks[[5]])) else coord_cartesian(ylim=c(breaks[[1]],breaks[[5]]))
    , if (axis == "x") geom_vline(xintercept = breaks[[3]], linetype = "dotdash", colour = gen_col("B"), alpha = .5) else geom_hline(yintercept = breaks[[3]],linetype = "dotdash", colour = gen_col("B"), alpha = .5)
  ))
}


# find the base intercept for the probes 
c$mw_i <- bayes_coef_intercept(mod.cont$mw, "pnorm")[["estimate"]]
c$mb_i <- bayes_coef_intercept(mod.cont$mb, "pnorm")[["estimate"]]
c$smw_i <- bayes_coef_intercept(mod.cont$smw, "pnorm")[["estimate"]]



