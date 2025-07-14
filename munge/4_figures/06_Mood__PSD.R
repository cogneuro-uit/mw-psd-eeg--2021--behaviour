# # Model
# figs[["Mood__PSD"]] <-""
#   expand_grid(
#     name = c("Outcome")
#     # SD can vary from -1 to 1 (z score)
#     , sleep_deviation = c(-1, 0, 1)
#     , mw = c(-1,0,1)
#   ) |>
#   mutate(
#     # Z-score mood and sleep
#     sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation),
#     sleep = summarised_vals$mw_m + (summarised_vals$mw_sd * mw),
# 
#     # positive mood
#     , pos_ns  = mean( mood$cont$p_s.m[["b_Intercept"]] )
#     , pos_psd = pos_ns + mean(mood$cont$p_s.m[["b_c.Adjusted_Duration.diff.pos"]]) * sleep
#     , pos_mw  = pos_ns + mean(mood$cont$p_s.m[["b_c.Adjusted_Duration.diff.pos:mw"]]) * sleep * mw
#     # positive mood
#     , neg_ns  = mean( mood$cont$n_s.m[["b_Intercept"]] )
#     , neg_psd = pos_ns + mean( mood$cont$n_s.m[["b_c.Adjusted_Duration.diff.pos"]]) * sleep
#     , neg_mw  = pos_ns + mean( mood$cont$n_s.m[["b_c.Adjusted_Duration.diff.pos:mw"]]) * sleep * mw
#   ) |>
#   pivot_longer(c(ends_with("psd"), ends_with("ns")), names_to="names") |>
#   separate_wider_delim(names, "_", names_sep = "_", names = c("mood", "cond")) |>
#   mutate(cond = case_when(
#     names_cond == "ns" ~ "NS"
#     , sleep_deviation == -1 ~ "PSD -1 SD"
#     , sleep_deviation == 0 ~ "PSD Mean"
#     , sleep_deviation == +1 ~ "PSD +1 SD"
#     ) |> factor(levels = c("PSD +1 SD", "PSD Mean", "PSD -1 SD", "NS"))
#   , mood = case_when(
#     names_mood=="pos" ~ "Positive"
#     , names_mood=="neg" ~ "Negative"
#     ) |> fct_relevel("Positive") 
#   ) |> 
#   ggplot(aes(mw, pos_mw, col = cond, linetype = cond)) +
#   facet_wrap( ~ mood, scales="free" ) +
#   geom_line( linewidth = 1 ) +
#   labs(y = "Mood score", col = "Condition", linetype = "Condition") +
#   scale_color_manual(    values = name_colour_interactions ) +
#   scale_linetype_manual( values = name_line_interactions ) +
#   theme( legend.position = "top", legend.direction = "horizontal" )
# 
#   
# conditional_save(
#   figs[["Mood__PSD"]]
#   , "Effect of sleep deprivation on mood"
# )
# 
