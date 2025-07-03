figs[["sleep_variables_mw"]] <-
  data.probe.mood.sleep |>
  mutate(sleepdep = fct_relevel(sleepdep, "SD")) |>
  pivot_longer(c(c.Self.report_Duration.diff.pos, c.Actigraphy_Duration.diff.pos, c.Adjusted_Duration.diff.pos)) |>
  summarise(
    .by = c(subj, sleepdep, name), 
    sleep = unique(value),
    mw = mean(as.numeric(mw)),
  ) |>
  mutate(
    name = case_when(
      str_detect(name, "Self.report") ~ "Self report"
      , str_detect(name, "Actigraphy") ~ "Actigraphy"
      , str_detect(name, "Adjusted") ~ "Adjusted"
    ) |> fct_relevel(c("Self report", "Actigraphy"))
    , Condition = if_else(sleepdep=="SD", "PSD", "NS") |> fct_relevel("PSD")
    , modalityXcondition = case_when(
      Condition == "NS"    ~ "NS"
      , Condition == "PSD" ~ paste0(name," PSD")
    ) |> factor(levels = c("NS", "Self report PSD", "Actigraphy PSD", "Adjusted PSD"))
    , modalityXsubj       = paste0(name, "_", subj)
    # highlight certain partic
    , highlight_subj = case_when(
      subj %in% c("003", "012", "005", "017", "023") ~ T
      , T ~ F )
    , alpha_dots = case_when(
      highlight_subj == T & Condition == "PSD" ~ "Highlight_dot"
      , T ~ Condition )
    , alpha_lines = case_when(
      highlight_subj == T ~ "Highlight_line"
      , T ~ "weak_line"
    )
  ) |> 
  ggplot(aes(sleep, mw, col = modalityXcondition, group = modalityXsubj)) +
  # aesthetics
  scale_colour_manual(values = gen_col("/G-r-b-grbg")) + # + the general (smooth) <<lines
  scale_alpha_manual(values = c("NS" = 0.0, "PSD" = 0.25, "Highlight_dot" = .9,
                                "Highlight_line" = .4, "weak_line" = .035)) +
  # connecting lines
  geom_line(aes(col = NULL, alpha = alpha_lines)) +
  # Dots
  geom_point(aes(alpha = alpha_dots)) +
  geom_text_repel(aes(label = if_else(sleep == 0, NA, subj), alpha = alpha_dots)) +
  # grand lines 
  geom_line(aes(group = name, col = name), stat = "smooth", method = "lm", alpha = .7, linewidth = .7) +
  labs(x = "Average sleep deprivation", y = "Mind wandering", col = "Condition") +
  guides(alpha = "none")

conditional_save(
  figs[["sleep_variables_mw"]]
  , "The relationship between sleep deprivation and MW tendency between modalities"
)
