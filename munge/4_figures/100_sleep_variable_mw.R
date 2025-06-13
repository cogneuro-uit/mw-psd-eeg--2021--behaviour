
figs[["sleep_variables_mw"]] <- 
  data.probe.mood.sleep |>
  mutate(sleepdep = fct_relevel(sleepdep, "SD")) |>
  pivot_longer(c(c.Self.report_Duration.diff.pos, c.Actigraphy_Duration.diff.pos, c.Adjusted_Duration.diff.pos)) |>
  summarise(
    .by = c(subj, sleepdep, name), 
    sleep = unique(value),
    mw = mean(as.numeric(mw)),
  ) |>
  mutate(name = case_when(
    str_detect(name, "Self.report") ~ "Self report"
    , str_detect(name, "Actigraphy") ~ "Actigraphy"
    , str_detect(name, "Adjusted") ~ "Adjusted") |> fct_relevel(c("Self report", "Actigraphy"))
    , Condition = if_else(sleepdep=="SD", "PSD", "NS") |> fct_relevel("PSD")
    , modalityXcondition = case_when(
      Condition == "NS"    ~ "NS"
      , Condition == "PSD" ~ paste0(name," PSD")
    ) |> factor(levels = c("NS", "Self report PSD", "Actigraphy PSD", "Adjusted PSD"))
    , modalityXsubj       = paste0(name, "_", subj)
  ) |>
  ggplot(aes(sleep, mw, col = modalityXcondition, group = modalityXsubj, alpha = Condition)) +
  geom_point(alpha = .5) +
  geom_line(aes(col = NULL), alpha = .05) +
  scale_alpha_manual(values = c("PSD"=.2, "NS"=0.01)) +
  geom_smooth(aes(group = name, col = name), method = "lm", alpha = .08) +
  ggrepel::geom_text_repel(aes(label = if_else(sleep == 0, NA, subj)), alpha = .6) +
  scale_colour_manual(values = gen_col("/G-r-b-grbg")) +
  labs(x = "Average sleep deprivation", y = "Mind wandering", col = "Condition")

conditional_save(
  figs[["sleep_variables_mw"]]
  , "The relationship between sleep deprivation and MW tendency between modalities"
)
