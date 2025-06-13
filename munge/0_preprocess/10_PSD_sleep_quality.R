
PSD_sleep_quality <- 
  sleeptimes_updated |>
  filter(pre_control==1 | pre_sleepdep==1) |>
  mutate(psd = case_when(
    pre_control == 1 ~ "control"
    , pre_sleepdep == 1 ~ "SD"
  )) |>
  left_join(
    sleeptimes_updated_trans
    , by = join_by("subj", "psd"=="sleepdep")
  ) |>
  summarise(
    .by = c(subj, psd), 
    , sleep_quality = mean(sleep_quality, na.rm=T)
    , adj = unique(c.Adjusted_Duration.diff.pos)
    , sr = unique(Self.report_Duration.diff.pos)
    , ag = unique(Actigraphy_Duration.diff.pos)
  ) |>
  pivot_wider(names_from = psd, values_from = c(sleep_quality, adj, sr, ag)) |>
  mutate(
    across(ends_with("_control"), ~ 0)
    , sq_diff = sleep_quality_control - sleep_quality_SD
  ) |> 
  pivot_longer(c(adj_SD, sr_SD, ag_SD)) |>
  mutate(name = case_when(
    name == "adj_SD" ~ "Adjusted sleep"
    , name == "ag_SD" ~ "Actigraphy sleep"
    , name == "sr_SD" ~ "Self-reported sleep"
  )) |> 
  rename(sleep = value)
