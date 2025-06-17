tbls[["test_sleep_vars_across_modality"]] <- 
  sleeptimes_updated_trans |>
  select(-starts_with("Adjusted"), -starts_with("c.")) |>
  pivot_longer(c(starts_with("Self"), starts_with("Actig"))) |>
  separate_wider_delim(name, "_", names = c("Source", "Sleep")) |>
  filter( !str_detect(Sleep, ".diff") ) |>
  pivot_wider(names_from = Source, values_from = value) |>
  summarise(
    .by = c(sleepdep, Sleep),
    SR_m = mean( Self.report, na.rm=T),
    SR_sd = sd(  Self.report, na.rm=T),
    AG_m = mean( Actigraphy, na.rm=T),
    AG_sd = sd(  Actigraphy, na.rm=T),
    diff = SR_m - AG_m,
    diff_sd = sd(Self.report - Actigraphy, na.rm=T),
    bf   = extractBF( 
      ttestBF(Self.report[!is.na(Self.report) & !is.na(Actigraphy)], 
              Actigraphy[!is.na(Self.report) & !is.na(Actigraphy)], paired=T) 
    )$bf,
    SR_m   = if_else(SR_m > 24, SR_m - 24, SR_m),
    AG_m    = if_else(AG_m  > 24, AG_m  - 24, AG_m),
  ) |>
  mutate(
    across(ends_with("_m"), ~ if_else(
      Sleep=="Duration", clock_h_m(.x), clock_24(.x))),
    across(c(ends_with("_sd"), "diff"), ~clock_h_m(.x)),
    bf = if_else(bf>1000, format(bf, scientific=T, digits=2), 
                 fmt_APA_numbers(bf, .chr=T)),
    sleepdep = if_else(sleepdep=="SD", "Partial sleep deprivation", "Normal sleep"),
    e1 = "", e2="", 
    Sleep = case_when(
      Sleep == "Wake" ~ "Wake time", 
      Sleep == "Onset" ~ "Onset time",
      Sleep == "Duration" ~ "Sleep Duration"
    )) |> 
  gt(groupname_col = "sleepdep") |>
  tab_spanner("Self-report", starts_with("SR_")) |>
  tab_spanner("Actigraphy", starts_with("AG_")) |>
  cols_label(
    ends_with("_m") ~ md("*M*"),
    ends_with("_sd") ~ md("*SD*"),
    diff = md("*M*~diff~"), 
    diff_sd = md("*SD*~diff~"),
    bf = md("BF~10~"),
    sleepdep = "Variable",
    starts_with("e") ~ "", 
  ) |> 
  cols_move(e1, SR_sd) |>
  cols_move(e2, AG_sd) |>
  cols_align("center", c(everything(),-Sleep, -sleepdep)) |>
  tab_footnote(md(
    "*Note.* Differences are calculated as self-report - actigraphy.")) |>
  opt_footnote_marks(marks = letters) |>
  tab_footnote(md(
    "Actigraphy has two less *n*, as two actigraphies malfunctioned during the data collection."), 
    locations = cells_column_spanners("Actigraphy") )

conditional_save(
  tbls[["test_sleep_vars_across_modality"]]
  , "Test sleep measures across modalities"
)
