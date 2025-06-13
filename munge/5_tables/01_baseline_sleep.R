
tbls[["baseline_sleep"]] <-
  general_sleep |>
  mutate(
    max = if_else(max > 24, max - 24, max)
    , min = case_when( 
      str_starts(name, "pref_sleep") | str_starts(name, "pref_wake") ~ clock_24(min)
      , T ~ as.character(min))
    , max = case_when(
      str_starts(name, "pref_sleep") | str_starts(name, "pref_wake") ~ clock_24(max)
      , T ~ as.character(max))
  ) |>
  bind_rows( sleep_quiz_summary |> mutate(across(c(min,max),  ~fmt_APA_numbers(.x, .chr=T))) ) |>
  mutate(
    across(c(where(is.numeric), -alpha), ~fmt_APA_numbers(.x, .chr=T))
    , across(everything(), ~if_else(is.na(.x), "", as.character(.x)))
    , range = if_else(min=="", "", paste0(min, " - ", max))
    , n = as.integer(n)
    , n = if_else(is.na(n), "", as.character(n))
  )  |>
  select(-min, -max) |>
  gt() |>
  tab_spanner("Data", c(m, sd, range)) |>
  tab_spanner("Reliability", c(n, alpha, CI)) |>
  cols_label(
    name  ~ "Measure"
    , m     ~ md("*M*")
    , sd    ~ md("*SD*")
    , range ~ "Data range"
    , alpha ~ md("$\\alpha$")
    , CI = md("95% CI")
  ) |>
  cols_move(range, sd) |>
  text_case_match(
    "night_shift"        ~ "Night shift",
    "preferential_sleep" ~ "Preferential sleep",
    "pref_sleep"         ~ "Time of going to bed",
    "pref_wake"          ~ "Time of awakening",
    "pref_sleep_dur"     ~ "Duration of sleep",
    "actual_sleep"       ~ "Typical sleep duration",
    "sleep_week"         ~ "Weekday",
    "sleep_week_end"     ~ "Weekend",
    "coff_day"           ~ "Coffee consumption",
    "energy_drink"       ~ "Number of energy drinks",
    "fatigue"            ~ "Fatigue",
    "sleepiness"         ~ "Sleepiness",
    "insomnia"           ~ "Insomnia", 
    "pittsburgh"         ~ "Sleep quality", 
    "dinural_avg"        ~ "Dinural preference",
    "PANAS"              ~ "PANAS", 
    "panas_pos"          ~ "Positive",
    "panas_neg"          ~ "Negative", 
    "alcohol"            ~ "Alcohol",
    .default="", .locations = cells_body(columns = name)
  ) |>
  opt_footnote_marks(marks = letters) |>
  tab_footnote("On weekdays.", locations = cells_body(name, 2) ) |>
  tab_footnote("On a typical day.", locations = cells_body(name, c(9,10)) ) |>
  tab_footnote("Measured with Fatigue Severity Scale (Krupp et al., 1989).", 
               locations = cells_body(name, c(11)) ) |>
  tab_footnote("Measured with Epworth Sleepiness Scale (Johns, 1991).", 
               locations = cells_body(name, c(12)) ) |> 
  tab_footnote("Measured with Insomnia Severity Index (Bastien et al., 2001).", 
               locations = cells_body(name, c(13)) ) |>
  tab_footnote("Measured with Pittsburgh Sleep Quality Index, low scores indicate good sleep quality (Buysse et al., 1989).", 
               locations = cells_body(name, c(14)) ) |>
  tab_footnote("Measured with Diurnal Scale (calculated as mean), low scores indicate a preference to go to bed late and wake up late (Torsvall & Åkerstedt, 1980).", 
               locations = cells_body(name, c(15)) ) |>
  tab_footnote("Measured with the Positive and Negative Affect Schedule (Watson et al., 1988).", 
               locations = cells_body(name, c(16)) ) |>
  tab_footnote("Measure provided by Saksvik-Lehouillier et al. (2020).", 
               locations = cells_body(name, c(19)) ) |>
  tab_fmt_APA() |>
  cols_align("center", c(everything(), -name))


conditional_save(
  tbls[["baseline_sleep"]]
  , "Baseline - sleep"
)
