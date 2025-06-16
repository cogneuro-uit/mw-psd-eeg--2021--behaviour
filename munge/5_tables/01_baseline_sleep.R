tbls[["baseline_sleep"]] <-
  general_sleep |>
  mutate( 
    across(c(m,sd), ~fmt_APA_numbers(.x, .chr=T)) 
    , across(c(min, max), ~as.character(.x)) 
  ) |>
  bind_rows( 
    sleep_quiz_summary |> mutate(
      across(c(m,sd,min,max),  ~fmt_APA_numbers(.x, .chr=T))
      , n = as.character(n) )
  ) |>
  mutate(
    range = if_else(is.na(min), NA, sprintf("%s - %s", min, max))
    , across(c(everything(), -name), ~
               if_else(name %in% c("preferential_sleep", "actual_sleep", "PANAS"), "",.x))
  )  |>
  filter(!(name == "dinural_sum")) |>
  select(-min, -max) |>
  gt() |>
  tab_spanner("Data", c(m, sd, range)) |>
  tab_spanner("Reliability", c(n, alpha, CI)) |>
  cols_label(
    name  ~ "Measure"
    , m     ~ md("*M*")
    , sd    ~ md("*SD*")
    , range ~ "Range"
    , alpha ~ md("$\\alpha$")
    , CI = md("95% CI")
  ) |>
  cols_move(range, sd) |>
  text_case_match(
    "night_shift"        ~ "Night shift ",
    "preferential_sleep" ~ "Preferential sleep",
    "pref_sleep"         ~ "Time of going to bed (HH:MM)",
    "pref_wake"          ~ "Time of awakening (HH:MM)",
    "pref_sleep_dur"     ~ "Duration of sleep",
    "actual_sleep"       ~ "Typical sleep duration during",
    "sleep_week"         ~ "Weekdays",
    "sleep_week_end"     ~ "Weekends",
    "coff_day"           ~ "Coffee consumption (number of cups)",
    "energy_drink"       ~ "Number of energy drinks (units)",
    "fatigue"            ~ "Fatigue",
    "sleepiness"         ~ "Sleepiness",
    "insomnia"           ~ "Insomnia", 
    "pittsburgh"         ~ "Sleep quality", 
    "dinural_avg"        ~ "Dinural preference",
    "PANAS"              ~ "PANAS", 
    "panas_pos"          ~ "Positive",
    "panas_neg"          ~ "Negative", 
    "alcohol"            ~ "Alcohol habit score",
    .default = "", .locations = cells_body(columns = name)
  ) |>
  opt_footnote_marks(marks = letters) |>
  tab_footnote(md("*Note*. "))
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
  tab_footnote("Measure provided by Saksvik-Lehouillier et al. (2020), higher score indicate greater alcohol consumption habit", 
               locations = cells_body(name, c(19)) ) |>
  cols_align("center", c(everything(), -name)) |>
  fmt_missing()

conditional_save(
  tbls[["baseline_sleep"]]
  , "Baseline - sleep"
)
