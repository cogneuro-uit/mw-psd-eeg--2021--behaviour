
# Quick summary
summarised_vals <- 
  data.probe.mood.sleep |> 
  filter(sleepdep=="SD") |> 
  summarise(
    sleep_m  = mean(c.Adjusted_Duration.diff.pos),
    sleep_sd = sd(c.Adjusted_Duration.diff.pos), 
    mood_pos_m   = mean(pre_pos),
    mood_pos_sd  = sd(pre_pos),
    mood_neg_m   = mean(pre_neg),
    mood_neg_sd  = sd(pre_neg),
  )


# Sleep Adjustment    ======
## Inter-rater agreement R1       =======
sleep_adjustment_round1_df <- 
  sleep_adjustment_r1 |>
  pivot_longer( c(wake_gc, onset_gc, wake_sa, onset_sa) ) |>
  separate_wider_delim(name, "_", names = c("cond", "eval"), too_many = "drop") |>
  mutate(eval = if_else(eval=="gc", "GC", "SA")) |>
  filter(!is.na(value)) |>
  pivot_wider(names_from = eval, values_from = value) |>
  filter(!is.na(GC) & !is.na(SA)) |>
  summarise(
    .by = cond, 
    cor = cor(GC, SA)
  )

## Inter-rater agreement R2       =======
sleep_adjustment_round2_df <- 
  sleep_adjustment_r2 |>
  mutate(
    use_wake = if_else(is.na(use_wake), FALSE, use_wake),
    use_onset = if_else(is.na(use_onset), FALSE, use_onset),
    SA_wake = wake_sa_MORE,
    SA_onset = onset_sa_MORE,
    GC_wake = if_else(use_wake, wake_sa_MORE, wake_GC),
    GC_onset = if_else(use_onset, onset_sa_MORE, onset_GC),
    wake_diff = SA_wake - GC_wake,
    wake_cond = abs(wake_diff) > .25,
    onset_diff = SA_onset - GC_onset,
    onset_cond = abs(onset_diff) > .25,
  ) |>
  pivot_longer(c(SA_wake,SA_onset,GC_wake, GC_onset)) |>
  separate_wider_delim(cols=name, "_", names_sep="_") |>
  filter(!is.na(value)) |>
  pivot_wider(names_from=name_1, values_from=value) |>
  filter(!is.na(GC) & !is.na(SA)) |>
  summarise(.by = name_2, cor = cor(GC,SA))



# Coff / alcohol      =====
demo_pre_test <- 
  demographics |>
  pivot_longer(c(S1_cond, S2_cond), names_to = c("Session", "_rm"), names_sep="_") |>
  rename(condition = value) |>
  mutate(no_caffeine = case_when( 
    Session == "S1" & condition == "SD" ~ S1_No_coffee.tea.energy_drinks,
    Session == "S2" & condition == "SD" ~ S2_No_coffee.tea.energy_drinks,
    Session == "S1" & condition == "control" ~ S1_No_coffee.tea.energy_drinks,
    Session == "S2" & condition == "control" ~ S2_No_coffee.tea.energy_drinks,
  ),
  no_alcohol = case_when(
    Session == "S1" & condition == "SD" ~ S1_No_alcohol,
    Session == "S2" & condition == "SD" ~ S2_No_alcohol,
    Session == "S1" & condition == "control" ~ S1_No_alcohol,
    Session == "S2" & condition == "control" ~ S2_No_alcohol,
  )) 

demo_pre_test_sum <-
  demo_pre_test |>
  pivot_longer(c(no_caffeine, no_alcohol)) |>
  summarise(
    .by = c(name, condition),
    sum = sum(!value, na.rm=T),
    na = sum(is.na(value), na.rm=T),
  ) 

# Baseline   =====
## Demographics   =====
demo_text_summary <- 
  demographics |> 
  mutate(Age = as.numeric(Age)) |>
  summarise(
    m   = mean(Age, na.rm=T), 
    sd  = sd(Age, na.rm=T), 
    min = min(Age,na.rm=T), 
    max = max(Age,na.rm=T)
  ) |> 
  mutate( across(1:2, ~fmt_APA_numbers(.x, .chr=T)),
          across(3:4, ~round(.x, 0)) )

# Reduced demographics
demographics_reduced <-
  demographics |>
  left_join(
    sleeptimes_updated_trans |> filter(sleepdep=="SD"),
    by = c("subj")
  ) |>
  filter(Adjusted_Duration.diff.pos > 1.5)

### Summary   =====
subj <- list(
  full =  list(
    num      = demographics$subj |> length(),
    age      = demographics$Age |> as.numeric() |> mean(na.rm=T) |> fmt_APA_numbers(),
    age_sd   = demographics$Age |> sd(na.rm=T)  |> fmt_APA_numbers(),
    age_min  = demographics$Age |> as.numeric() |> min(na.rm=T) |> fmt_APA_numbers(),
    age_max  = demographics$Age |> as.numeric() |> max(na.rm=T) |> fmt_APA_numbers(),
    gender_f = sum( demographics$Gender=="1" )
  ),
  red = list(
    num    = demographics_reduced |> pull(subj) |> length(),
    exc    = nrow(demographics) - nrow(demographics_reduced),
    age      = demographics_reduced$Age |> as.numeric() |> mean(na.rm=T) |> fmt_APA_numbers(),
    age_sd   = demographics_reduced$Age |> sd(na.rm=T) |> fmt_APA_numbers(),
    age_min  = demographics_reduced$Age |> as.numeric() |> min(na.rm=T) |> fmt_APA_numbers(),
    age_max  = demographics_reduced$Age |> as.numeric() |> max(na.rm=T) |> fmt_APA_numbers(),
    gender_f = sum( demographics_reduced$Gender=="1" )
  )
)

## General sleep    =====
general_sleep_renamed <- 
  sleep_quiz |>
  mutate(
    .before=1,
    night_shift    = S1_Q1,
    pref_sleep     = as.numeric(S1_Q2)/3600,
    pref_sleep     = if_else(pref_sleep<=5, pref_sleep+24, pref_sleep),
    pref_wake      = as.numeric(S1_Q3)/3600,
    pref_sleep_dur = S1_Q4,
    sleep_week     = S1_Q5,
    sleep_week_end = S1_Q6,
    coff_day       = S1_Q7,
    energy_drink   = S1_Q8,
  )

general_sleep <- 
  general_sleep_renamed |>
  pivot_longer(c(
    night_shift, pref_sleep, pref_wake, pref_sleep_dur,
    sleep_week, sleep_week_end, coff_day, energy_drink)
  ) |>
  summarise(
    .by = name,
    m   = mean(value, na.rm=T),
    sd  = sd(value, na.rm=T),
    min = min(value, na.rm=T),
    max = max(value, na.rm=T),
  ) |>
  add_row(name = "preferential_sleep", .before=2) |>
  add_row(name = "actual_sleep", .before=6) 

# PANAS 
panas_pos <- map(c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19), \(x){
  paste0("S6_Q",x)}) |> list_c()
panas_neg <- map(c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20), \(x){
  paste0("S6_Q",x)}) |> list_c()



# Model access     =======
## Continuous model     =====
c <- list( 
  mw  = as.matrix(mod.cont$mw),
  mb  = as.matrix(mod.cont$mb),
  smw = as.matrix(mod.cont$smw),
  bv  = as.matrix(mod.cont$bv),
  ae  = as.matrix(mod.cont$ae)
)

## Reduced model        ======
r <- list( 
  mw  = as.matrix(mod.dich$mw),
  mb  = as.matrix(mod.dich$mb),
  smw = as.matrix(mod.dich$smw),
  bv  = as.matrix(mod.dich$bv),
  ae  = as.matrix(mod.dich$ae)
)


##  Mood        =======
mood <- list()
mood$n <- as.matrix( mod.mood.cont[["bayes"]][["pos"]] )
mood$p <- as.matrix( mod.mood.cont[["bayes"]][["neg"]] )

