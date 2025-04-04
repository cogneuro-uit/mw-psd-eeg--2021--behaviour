# PANAS 
panas_pos <- map(c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19), \(x){
  paste0("S6_Q",x)}) |> list_c()
panas_neg <- map(c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20), \(x){
  paste0("S6_Q",x)}) |> list_c()


#' General sleep summary 
general_sleep <- 
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
  ) |>
  pivot_longer(c(
    night_shift, pref_sleep, pref_wake, pref_sleep_dur,
    sleep_week, sleep_week_end, coff_day, energy_drink)
  ) |>
  summarise(
    .by = name,
    m = mean(value, na.rm=T),
    sd = sd(value, na.rm=T),
    min = min(value, na.rm=T),
    max = max(value, na.rm=T),
  ) |>
  add_row(name = "preferential_sleep", .before=2) |>
  add_row(name = "actual_sleep", .before=6) 


#' Transform variables to calculate various scales:
sleep_quiz_trans <- 
  sleep_quiz |>
  rowwise() |>
  # Sleep efficacy calculation (for pittsburgh)
  mutate(c5a = sum(c_across(S4_Q12:S4_Q21)) - 9 ) |>
  ungroup() |>
  mutate(
    ### Pittsburgh calc
    # C1: Overall sleep quality
    c1 = S4_Q22-1,
    # C2: Problems falling asleep
    c2a = case_when(
      S4_Q9 <= 15 ~ 0,
      S4_Q9 <= 30 ~ 1,
      S4_Q9 <= 60 ~ 2,
      S4_Q9 >  60 ~ 3),
    c2b = c2a + (S4_Q12-1),
    c2 = case_when(
      c2b == 0 ~ 0,
      c2b <= 2 ~ 1,
      c2b <= 4 ~ 2,
      c2b <= 6 ~ 3),
    # C3: Hw many hours of sleep?
    c3 = case_when(
      S4_Q11 >= hours(7) ~ 0,
      S4_Q11 >= hours(6) ~ 1,
      S4_Q11 >= hours(5) ~ 2,
      S4_Q11 < hours(5) ~ 3),
    # C4: Sleep efficiency
    c4a = ( (as.numeric(S4_Q11)/3600) / 
              ( (as.numeric(S4_Q10) / 3600) + 
                  (24 - as.numeric(S4_Q8) / 3600) ) ) * 100,
    c4 = case_when(
      c4a >= 85 ~ 0,
      c4a >= 75 ~ 1,
      c4a >= 65 ~ 2,
      c4a <  65 ~ 3,
    ),
    # C5: Sleep problems:
    c5 = case_when(
      c5a == 0 ~ 0,
      c5a >= 1 ~ 1,
      c5a >= 10 ~ 2,
      c5a >= 19 ~ 3),
    # C6: Medicine
    c6 = S4_Q23 - 1,
    # C7: Problems keeping awake
    c7a = S4_Q24 + S4_Q25 - 2,
    c7 = case_when(
      c7a == 0 ~ 0,
      c7a <= 2 ~ 1,
      c7a <= 4 ~ 2,
      c7a <= 6 ~ 3),
    
    # Reverse scores: 
    S5_Q1 = 5 - S5_Q1,
    S5_Q3 = 5 - S5_Q3,
    S5_Q6 = 5 - S5_Q6,
    S5_Q7 = 5 - S5_Q7,
  )  |>
  select(-c2a, -c2b, -c4a, -c7a)


#' Summarize the scales
sleep_quiz_summary <- 
  sleep_quiz_trans |>
  rowwise() |>
  mutate(
    .before = 1,
    fatigue     = mean(c_across(starts_with("S2_"))),
    #' Average all responses (S2.1:S2.8)
    sleepiness  = sum(c_across(starts_with("S3_"))),
    #' Sum all responses. Sleepiness:
    #' 0-5 : low; 6-10 : normal; 11-12 : mild
    #' 13-15 : Moderate; 16-24 : severe
    insomnia    = sum(c_across(S4_Q1:S4_Q7), na.rm = T),
    #' Sum all responses (S4.1:S4.7)
    #' 0-7: no insomnia; 8-14: subthreshold; 
    #' 15-21: moderate; 22-28: severe
    pittsburgh  = sum(c_across(c1:c7)),
    #' Sum all C columns
    #' Higher scores indicate worse sleep quality.
    dinural_avg = mean(c_across(S5_Q1:S5_Q7)),
    dinural_sum = sum(c_across(S5_Q1:S5_Q7)),
    #' Average or sum all responses
    #' *LOWER* values indicate *NIGHT preference* (night owl)
    #' *HIGHER* values indicate *EARLY preference* (early bird) 
    panas_neg   = sum(c_across(panas_neg), na.rm = T),
    panas_pos   = sum(c_across(panas_pos), na.rm = T),
    alcohol     = sum(c_across(S7_Q1:S7_Q3), na.rm = T)
  ) |>
  ungroup() |> 
  pivot_longer(c(fatigue, sleepiness, insomnia, pittsburgh, 
                 dinural_avg, panas_pos, panas_neg, alcohol)) |>
  summarise(
    .by = name, 
    m   = mean(value, na.rm=T),
    sd  = sd(value,   na.rm=T),
    min = min(value,  na.rm=T),
    max = max(value,  na.rm=T)
  ) |>
  add_row(name = "PANAS", .before=6)

rm(panas_neg)
rm(panas_pos)