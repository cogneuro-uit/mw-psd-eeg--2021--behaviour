#=====          Simple bind           =====
# bind self-report (SR) and actigraphy (AG) 
sleeptimes <- 
  sleep_diary |>
  left_join(actigraphy, by=c("subj","date"="date_end_ag"))


#   Adjusted sleep      =====
## Fixes for adjusted sleep       =====
# fix empty entries & transform to character.
p_l_wak <- map(p_l_wak, \(df){
  if( !is.null( df ) ){
    if( length( df ) != 0 ){
      df |> mutate(wake_zone = as.character(wake_zone))
    }
  } else { 
    tibble() 
  }
}) |> list_rbind()

p_l_on <- map(p_l_on, \(df){
  if( !is.null( df ) ){
    if( nrow( df ) != 0 ){
      df |> mutate(onset_zone = as.character(onset_zone))
    }
  } else { tibble() }
}) |> list_rbind()

# agreement 2 (collective)
p_l_wak2 <- map(p_l_wak2, \(df){
  if( !is.null( df ) ){
    if( nrow( df ) != 0 ){
      df |> mutate(wake_zone = as.character(wake_zone))
    }
  } else { tibble() }
}) |> list_rbind()

p_l_on2 <- map(p_l_on2, \(df){
  if( !is.null( df ) ){
    if( nrow( df ) != 0 ){
      df |> mutate(onset_zone = as.character(onset_zone))
    }
  } else { tibble() }
}) |> list_rbind()



### Round 1      ======
#' sleep adjustment after the first round 
sleep_adjustment_r1 <- 
  sleeptimes |>
  left_join(
    p_l_wak |> 
      select(subj, date, wake_time_adj, wake_folleso_adj) |>
      rename(wake_sa = wake_folleso_adj)
    ,  by = c("subj","date")
  ) |>
  left_join(
    p_l_on |> 
      select(subj, date, onset_time_adj, onset_folleso_adj) |>
      rename(onset_sa=onset_folleso_adj)
    , by = c("subj","date")
  ) |> 
  left_join(
    time_adj |> 
      filter(!is.na(subj)) |>
      pivot_wider(names_from=wake_or_onset, values_from = time_adj, 
                  names_prefix = "gc_") |>
      rename(wake_gc = gc_Wake, onset_gc = gc_Onset)
    , by = c("subj", "date")
  )

# Check these participants after round 1:
check_sleep_adjustment_r1 <-
  sleep_adjustment_r1 |>
  pivot_longer( c(wake_gc, onset_gc, wake_sa, onset_sa) ) |>
  separate_wider_delim(name, "_", names = c("cond", "eval"), too_many = "drop") |>
  mutate(eval = if_else(eval=="gc", "GC", "SA")) |>
  filter(!is.na(value)) |>
  pivot_wider(names_from = eval, values_from = value) |>
  mutate(
    .before=1, 
    diff = GC - SA, 
    check = abs(diff) > .25
  ) |> 
  filter(check) |>
  select(subj, date, cond, diff, )


### Round 2   ======
#' Sleep adjustment after round 2
sleep_adjustment_r2 <- 
  sleeptimes |>
  #' Add updated wake times (including SA's entries)
  left_join(
    p_l_wak2 |> 
      select(subj, date, wake_folleso_adj) |>
      rename(wake_sa_MORE = wake_folleso_adj),
    by = c("subj", "date") 
  ) |>
  #' Add updated onset times (including SA's entries)
  left_join(
    p_l_on2 |> 
      select(subj, date, onset_folleso_adj) |>
      rename(onset_sa_MORE = onset_folleso_adj),
    by = c("subj","date")
  ) |> 
  #' Add GC's onset/wake times: 
  left_join(
    time_adj |>
      pivot_wider(names_from = wake_or_onset, 
                  values_from = time_adj) |>
      rename(wake_GC = Wake, onset_GC = Onset) |>
      filter(!is.na(subj)) |>
      select(subj, date, wake_GC, onset_GC),
    by = c("subj", "date") 
  ) |>
  #' Add agreed condition
  left_join( 
    check_sleep_adjustment_r1 |>
      mutate(use_this = T) |>
      select(-diff) |>
      pivot_wider(names_from = cond, 
                  values_from = use_this, names_prefix = "use_"),
    by = c("subj", "date") )


sleeptimes_updated <- 
  sleep_adjustment_r2 |>
  #' Rename Self-report (SR) and actigraphy (AG) for better overview
  rename(
    duration_SR  = sleep_duration, 
    onset_SR     = sleep_time_cum,
    wake_SR      = last_awaking_fix,
    onset_AG     = sleep_onset_ag_h,
    wake_AG      = sleep_wake_ag_h, 
    duration_AG  = sleep_duration_ag_h,
  ) |>
  mutate(
    #' The disagreement cases are resolved and stored in wake_sa_MORE (hence MORE).
    #' These cases must be added to GC's data. Otherwise, they remain GC's. 
    #'  
    #' Fill false for use_wake/onset variable (the cases that were in disagreement). 
    use_wake = if_else(is.na(use_wake), FALSE, use_wake),
    use_onset = if_else(is.na(use_onset), FALSE, use_onset),
    #' Add SA sleep adjustment (INCLUDING FINAL AGREEMENT)
    wake_SA = wake_sa_MORE,
    onset_SA = onset_sa_MORE,
    #' Add GC sleep adjustment (INCLUDING FINAL AGREEMENT)
    wake_GC = if_else(use_wake, wake_sa_MORE, wake_GC),
    onset_GC = if_else(use_onset, onset_sa_MORE, onset_GC),
    
    #' For the below, we make a single variable related to the "participants" sleep times.
    #' These are added as the mean between the two, or if missing, as either one:
    #' Add a variable related to th
    wake_PAR  = case_when(
      !is.na(wake_SR) & !is.na(wake_AG) ~ (wake_SR + wake_AG) / 2,
      !is.na(wake_SR) & is.na(wake_AG)  ~ wake_SR,
      is.na(wake_SR)  & !is.na(wake_AG) ~ wake_AG,
    ),
    onset_PAR = case_when(
      !is.na(onset_SR) & !is.na(onset_AG) ~ (onset_SR + onset_AG) / 2,
      !is.na(onset_SR) & is.na(onset_AG)  ~ onset_SR,
      is.na(onset_SR)  & !is.na(onset_AG) ~ onset_AG,
    ),
    
    #' Add a final mean between the researchers variables
    wake_RES  = (wake_SA + wake_GC) / 2,
    onset_RES = (onset_SA + onset_GC) / 2,
    
    #' An absolute final variable, related to the researchers agreed sleep adjustments,
    #' but with added SR/AG data where missing. 
    #' 
    wake_final = case_when( 
      !is.na(wake_RES)  ~ wake_RES,
      T ~ wake_PAR
    ), 
    onset_final = case_when( 
      !is.na(onset_RES)  ~ onset_RES,
      T ~ onset_PAR),
    
    # Duration
    duration_final = wake_final - (onset_final - 24),
    duration_SA = wake_SA - (onset_SA - 24),
    duration_GC = wake_GC - (onset_GC - 24),
  ) |>
  mutate(
    #' Case 17 receive special treatment as there seems to be something wrong (?) with the AG 
    wake_final = if_else(subj=="017" & pre_sleepdep==1, wake_SR, wake_final),
    onset_final = if_else(subj=="017" & pre_sleepdep==1, onset_SR, onset_final),
    duration_final = if_else(subj=="017" & pre_sleepdep==1, duration_SR, duration_final),
  )



### Selected sleep times          =====
#' Here we calculated the sleep difference between the conditions, as well as
#' prepare the data for connecting to the behaviour.
sleeptimes_updated_trans <- 
  sleeptimes_updated |>
  mutate(
    condition = case_when(
      pre_control  == 1 ~ "control", 
      pre_sleepdep == 1 ~ "SD",
      T ~ NA)
  ) |> 
  filter(!is.na(condition)) |>
  select(
    subj, condition, 
    # SR 
    wake_SR, onset_SR, duration_SR,
    # AG
    wake_AG, onset_AG, duration_AG,
    # Adjusted
    onset_final, wake_final, duration_final
  ) |> 
  pivot_longer(c(
    wake_SR, onset_SR, duration_SR,
    wake_AG, onset_AG, duration_AG,
    onset_final, wake_final, duration_final)
  ) |>
  summarise(
    .by = c(subj, condition, name),
    m = mean(value, na.rm=T) # small thing this
  ) |> 
  mutate(report  = case_when(
    str_ends(name, "_SR") ~ "Self.report",
    str_ends(name, "_AG") ~ "Actigraphy",
    str_ends(name, "_final") ~ "Adjusted",
  ), 
  name = case_when(
    str_starts(name, "onset") ~ "Onset",
    str_starts(name, "wake") ~ "Wake",
    str_starts(name, "duration") ~ "Duration",
    ),
  ) |>
  rename(sleep = name) |>
  pivot_wider(names_from = c(condition, report, sleep), values_from = m) |>
  mutate(
    # Self reported duration
    SD_Self.report_Duration.diff      = SD_Self.report_Duration - control_Self.report_Duration,
    control_Self.report_Duration.diff = SD_Self.report_Duration.diff,
    # Self reported duration
    SD_Actigraphy_Duration.diff       = SD_Actigraphy_Duration - control_Actigraphy_Duration,
    control_Actigraphy_Duration.diff  = SD_Actigraphy_Duration.diff,
    # Adjusted duration
    SD_Adjusted_Duration.diff         = SD_Adjusted_Duration - control_Adjusted_Duration,
    control_Adjusted_Duration.diff    = SD_Adjusted_Duration.diff
  ) |>
  pivot_longer(c(starts_with("SD"),starts_with("control")),
               names_to = c("sleepdep", "report","type"), names_sep = "_") |>
  pivot_wider(names_from = c("report", "type"), values_from = "value") |>
  mutate(
    Self.report_Duration.diff.pos   = Self.report_Duration.diff*-1,
    Actigraphy_Duration.diff.pos    = Actigraphy_Duration.diff*-1,
    Adjusted_Duration.diff.pos      = Adjusted_Duration.diff*-1,
    c.Self.report_Duration.diff.pos = if_else(sleepdep=="SD", Self.report_Duration.diff.pos, 0),
    c.Actigraphy_Duration.diff.pos  = if_else(sleepdep=="SD", Actigraphy_Duration.diff.pos, 0),
    c.Adjusted_Duration.diff.pos    = if_else(sleepdep=="SD", Adjusted_Duration.diff.pos, 0),
  )
  


# Add sleep to the behavioural data =====
data.probe.mood.sleep <- 
  data.probe.mood |>
  left_join(
    sleeptimes_updated_trans,
    by = c("subj", "sleepdep")
  ) 

print( "PREPROCESS DONE! ")
    
