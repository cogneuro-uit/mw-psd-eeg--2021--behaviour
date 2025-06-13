tbls[["test_sleep_wake_delay"]] <-
  sleeptimes_updated |>
  mutate(
    condition = case_when(
      pre_control  == 1 ~ "NS",
      pre_sleepdep == 1 ~ "PSD") 
  ) |> 
  filter( pre_control==1 | pre_sleepdep==1 ) |>
  mutate(last_awaking = if_else(is.na(last_awaking), rise_from_bed, last_awaking)) |>
  pivot_longer(c(entered_bed_cum, tried_to_sleep_cum, last_awaking, rise_from_bed)) |>
  mutate(split = case_when( str_ends(name, "_cum") ~ "sleep", T ~ "wake") ) |>
  mutate(
    .by = c(subj, date, split)
    , len = c("start", "end")
  ) |> 
  select(-name) |> 
  pivot_wider(names_from = len, values_from = value) |> 
  summarise(
    .by = c(subj, condition, split)
    , start_NA = sum(is.na(start))
    , start    = mean(start, na.rm=T)
    , end_NA   = sum(is.na(end)) 
    , end      = mean(end, na.rm = T)
  ) |>
  mutate(
    .by = c(condition, split)
    , start_miss = n_distinct(subj[start_NA > 0])
    , end_miss   = n_distinct(subj[start_NA > 0]) 
    , any_miss   = n_distinct(subj[start_NA > 0 | end_NA > 0]) 
  ) |>
  summarise(
    .by = c(condition, split)
    # START
    # , start_n    = unique(start_miss)
    # , start_NA   = sum(start_NA)
    , start_m    = mean(start, na.rm = T)
    , start_sd   = sd(start, na.rm = T)
  
    # END
    # , end_n      = unique(end_miss)
    # , end_NA     = sum(end_NA)
    , end_m      = mean(end, na.rm = T)
    , end_sd     = sd(end, na.rm = T)
    
    # DIFF
    # , diff_n     = length(subj) - unique(any_miss)
    # , diff_n     = unique(if_else(condition == "NS", diff_n+1, diff_n))
    , diff_m     = mean(end - start, na.rm = T)
    , diff_sd    = sd( end - start, na.rm = T)
    , bf         = extractBF( ttestBF(
      start[!is.na(start) & !is.na(end)], 
      end[!is.na(start) & !is.na(end)], paired=T) )[["bf"]]
  ) |>
  mutate( 
    across(is.double, ~fmt_APA_numbers(.x))
    , bf = if_else(bf > 1000, format(bf, scientific = T, digits = 3), as.character(bf))
    , split = if_else(split=="sleep", "Bed-sleep delay", "Wake-rise delay")
  ) |>
  rename(diff_bf = bf) |>
  gt(groupname_col = "split") |>
  tab_spanner("Start time", starts_with("start_")) |>
  tab_spanner("End time", starts_with("end_")) |>
  tab_spanner("Test", starts_with("diff_")) |>
  cols_label(
    ends_with("_n") ~ md("NA~*n*~")
    , ends_with("_NA") ~ md("NA~m~")
    , ends_with("_m") ~ md("*M*")
    , ends_with("_sd") ~ md("*SD*")
    , ends_with("_bf") ~ md("BF~10~")
    , condition = "Condition"
  ) |>
  tab_footnote(
    md("PSD = Partial sleep deprivation, NS = normal sleep, *M* = mean, *SD* = standard deviation,
       BF~10~ = Bayesian factor in favour of the alternative hypothesis.")
  ) |>
  tab_fmt_APA() |>
  cols_align("center", c(-condition))
  
conditional_save(
  tbls[["test_sleep_wake_delay"]]
  , "Test sleep-wake delay"
)

  
  