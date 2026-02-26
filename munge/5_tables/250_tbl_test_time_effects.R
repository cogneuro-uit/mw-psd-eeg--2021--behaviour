# Simple test table         ======
tbls[["effect_of_test_time"]] <- 
  effect_of_test_time |>
  gt() |>
  tab_spanner("09:00", ends_with("_9")) |>
  tab_spanner("11:00", ends_with("_11")) |>
  tab_spanner("Welch t-test", c(t,df,p)) |>
  cols_label(
    starts_with("m_") ~ md("*M*")
    , starts_with("sd_") ~ md("*SD*")
    , diff = md("*M*~diff~")
    , t = md("*t*"), p = md("*p*"), bf = md("BF~10~")
  )


conditional_save(
  tbls[["effect_of_test_time"]]
  , "Effect of test time"
)


# brms model  table       ======
# PROBES setup          ======
tbl_test_time_probes <- 
  bayes_tbl_sum(test_time$mw, apa_table = T, add_loo_R2 = T)  |>
  bayes_tbl_add_sig() |>
  rename_with( ~paste0("mw_",.x), 3:6) |>
  left_join(
    bayes_tbl_sum(test_time$mb, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("mb_",.x), 3:6)
    , by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(test_time$smw, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("smw_",.x), 3:6)
    , by = c("group", "var") 
  ) |>
  mutate(
    var = str_replace_all(var, "c.Adjusted_Duration.diff.pos", "PSD") |> 
      str_replace_all("test_time11", "11_00") |>
      str_replace_all(":", " x ") |>
      str_replace_all("_", ":")
  )

## gt table         =======
tbls[["test_time_on_probes"]] <- 
  tbl_test_time_probes |>
  gt(groupname_col = "group") |>
  tab_spanner("Mind wandering", starts_with("mw_")) |>
  tab_spanner("Mind blanking", starts_with("mb_")) |>
  tab_spanner("Spontaneous mind wandering", starts_with("smw_")) |>
  tab_bayes_generics() |>
  tab_header("", md("*The Effect of Order on Thought Probes*"))

## conditional save       ======
conditional_save(
  tbls[["test_time_on_probes"]]
  , "Test time on thought probes"
)



# BEHAVIOUR setup     ======
tbl_test_time_behav <- 
  bayes_tbl_sum(test_time$bv, apa_table = T, add_loo_R2 = T)  |>
  bayes_tbl_add_sig() |>
  rename_with( ~paste0("bv_",.x), 3:6) |>
  left_join(
    bayes_tbl_sum(test_time$ae, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("ae_",.x), 3:6)
    , by = c("group", "var") 
  ) |>
  mutate(
    var = str_replace_all(var, "c.Adjusted_Duration.diff.pos", "PSD") |> 
      str_replace_all("test_time11", "11_00") |>
      str_replace_all(":", " x ") |>
      str_replace_all("_", ":")
  )

## Gt table         ======
tbls[["test_time_on_behav"]] <-
  tbl_test_time_behav |>
  gt(groupname_col = "group") |>
  tab_spanner("Behavioural variability", matches("bv_")) |>
  tab_spanner("Approximate Entropy", matches("ae_")) |>
  tab_bayes_generics() |>
  tab_header("", md("*Order Effect on Behaviour*"))

## conditional save       ======
conditional_save(
  tbls[["test_time_on_behav"]]
  , "Test time on behaviour"
) 

