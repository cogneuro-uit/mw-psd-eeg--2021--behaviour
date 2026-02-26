# PROBES setup          ======
probe_tbl_cont <- 
  bayes_tbl_sum(order_effects$mw, apa_table = T, add_loo_R2 = T)  |>
  bayes_tbl_add_sig() |>
  rename_with( ~paste0("mw_",.x), 3:6) |>
  left_join(
    bayes_tbl_sum(order_effects$mb, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("mb_",.x), 3:6)
    , by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(order_effects$smw, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("smw_",.x), 3:6)
    , by = c("group", "var") 
  ) |>
  mutate(
    var = str_replace_all(var, "c.Adjusted_Duration.diff.pos", "PSD") |> 
      str_replace_all("sessionS2", "S2") |> 
      str_replace_all(":", " x ")
  )

## gt table         =======
tbls[["order_effects_on_probes"]] <- 
  probe_tbl_cont |>
  gt(groupname_col = "group") |>
  tab_spanner("Mind wandering", starts_with("mw_")) |>
  tab_spanner("Mind blanking", starts_with("mb_")) |>
  tab_spanner("Spontaneous mind wandering", starts_with("smw_")) |>
  tab_bayes_generics()

## conditional save       ======
conditional_save(
  tbls[["order_effects_on_probes"]]
  , "Order effects on thought probes"
)



# BEHAVIOUR setup     ======
behav_tbl_cont <- 
  bayes_tbl_sum(order_effects$bv, apa_table = T, add_loo_R2 = T)  |>
  bayes_tbl_add_sig() |>
  rename_with( ~paste0("bv_",.x), 3:6) |>
  left_join(
    bayes_tbl_sum(order_effects$ae, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("ae_",.x), 3:6)
    , by = c("group", "var") 
  ) |>
  mutate(
    var = str_replace_all(var, "c.Adjusted_Duration.diff.pos", "PSD") |> 
      str_replace_all("sessionS2", "S2") |> 
      str_replace_all(":", " x ") |>
      str_replace_all("probenum_prop", "Trial")
  )

## Gt table         ======
tbls[["order_effects_on_behav"]] <-
  behav_tbl_cont |>
  gt(groupname_col = "group") |>
  tab_spanner("Behavioural variability", matches("bv_")) |>
  tab_spanner("Approximate Entropy", matches("ae_")) |>
  tab_bayes_generics()


## conditional save       ======
conditional_save(
  tbls[["order_effects_on_behav"]]
  , "Order effects on behaviour"
) 
