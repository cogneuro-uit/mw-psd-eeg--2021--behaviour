
probe_tbl_reduced <- 
  bayes_tbl_sum(mod_bay_sleep_cont$mw, apa_table = T)  |>
  bayes_tbl_add_sig() |>
  mutate_bayes_mod_probe() |> 
  rename_with(~paste0("cont_mw_",.x), 3:6) |>
  left_join(
    bayes_tbl_sum(mod_bay_sleep_cont$mb, apa_table = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |> 
      rename_with(~paste0("cont_mb_",.x), 3:6),
    by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(mod_bay_sleep_cont$smw, apa_table = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |> 
      rename_with(~paste0("cont_smw_",.x), 3:6),
    by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(mod_bay_split_sleep$mw, apa_table = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |> 
      rename_with(~paste0("exc_mw_",.x), 3:6),
    by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(mod_bay_split_sleep$mb, apa_table = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |> 
      rename_with(~paste0("exc_mb_",.x), 3:6),
    by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(mod_bay_split_sleep$smw, apa_table = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |> 
      rename_with(~paste0("exc_smw_",.x), 3:6),
    by = c("group", "var") 
  ) |>
  mutate(
    c_mw_p    = if_else(as.numeric(cont_mw_p) >= .95, TRUE, FALSE),
    c_mb_p    = if_else(as.numeric(cont_mb_p) >= .95, TRUE, FALSE),
    c_smw_p   = if_else(as.numeric(cont_smw_p) >= .95, TRUE, FALSE),
    e_mw_p    = if_else(as.numeric(exc_mw_p) >= .95, TRUE, FALSE),
    e_mb_p    = if_else(as.numeric(exc_mb_p) >= .95, TRUE, FALSE),
    e_smw_p    = if_else(as.numeric(exc_smw_p) >= .95, TRUE, FALSE),
    diff_mw   = if_else(c_mw_p  != e_mw_p, TRUE, FALSE),
    diff_mb   = if_else(c_mb_p  != e_mb_p, TRUE, FALSE),
    diff_smw  = if_else(c_smw_p != e_smw_p, TRUE, FALSE),
    across(c(starts_with("c_"), starts_with("e_")), ~NULL),
    mw_e="", mb_e="",smw_e=""
  )


tbls[["dich_probit_model"]] <- 
  probe_tbl_reduced |>
  gt(groupname_col = "group") |>
  tab_spanner("Mind wandering", starts_with("exc_mw_")) |>
  tab_spanner("Mind blanking", starts_with("exc_mb_")) |>
  tab_spanner("Spontaneous mind wandering", starts_with("exc_smw_")) |>
  cols_hide(
    c(starts_with("cont_"), starts_with("diff_"))
  ) |>
  cols_move(starts_with("smw_"), exc_mb_p) |>
  cols_move(starts_with("mb_"),  exc_mw_p) |>
  cols_move(mw_e,  exc_mw_p) |>
  cols_move(mb_e,  exc_mb_p) |>
  cols_move(smw_e, exc_smw_p) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("exc_mw"),diff_mw)
  ) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("exc_mb"),diff_mb)
  ) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("exc_smw"),diff_smw)
  ) |>
  tab_bayes_generics(
    pre_footnote = "Bold rows indicate a difference to the continuous Bayesian model", 
    post_footnote = "BV = behavioural variability, AE = approximate entropy, 
    Pre-positive = Pre-test positive mood, Pre-negative = Pre-test negative mood, 
    PSD = partial sleep deprivation.") 

conditional_save(
  tbls[["dich_probit_model"]]
  , "Dichotomous thought probe models"
)

