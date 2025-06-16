behav_tbl <- 
  bayes_tbl_sum(mod_bay_sleep_cont$bv, add_sigma = T, fmt_md=T,
                add_loo = T,add_R2 = T, apa_table = T) |>
  bayes_tbl_add_sig() |>
  mutate_bayes_mod_beh() |>
  rename_with(~paste0("cont_bv_", .x), 3:6) |>
  left_join(
    bayes_tbl_sum(mod_bay_sleep_cont$ae, add_sigma = T, fmt_md=T,
                  add_loo = T,add_R2 = T, apa_table = T) |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |>
      rename_with(~paste0("cont_ae_", .x), 3:6)
  ) |>
  # split
  left_join(
    bayes_tbl_sum(mod_bay_split_sleep$bv, add_sigma = T, fmt_md=T,
                  add_loo = T,add_R2 = T, apa_table = T) |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("exc_bv_", .x), 3:6)
  ) |>
  left_join(
    bayes_tbl_sum(mod_bay_split_sleep$ae, add_sigma = T,fmt_md=T,
                  add_loo = T,add_R2 = T, apa_table = T) |>
      mutate_bayes_mod_beh() |>
      rename_with(~paste0("exc_ae_", .x), 3:6)
  )  |>
  mutate(
    c_ae_p   = if_else(as.numeric(cont_ae_p) >= .95, TRUE, FALSE),
    c_bv_p   = if_else(as.numeric(cont_bv_p) >= .95, TRUE, FALSE),
    e_ae_p   = if_else(as.numeric(exc_ae_p) >= .95, TRUE, FALSE),
    e_bv_p   = if_else(as.numeric(exc_bv_p) >= .95, TRUE, FALSE),
    diff_bv  = if_else(c_bv_p != e_bv_p, TRUE, FALSE),
    diff_ae  = if_else(c_ae_p != e_ae_p, TRUE, FALSE),
    e = ""
  ) 


tbls[["dich_behav_models"]] <- 
  # Continuous
  bayes_tbl_sum(mod_bay_sleep_cont$bv, apa_table = T) |>
  bayes_tbl_add_sig() |>
  mutate_bayes_mod_beh() |>
  rename_with(~paste0("cont_bv_", .x), 3:6) |>
  left_join(
    bayes_tbl_sum(mod_bay_sleep_cont$ae, apa_table = T) |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |>
      rename_with(~paste0("cont_ae_", .x), 3:6)
  ) |>
  # split
  left_join(
    bayes_tbl_sum(mod_bay_split_sleep$bv, apa_table = T) |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |>
      rename_with(~paste0("exc_bv_", .x), 3:6)
  ) |>
  left_join(
    bayes_tbl_sum(mod_bay_split_sleep$ae, apa_table = T) |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |>
      rename_with(~paste0("exc_ae_", .x), 3:6)
  )  |>
  mutate(
    c_ae_p   = if_else(as.numeric(cont_ae_p) >= .95, TRUE, FALSE),
    c_bv_p   = if_else(as.numeric(cont_bv_p) >= .95, TRUE, FALSE),
    e_ae_p   = if_else(as.numeric(exc_ae_p) >= .95, TRUE, FALSE),
    e_bv_p   = if_else(as.numeric(exc_bv_p) >= .95, TRUE, FALSE),
    diff_bv  = if_else(c_bv_p != e_bv_p, TRUE, FALSE),
    diff_ae  = if_else(c_ae_p != e_ae_p, TRUE, FALSE),
    e = ""
  ) |> 
  gt(groupname_col = "group") |>
  tab_spanner("Behavioural variability", matches("_bv_")) |>
  tab_spanner("Approximate Entropy", matches("_ae_")) |>
  cols_hide(c(starts_with("cont"), starts_with("c_"), starts_with("e_"))) |>
  cols_move(e, exc_bv_p) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("exc_bv"),diff_bv)
  ) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("exc_ae"),diff_ae)
  ) |>
  cols_hide(starts_with("diff")) |>
  tab_bayes_generics(pre_footnote = "Bold rows indicate a difference to the continuous Bayesian model", 
                     post_footnote = "Pre-positive = Pre-test positive mood, Pre-negative = Pre-test negative mood, PSD = partial sleep deprivation.") 

conditional_save(
  tbls[["dich_behav_models"]]
  , "Dichotomous behaviour models"
)
