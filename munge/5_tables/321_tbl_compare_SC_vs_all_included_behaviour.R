behav_tbl_reduced <-
  bayes_tbl_sum(mod.dich$bv, apa_table = T, add_loo_R2 = T) |>
  bayes_tbl_add_sig() |>
  mutate_bayes_mod_beh() |>
  rename_with(~paste0("dich_bv_", .x), 3:6) |>
  left_join(
    bayes_tbl_sum(mod.dich$ae, apa_table = T, add_loo_R2 = T) |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |>
      rename_with(~paste0("dich_ae_", .x), 3:6),
    by = c("group", "var")
  ) |>
  left_join(
    bayes_tbl_sum(mod.all_pars$bv, apa_table = T, add_loo_R2 = T) |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |>
      rename_with(~paste0("all_bv_", .x), 3:6),
    by = c("group", "var")
  ) |>
  left_join(
    bayes_tbl_sum(mod.all_pars$ae, apa_table = T, add_loo_R2 = T) |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |>
      rename_with(~paste0("all_ae_", .x), 3:6),
    by = c("group", "var")
  ) |>
  mutate(
    d_bv_p  = if_else(as.numeric(dich_bv_p) >= .95, TRUE, FALSE),
    d_ae_p  = if_else(as.numeric(dich_ae_p) >= .95, TRUE, FALSE),
    a_bv_p  = if_else(as.numeric(all_bv_p) >= .95, TRUE, FALSE),
    a_ae_p  = if_else(as.numeric(all_ae_p) >= .95, TRUE, FALSE),
    diff_bv = if_else(d_bv_p != a_bv_p, TRUE, FALSE),
    diff_ae = if_else(d_ae_p != a_ae_p, TRUE, FALSE),
    across(c(starts_with("d_"), starts_with("a_")), ~NULL),
    e = ""
  )

tbls[["all_pars_behav_models"]] <-
  behav_tbl_reduced |>
  gt(groupname_col = "group") |>
  tab_spanner("SC",               starts_with("dich_bv_"), id = "sp_dich_bv") |>
  tab_spanner("All participants", starts_with("all_bv_"),  id = "sp_all_bv")  |>
  tab_spanner("SC",               starts_with("dich_ae_"), id = "sp_dich_ae") |>
  tab_spanner("All participants", starts_with("all_ae_"),  id = "sp_all_ae")  |>
  tab_spanner("Behavioural variability", spanners = c("sp_dich_bv", "sp_all_bv")) |>
  tab_spanner("Approximate Entropy",     spanners = c("sp_dich_ae", "sp_all_ae")) |>
  cols_hide(starts_with("diff_")) |>
  cols_move(starts_with("all_bv_"),  after = dich_bv_p) |>
  cols_move(e,                       after = all_bv_p)  |>
  cols_move(starts_with("dich_ae_"), after = e)         |>
  cols_move(starts_with("all_ae_"),  after = dich_ae_p) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(c(matches("dich_bv"), matches("all_bv")), diff_bv)
  ) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(c(matches("dich_ae"), matches("all_ae")), diff_ae)
  ) |>
  tab_bayes_generics(
    pre_footnote = "Bold rows indicate a difference between the SC and all-participants models.",
    post_footnote = "SC = stringent compliance. Pre-positive = Pre-test positive affect,
    Pre-negative = Pre-test negative affect, PSD = partial sleep deprivation.")

conditional_save(
  tbls[["all_pars_behav_models"]]
  , "All participants behaviour models"
)
