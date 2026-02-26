probe_tbl_reduced <-
  bayes_tbl_sum(mod.dich$mw, apa_table = T, add_loo_R2 = T)  |>
  bayes_tbl_add_sig() |>
  mutate_bayes_mod_probe() |>
  rename_with(~paste0("dich_mw_",.x), 3:6) |>
  left_join(
    bayes_tbl_sum(mod.dich$mb, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |>
      rename_with(~paste0("dich_mb_",.x), 3:6),
    by = c("group", "var")
  ) |>
  left_join(
    bayes_tbl_sum(mod.dich$smw, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |>
      rename_with(~paste0("dich_smw_",.x), 3:6),
    by = c("group", "var")
  ) |>
  left_join(
    bayes_tbl_sum(mod.all_pars$mw, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |>
      rename_with(~paste0("all_mw_",.x), 3:6),
    by = c("group", "var")
  ) |>
  left_join(
    bayes_tbl_sum(mod.all_pars$mb, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |>
      rename_with(~paste0("all_mb_",.x), 3:6),
    by = c("group", "var")
  ) |>
  left_join(
    bayes_tbl_sum(mod.all_pars$smw, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |>
      rename_with(~paste0("all_smw_",.x), 3:6),
    by = c("group", "var")
  ) |>
  mutate(
    d_mw_p    = if_else(as.numeric(dich_mw_p) >= .95, TRUE, FALSE),
    d_mb_p    = if_else(as.numeric(dich_mb_p) >= .95, TRUE, FALSE),
    d_smw_p   = if_else(as.numeric(dich_smw_p) >= .95, TRUE, FALSE),
    a_mw_p    = if_else(as.numeric(all_mw_p) >= .95, TRUE, FALSE),
    a_mb_p    = if_else(as.numeric(all_mb_p) >= .95, TRUE, FALSE),
    a_smw_p   = if_else(as.numeric(all_smw_p) >= .95, TRUE, FALSE),
    diff_mw   = if_else(d_mw_p  != a_mw_p, TRUE, FALSE),
    diff_mb   = if_else(d_mb_p  != a_mb_p, TRUE, FALSE),
    diff_smw  = if_else(d_smw_p != a_smw_p, TRUE, FALSE),
    across(c(starts_with("d_"), starts_with("a_")), ~NULL),
    mw_e="", mb_e="",smw_e=""
  )

tbls[["all_pars_probit_model"]] <-
  probe_tbl_reduced |>
  gt(groupname_col = "group") |>
  tab_spanner("SC",               starts_with("dich_mw_"),  id = "sp_dich_mw") |>
  tab_spanner("All participants", starts_with("all_mw_"),   id = "sp_all_mw")  |>
  tab_spanner("SC",               starts_with("dich_mb_"),  id = "sp_dich_mb") |>
  tab_spanner("All participants", starts_with("all_mb_"),   id = "sp_all_mb")  |>
  tab_spanner("SC",               starts_with("dich_smw_"), id = "sp_dich_smw") |>
  tab_spanner("All participants", starts_with("all_smw_"),  id = "sp_all_smw")  |>
  tab_spanner("Mind wandering",           spanners = c("sp_dich_mw",  "sp_all_mw"))  |>
  tab_spanner("Mind blanking",            spanners = c("sp_dich_mb",  "sp_all_mb"))  |>
  tab_spanner("Spontaneous mind wandering", spanners = c("sp_dich_smw", "sp_all_smw")) |>
  cols_hide(starts_with("diff_")) |>
  cols_move(starts_with("all_mw_"),  after = dich_mw_p) |>
  cols_move(mw_e,                    after = all_mw_p)  |>
  cols_move(starts_with("dich_mb_"), after = mw_e)      |>
  cols_move(starts_with("all_mb_"),  after = dich_mb_p) |>
  cols_move(mb_e,                    after = all_mb_p)  |>
  cols_move(starts_with("dich_smw_"), after = mb_e)      |>
  cols_move(starts_with("all_smw_"),  after = dich_smw_p) |>
  cols_move(smw_e,                    after = all_smw_p)  |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(c(matches("dich_mw"), matches("all_mw")), diff_mw)
  ) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(c(matches("dich_mb"), matches("all_mb")), diff_mb)
  ) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(c(matches("dich_smw"), matches("all_smw")), diff_smw)
  ) |>
  tab_bayes_generics(
    pre_footnote = "Bold rows indicate a difference between the SC and all-participants models.",
    post_footnote = "SC = stringent compliance. BV = behavioural variability, AE = approximate entropy,
    Pre-positive = Pre-test positive affect, Pre-negative = Pre-test negative affect,
    PSD = partial sleep deprivation.")

conditional_save(
  tbls[["all_pars_probit_model"]]
  , "All participants probit model"
)

