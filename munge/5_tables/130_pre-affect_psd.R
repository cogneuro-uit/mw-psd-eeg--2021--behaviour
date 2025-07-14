tbls[["psd_on_pre-task_affect"]] <- 
  bayes_tbl_sum(mod.mood.cont$pos$psd, apa_table = T, add_loo_R2 = T) |>
  bayes_tbl_add_sig() |>
  rename_with(~paste0("cont_pos_",.x), c(-group, -var)) |>
  left_join(
    bayes_tbl_sum(mod.mood.cont$neg$psd, apa_table = T, add_loo_R2 = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("cont_neg_",.x), c(-group, -var))
    , by = c("group", "var")
  ) |>
  mutate(var = if_else(str_starts(var, "c.Adjusted"), "sleepdepSD", var)) |>
  left_join(
    bayes_tbl_sum(mod.mood.dich$pos$psd, apa_table = T, add_loo_R2 = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("dich_pos_",.x), c(-group, -var))
    , by = c("group", "var")
  ) |>
  left_join(
    bayes_tbl_sum(mod.mood.dich$neg$psd, apa_table = T, add_loo_R2 = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("dich_neg_",.x), c(-group, -var))
    , by = c("group", "var")
  ) |> 
  mutate(
    pos_e="", neg_e="", e = ""
    , var = if_else(var=="sleepdepSD", "PSD", var)
    , pos_diff  = if_else( 
      (( cont_pos_p |> str_remove("[<>=]") |> as.numeric() ) >= .95 ) != 
        (( dich_pos_p |> str_remove("[<>=]") |> as.numeric() ) >= .95 ), TRUE, FALSE)
    , neg_diff  = if_else( 
      (( cont_neg_p |> str_remove("[<>=]") |> as.numeric() ) >= .95 ) != 
        (( dich_neg_p |> str_remove("[<>=]") |> as.numeric() ) >= .95 ), TRUE, FALSE)
  ) |>
  gt(groupname_col = "group") |>
  tab_spanner("Continuous PSD", starts_with("cont_")) |>
  tab_spanner("Dichotomous PSD", starts_with("dich_")) |>
  tab_spanner("Negative affect", matches("neg_")) |>
  tab_spanner("Positive affect", matches("pos_")) |>
  cols_move(contains("_pos_"), var) |>
  cols_move("e", dich_pos_p) |>
  cols_move("pos_e", cont_pos_p) |>
  cols_move("neg_e", cont_neg_p) |>
  tab_bayes_generics(
    pre_footnote = "Continuous partial sleep deprivation (PSD) refers to how the 
    PSD variable was treated, i.e., as a continuous variable. While under the dichotomous PSD, 
    the variable was treated as a dichotomous (true/false) -- pertaining to fewer participants (*n* = 23)."
    # Bold rows indicate a difference between the models -- no diff, no add.
    , post_footnote = "MW = mind wandering."
  ) |>
  cols_hide(ends_with("_diff")) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("_pos_"), pos_diff)
  ) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("_neg_"), neg_diff)
  ) 

conditional_save(
  tbls[["psd_on_pre-task_affect"]]
  , "Sleep deprivation on pre-task affect--both models"
)
