bv_tbl_cont <- 
  bayes_tbl_sum(mod.dich$bv, apa_table = T, add_loo_R2 = T)  |>
  bayes_tbl_add_sig() |>
  mutate_bayes_mod_beh() |> 
  rename_with( ~paste0("exc_", .x), 3:6 ) |>
  left_join(
    bayes_tbl_sum(mod.cont$bv, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |> 
      rename_with( ~paste0("cont_adj_", .x), 3:6 )
    , by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(mod.cont_ag$bv, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |> 
      rename_with( ~paste0("cont_ag_", .x), 3:6)
    , by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(mod.cont_sr$bv, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |> 
      rename_with( ~paste0("cont_sr_", .x), 3:6)
    , by = c("group", "var") 
  ) |>
  mutate(
    cond_exc_p        = if_else(as.numeric(exc_p) >= .95, TRUE, FALSE)
    , cond_cont_adj_p = if_else(as.numeric(cont_adj_p) >= .95, TRUE, FALSE)
    , cond_cont_ag_p  = if_else(as.numeric(cont_ag_p) >= .95, TRUE, FALSE)
    , cond_cont_sr_p  = if_else(as.numeric(cont_sr_p) >= .95, TRUE, FALSE)
    , adj_vs_exc      = if_else(cond_cont_adj_p  != cond_exc_p, TRUE, FALSE)
    , adj_vs_ag       = if_else(cond_cont_adj_p  != cond_cont_ag_p, TRUE, FALSE)
    , adj_vs_sr       = if_else(cond_cont_adj_p  != cond_cont_sr_p, TRUE, FALSE)
    , across(c(starts_with("c_"), starts_with("e_")), ~NULL)
    , .e_exc="", .e_cont_adj="", .e_cont_ag="", .e=""
  )


tbls[["BV_predictors_across_sleep_variables"]] <-
  bv_tbl_cont |>
  gt(groupname_col = "group") |>
  # tab_spanner("Dichotomous", starts_with("exc_")) |>
  tab_spanner("Strict Compliance", starts_with("exc_")) |>
  tab_spanner("Adjusted PSD", starts_with("cont_adj")) |>
  tab_spanner("Actigraphy PSD", starts_with("cont_ag")) |>
  tab_spanner("Self-report PSD", starts_with("cont_sr")) |>
  tab_spanner("Dose-response", matches("cont_")) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("exc_"), adj_vs_exc)
  ) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("cont_ag"), adj_vs_ag)
  ) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("cont_sr"), adj_vs_sr)
  ) |>
  cols_move(.e_exc, exc_p) |>
  cols_move(.e_cont_adj, cont_adj_p) |>
  cols_move(.e_cont_ag, cont_ag_p) |>
  cols_hide(
    c(starts_with("cond"), contains("_vs_"), ".e")
  ) |> 
  cols_label(starts_with(".e") ~ "") |>
  tab_bayes_generics(
    pre_footnote = "Bold rows indicate a difference to the dichotomous Bayesian model.", 
    post_footnote = "BV = behavioural variability, AE = approximate entropy, 
     Pre-positive = Pre-test positive affect, Pre-negative = Pre-test negative affect, 
     PSD = partial sleep deprivation.") |>
  tab_header("", md("*Predictors of Behavioural Variability Across Sleep Variables*"))

conditional_save(
  tbls[["BV_predictors_across_sleep_variables"]]
  , "BV - Predictors across sleep variables"
)
