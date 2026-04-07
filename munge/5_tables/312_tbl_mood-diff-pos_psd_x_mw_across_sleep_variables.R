change_pos_mood_tbl <- 
  bayes_tbl_sum(mod.mood.dich[["pos"]][["psdXmw"]], apa_table = T, add_loo_R2 = T)  |>
  bayes_tbl_add_sig() |>
  mutate_bayes_mod_beh() |> 
  rename_with( ~paste0("exc_", .x), 3:6 ) |>
  left_join(
    bayes_tbl_sum(mod.mood.cont$pos$psdXmw, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |> 
      rename_with( ~paste0("cont_adj_", .x), 3:6 )
    , by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(mod.mood.ext$`diff_pos~AG_PSD*MW`, apa_table = T, add_loo_R2 = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_beh() |> 
      rename_with( ~paste0("cont_ag_", .x), 3:6)
    , by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(mod.mood.ext$`diff_pos~SR_PSD*MW`, apa_table = T, add_loo_R2 = T)  |>
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



tbls[["affect_change__pred_PSD_x_MW_across_sleep_variables"]] <- 
  change_pos_mood_tbl |>
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
    pre_footnote = "Bold rows indicate a difference to the the adjusted partial sleep deprivation (PSD) model."
    , post_footnote = "MW = Mind wandering"
  ) |>
  tab_header("Predictors of Mood Change for Positive Affect Across Sleep Variable Treatments")

conditional_save(
  tbls[["affect_change__pred_PSD_x_MW_across_sleep_variables"]] 
  , "Mood Diff Positive - PSD x MW across sleep variables"
)
