change_neg_mood_tbl <-
   bayes_tbl_sum(mod.mood.dich[["neg"]][["psdXmw"]], apa_table = T, add_loo_R2 = T)  |>
   bayes_tbl_add_sig() |>
   mutate_bayes_mod_beh() |>
   rename_with( ~paste0("dich_all_", .x), 3:6 ) |>
   left_join(
     bayes_tbl_sum(mod.mood.dich.sc[["neg"]][["change"]], apa_table = T, add_loo_R2 = T)  |>
     bayes_tbl_add_sig() |>
     mutate_bayes_mod_beh() |>
     rename_with( ~paste0("dich_sc_", .x), 3:6 )
     , by = c("group", "var")
   ) |>
   left_join(
     bayes_tbl_sum(mod.mood.cont[["neg"]][["psdXmw"]], apa_table = T, add_loo_R2 = T)  |>
       bayes_tbl_add_sig() |>
       mutate_bayes_mod_beh() |>
       rename_with( ~paste0("cont_adj_", .x), 3:6 )
     , by = c("group", "var")
   ) |>
   left_join(
     bayes_tbl_sum(mod.mood.ext$`diff_neg~AG_PSD*MW`, apa_table = T, add_loo_R2 = T)  |>
       bayes_tbl_add_sig() |>
       mutate_bayes_mod_beh() |>
       rename_with( ~paste0("cont_ag_", .x), 3:6)
     , by = c("group", "var")
   ) |>
   left_join(
     bayes_tbl_sum(mod.mood.ext$`diff_neg~SR_PSD*MW`, apa_table = T, add_loo_R2 = T)  |>
       bayes_tbl_add_sig() |>
       mutate_bayes_mod_beh() |>
       rename_with( ~paste0("cont_sr_", .x), 3:6)
     , by = c("group", "var")
   ) |>
   mutate(
     cond_dich_all_p   = if_else(as.numeric(dich_all_p) >= .95, TRUE, FALSE)
     , cond_dich_sc_p  = if_else(as.numeric(dich_sc_p) >= .95, TRUE, FALSE)
     , cond_cont_adj_p = if_else(as.numeric(cont_adj_p) >= .95, TRUE, FALSE)
     , cond_cont_ag_p  = if_else(as.numeric(cont_ag_p) >= .95, TRUE, FALSE)
     , cond_cont_sr_p  = if_else(as.numeric(cont_sr_p) >= .95, TRUE, FALSE)
     , adj_vs_all      = if_else(cond_cont_adj_p  != cond_dich_all_p, TRUE, FALSE)
     , adj_vs_sc       = if_else(cond_cont_adj_p  != cond_dich_sc_p, TRUE, FALSE)
     , adj_vs_ag       = if_else(cond_cont_adj_p  != cond_cont_ag_p, TRUE, FALSE)
     , adj_vs_sr       = if_else(cond_cont_adj_p  != cond_cont_sr_p, TRUE, FALSE)
     , across(c(starts_with("c_"), starts_with("e_")), ~NULL)
     , .e_dich_all="", .e_dich_sc="", .e_cont_adj="", .e_cont_ag="", .e=""
   )



tbls[["affect_change_neg__pred_PSD_x_MW_across_sleep_variables"]] <-
  change_neg_mood_tbl |>
  gt(groupname_col = "group") |>
  tab_spanner("Full Sample", starts_with("dich_all_")) |>
  tab_spanner("Strict Compliance", starts_with("dich_sc_")) |>
  tab_spanner("Dichotomous", matches("dich_")) |>
  tab_spanner("Adjusted PSD", starts_with("cont_adj")) |>
  tab_spanner("Actigraphy PSD", starts_with("cont_ag")) |>
  tab_spanner("Self-report PSD", starts_with("cont_sr")) |>
  tab_spanner("Dose-response", matches("cont_")) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("dich_all"), adj_vs_all)
  ) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("dich_sc"), adj_vs_sc)
  ) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("cont_ag"), adj_vs_ag)
  ) |>
  tab_style(
    cell_text(weight="bold"),
    cells_body(matches("cont_sr"), adj_vs_sr)
  ) |>
  cols_move(.e_dich_all, dich_all_p) |>
  cols_move(.e_dich_sc, dich_sc_p) |>
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
  tab_header("Predictors of Mood Change for Negative Affect Across Sleep Variable Treatments")

conditional_save(
  tbls[["affect_change_neg__pred_PSD_x_MW_across_sleep_variables"]]
  , "Mood Diff Negative - PSD x MW across sleep variables"
)
