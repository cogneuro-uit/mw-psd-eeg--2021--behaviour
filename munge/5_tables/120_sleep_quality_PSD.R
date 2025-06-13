tbls[["sleep_quality+sleep"]] <- 
  map(names(m.PSD_SQ), \(mod){
    bayes_tbl_sum(m.PSD_SQ[[mod]], apa_table = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0(mod, "_", .x), c(-var, -group)) 
  }) |> list_cbind() |>
  rename(var = "var...2", group ="group...1") |>
  select(-contains("...")) |>
  gt(groupname_col = "group") |>
  tab_spanner("Adjusted", starts_with("adj_")) |>
  tab_spanner("Self-report", starts_with("sr_")) |>
  tab_spanner("Actigraphy", starts_with("ag_")) |>
  tab_bayes_generics( ) |>
  tab_fmt_APA()

conditional_save(
  tbls[["sleep_quality+sleep"]]
  , "Sleep on sleep quality"
)
