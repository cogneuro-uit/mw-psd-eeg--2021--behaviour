
outputs[["tbls"]][["sleep_comparison"]] <- 
  sleep_time_session_condition |>
  mutate(e1 = "", e2 = "") |>
  gt(groupname_col = "name") |>
  tab_spanner("Partial sleep deprivation", starts_with("PSD_")) |>
  tab_spanner("Normal sleep", starts_with("NS_")) |>
  # cols_hide(seq) |>
  # cols_move(c(prepost, PANASposneg), 1) |>
  cols_label(
    ends_with("_m") ~ md("*M*"),
    ends_with("_sd") ~ md("*SD*"),
    diff_m = md("*M*~diff~"), diff_sd = md("*SD*~diff~"), 
    bf = md("BF~10~"), 
    modality = "Variable",
    starts_with("e") ~ "",
  ) |>
  cols_move(e1, psd_sd) |>
  cols_move(e2, ns_sd) |>
  cols_align("center", c(everything(),-name)) |>
  tab_footnote(
    md("*Note.* Differences are calculated as partial sleep deprivation (PSD) -  normal sleep (NS).")
    # PANAS = positive and negative affect scale, pre = pre-task measure, post = post-task measure, pos = positive, neg = negative. 
  ) |> 
  opt_footnote_marks(marks = letters) |>
  # a
  tab_footnote(
    "Sleep quality was rated on a 5-point Likert scale (1 = very bad to 5 = very good).",
    locations = cells_body(modality, modality=="Sleep quality") ) |>
  tab_fmt_APA()


condition_save_figure(
  outputs[["tbls"]][["sleep_comparison"]],
  "Sleep - comparison",
  .suppress_print = T
)