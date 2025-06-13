exclude_sleep_adjusted <- 
  sleeptimes_updated_trans |> 
  filter(sleepdep == "SD") |>
  filter(c.Adjusted_Duration.diff.pos < 1.5) |>
  pull(subj) |> 
  unique()

