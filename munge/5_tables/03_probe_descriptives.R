# Data transformation     ======

## Get general structure    =====
tbl_data <- 
  data.probe.mood.sleep |>  
  mutate(
    .by = subj
    , excluded = if_else(any(c.Adjusted_Duration.diff.pos > 1.5), F, T)
  ) |>
  mutate(
    .before = 4
    , Condition = if_else(sleepdep == "SD", "PSD","NS") |> fct_relevel("PSD")
    , Condition2 = case_when( 
      excluded == F & sleepdep == "SD" ~ "ex_PSD"
      , excluded == F & sleepdep == "control" ~ "ex_NS"
      , T ~ NA )
    , across(c(mw, mb, smw), ~as.numeric(.x))
    # relative resp
    , mb  = if_else(mw > 2, mb, NA),
    , smw = if_else(mw > 2, smw, NA)
  ) |>
  pivot_longer(c(mw,mb,smw), names_to="probe_type", values_to="probe_value") |>
  # exclude MB/S-MW that are not preceeded by a MW response
  filter(!is.na(probe_value))
  

## Summarise per data treatment     ======
tbl_data_sum <- 
  tbl_data |>
  summarise(
    .by = c(Condition, probe_type, probe_value),
    cont_n = n(),
  ) |>
  left_join(
    tbl_data |>
      summarise(
        .by = c(Condition2, probe_type, probe_value),
        exc_n = n(),
      ) |> 
      mutate(Condition = if_else(str_ends(Condition2, "PSD"), "PSD", "NS"))
    , by = c("Condition", "probe_type", "probe_value")
  ) |> 
  pivot_longer(c(cont_n, exc_n), values_to="count", names_to="dataset") |>
  select(-Condition2) |>
  mutate(
    Condition = fct_relevel(Condition, "NS")
    , probe_type = fct_relevel(probe_type, "mw")
  ) |> arrange(Condition, probe_type)


# Create gt table       ======

probe_descriptive_title = "**Table SXX**\n
  *Total Raw Count of Probe Values Across Probe Type, Condition, and Data Treatment*"
probe_descriptive_footnote =  "*Note.* Higher probe value indicate the respective phenomenon, while lower indicate the opposite, e.g., mind wandering (MW) = 4, while MW 1 = on-task. MB = mind blanking, S-MW = spontaneous MW, NS = normal sleep, PSD = partial sleep deprivation, Full = full sample, SC = strict compliance."

## Create a wide format          =====
tbls[["probe_descriptives_wide"]] <-
  tbl_data_sum |>
  mutate(dataset = if_else(dataset=="cont_n", "cont", "dich")) |>
  pivot_wider(names_from = c(Condition, probe_type, dataset), values_from = count) |>
  arrange(probe_value) |>
  gt() |>
  cols_add(.e_mw_ ="", .after = "NS_mw_dich") |>
  cols_add(.e1="", .after = "PSD_mw_dich") |>
  cols_add(.e2_mb_="", .after = "NS_mb_dich") |>
  cols_add(.e3="", .after = "PSD_mb_dich") |>
  cols_add(.e4_smw_="", .after = "NS_smw_dich") |>
  tab_spanner("NS", starts_with("NS")) |>
  tab_spanner("PSD", starts_with("PSD")) |>
  tab_spanner("MW", c(contains("_mw_"), .e_mw_)) |>
  tab_spanner("MB", c(contains("_mb_"), .e2_mb_)) |>
  tab_spanner("S-MW", c(contains("_smw_"), .e4_smw_)) |>
  cols_move(contains("_mw_"), probe_value) |>
  cols_move(contains("_mb_"), PSD_mw_dich) |>
  cols_move(.e1, PSD_mw_dich) |>
  cols_move(.e3, PSD_mb_dich) |>
  cols_label(
    ends_with("_cont") ~ "DR"
    , ends_with("_dich") ~ "SC"
    , probe_value = "Probe value"
    , starts_with(".e") ~ ""
  ) |>
  tab_footnote(md(probe_descriptive_footnote)) |>
  tab_header(md(probe_descriptive_title))

# Save
conditional_save(
  tbls[["probe_descriptives_wide"]]
  ,"Probe descriptives - Wide"
)

# Create a long format      ======
tbls[["probe_descriptives_long"]] <-
  tbl_data_sum |>
  mutate(
    dataset = case_when(
      dataset == "cont_n"   ~ "DR"
      , dataset == "exc_n"  ~ "SC"
      , TRUE                ~ as.character(dataset) # Fallback for unexpected values
    )
  ) |>
  pivot_wider(names_from = c(Condition, probe_type), values_from = count) |>
  arrange(probe_value, dataset) |>
  gt(groupname_col = c("dataset")) |>
  tab_spanner("MW", ends_with("_mw")) |>
  tab_spanner("MB", ends_with("_mb")) |>
  tab_spanner("S-MW", ends_with("_smw")) |>
  cols_label(
    starts_with("PSD") ~ "PSD"
    , starts_with("NS") ~ "NS"
    , probe_value = "Probe value"
  ) |>
  tab_footnote(md(probe_descriptive_footnote)) |>
  tab_header(md(probe_descriptive_title))

# Save
conditional_save(
  tbls[["probe_descriptives_long"]]
  , "Probe descriptives - Long"
)

