tbls[["compliance_test_excluded_vs_included"]] <- 
  test_exluded_included_NS |>
  mutate(
    name = case_when(
      name == "mw"    ~ "Mind wandering (MW)"
      , name == "mb"  ~ "Mind blanking"
      , name == "smw" ~ "Spontaneous MW"
      , name == "apen" ~ "Approximate entropy"
      , name == "bv"   ~ "Behavioural variability"
      , name == "pre_neg" ~ "Pre-task negative affect"
      , name == "pre_pos" ~ "Pre-task positive affect"
      , name == "fatigue" ~ "Fatigue"
      , name == "sleepiness" ~ "Sleepiness"
      , name == "insomnia" ~ "Insomnia"
      , name == "pittsburgh" ~ "Sleep Quality"
      , name == "dinural_avg" ~ "Diurnal"
      , name == "panas_neg" ~ "Past weeks negative affect"
      , name == "panas_pos" ~ "Past weeks positive affect"
      , name == "alcohol" ~ "Alcohol"
    ) |> factor(levels = c(
      "Mind wandering (MW)", "Mind blanking", "Spontaneous MW", "Approximate entropy", 
      "Behavioural variability", "Pre-task negative affect", "Pre-task positive affect",
      "Fatigue", "Sleepiness", "Insomnia", "Sleep Quality", "Diurnal", 
      "Past weeks negative affect", "Past weeks positive affect", "Alcohol"))
  ) |>
  arrange(name) |>
  gt() |>
  tab_spanner("Estimate", c("est","sd")) |>
  tab_spanner("Group Mean", c("dich", "exc")) |>
  tab_spanner("Welch t-test", c("t","df","p")) |>
  cols_add(.e="", .after = "exc") |>
  cols_add(.e2="", .after = "sd") |>
  cols_add(.e3="", .after = "bf") |>
  cols_label(
    "dich" = "SC"
    , "exc" = "Excluded"
    , "est" = md("*M*~diff~")
    , "sd"  = md("*SD*~diff~")
    , "bf"  = md("BF~10~")
    , "t" = md("*t*")
    , "p" = md("*p*")
    , "name" = "Variable"
    , starts_with(".e") ~ ""
  ) |>
  cols_hide(test) |>
  tab_fmt_APA() |>
  tab_footnote(
    md("*Note.*  Welch t-test has not been adjusted for multiple comparisons. ER~dir~ indicates the evidence ratio that the effect is in the *b* specified direction. *p*~dir~ indicates the probability that the effect is in the *b* specified direction.")
  ) |>
  tab_header("Table SX", subtitle = md("*The Effect of Order on Thought Probes*"))
  

conditional_save(
  tbls[["compliance_test_excluded_vs_included"]]
  , "Compliance - Test excluded people vs included people"
)
