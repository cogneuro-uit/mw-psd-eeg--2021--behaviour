# Test group differences ("collider bias")        ======
#' 
#' Does the individual we exclude based on not achieving adequate sleep, 
#' differ significantly from those who we included? 

test_exluded_included_NS <- 
  data.probe.mood.sleep.quiz |>
  mutate(
    excluded = case_when(
      c.Adjusted_Duration.diff.pos >= 1.5 & sleepdep=="SD"  ~ "dichotomous"
      , c.Adjusted_Duration.diff.pos < 1.5 & sleepdep=="SD" ~ "excluded"
      , c.Adjusted_Duration.diff.pos >= 1.5 & sleepdep=="control" ~ NA)
    , across(c(mw,mb,smw), ~as.numeric(.x)) ) |>
  group_by(subj) |>
  fill(excluded, .direction = "updown") |>
  ungroup() |>
  filter(.by = subj, sleepdep=="control") |> 
  pivot_longer(c(mw, mb, smw, apen, bv, value_fatigue, value_sleepiness, pre_neg, pre_pos
                 , value_insomnia, value_pittsburgh, value_dinural_avg, value_panas_neg, value_panas_pos, value_alcohol)) |>
  summarise(
    .by    = c(subj, excluded, name)
    , mean = mean(value)
  ) |>
  summarise(
    .by = name
    , test = "dich vs. exclude"
    , dich  = mean(mean[excluded=="dichotomous"])
    , exc  =  mean(mean[excluded=="excluded"])
    , est  = mean(mean[excluded=="dichotomous"]) - mean(mean[excluded=="excluded"])
    , sd   = sd(mean[excluded=="dichotomous"]) - sd(mean[excluded=="excluded"])
    , bf   = extractBF( ttestBF(mean[excluded=="dichotomous"], mean[excluded=="excluded"]) )$bf
    , int  = list(      t.test(mean[excluded=="dichotomous"], mean[excluded=="excluded"]) )
  ) |>
  mutate(
    t    = map(int, ~ unlist(.x)[["statistic.t"]])
    , df = map(int, ~ unlist(.x)[["parameter.df"]])
    , p  = map(int, ~ unlist(.x)[["p.value"]]) |> fmt_APA_numbers(, .p=T)
    , across(c(dich, exc, est,sd, bf,t,df), ~fmt_APA_numbers(.x))
    , int = NULL
    , name = str_remove_all(name, "value_")
  ) 

