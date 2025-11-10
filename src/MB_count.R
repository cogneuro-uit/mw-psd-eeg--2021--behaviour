#' On request from Boulakis Paradeisios and collegues. November 07, 2025.
#' 
#' We are currently conducting a meta-analysis of mind-blanking occurence rates.
#'  In research articles that have utilized MB probes, we found that you have 
#'  utilized them in your experience sampling paradigm. I would like to ask if 
#'  it is possible to provide
#' - the mean % MB across all participants during normal sleep (report == 1 in second question)
#' - the mean % MB across all participants after sleep deprivation (report == 1 in second question)

library(ProjectTemplate)
relative_path=""
load.project()

data.probe.mood.sleep |>
  mutate(mb_n = as.numeric(mb)) |>
  filter(mw > 2) |>
  summarise(
    .by = sleepdep
    , mb_rel    = sum(mb_n==3, mb==4) / n()
    , mb_perc   = mb_rel * 100
    , mb_4_rel  = sum(mb==4) / n()
    , mb_4_perc = mb_4_rel * 100
  ) |> 
  gt() |>
  fmt_number() |>
  gtsave("count_mb_simple.docx")


data.probe.mood.sleep |>
  filter(mw > 2) |>
  mutate(mb_n = as.numeric(mb)) |>
  summarise(
    .by = c(sleepdep, mb_n)
    , n = n()
  ) |>
  mutate(
    .by = sleepdep
    , tot_n = sum(n)
  ) |>
  mutate(
    rel = n / tot_n
    , perc = rel * 100
  ) |>
  mutate(
    .by = sleepdep
    , dichotomized = case_when(
      mb_n %in% c(3,4) ~ "MB"
      , mb_n %in% c(1,2) ~ "Content"
    )
  ) |>
  mutate(
    .by = c(sleepdep, dichotomized) 
    , n_dich = sum(n)
    , rel_dich = sum(rel) 
    , perc_dich = sum(perc)
  ) |>
  arrange( sleepdep, mb_n ) |>
  gt() |>
  fmt_number() |>
  gtsave("count_mb_elaborated.docx")


