## process into probe-dataset

#' Transform the data into a meaningful summary. 
#' We also attach *mood* scores for each test session.
#' We also **REVERSE** probe answers, such that each respective probe rank
#' correspond to the respective phenomenon.
data.probe <- 
  data %>% 
  group_by(subj, sleepdep, probenum) %>% 
  do({
    d <- .
    
    ## get taps (keys)
    dd=filter(d, stimulus=="tap")
    taps=as.integer(factor(dd$response, levels=c("lctrl","rctrl"), labels=c("left","right")))-1
    taps=tail(taps, nback)
    
    ## unravel tap-timings
    iti=dd$time %>% diff %>% tail(nback)
    
    ## return summary of it
    data.frame(
      #' Add a more descriptive name to the variables (instead of probe1)
      mw   = as.integer(d$response[d$stimulus=="probe1"])+1,
      mb   = as.integer(d$response[d$stimulus=="probe2"])+1,
      smw  = as.integer(d$response[d$stimulus=="probe3"])+1,
      apen = apen_int(taps,which.apen)[which.apen+1],
      bv   = sd(iti)
    )
  }) %>% ungroup %>% 
  mutate(.after = probenum, probenum_prop = probenum/25) |>
  mutate(
    across(c("mw","mb","smw"), ~ (5-.x) |> ordered(levels=1:4)),
    logapen  = -log(log(2)-apen),
    logbv    = log(bv),
    zlogapen = (logapen-mean(logapen))/sd(logapen),
    zlogbv   = (logbv-mean(logbv))/sd(logbv),
  ) |>
  left_join(
    demographics |> select(subj, group),
    by = "subj"
  ) |> 
  mutate(.after = subj, group=group, session = case_when(
    group == "ESD" & sleepdep == "SD"      ~ "S1",
    group == "ESD" & sleepdep == "control" ~ "S2",
    group == "LSD" & sleepdep == "SD"      ~ "S2",
    group == "LSD" & sleepdep == "control" ~ "S1",
  ))

# Attach mood
data.probe.mood <-
  data.probe |>
  left_join(
    panas_session |> 
      select(-PANASsum) |>
      pivot_wider(names_from = c(prepost, valence), values_from = PANASsum_0), 
    c("subj", "sleepdep")
  )
