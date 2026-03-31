#   Model       =====
##  BV plot       =====
###  Generate       ======
bv_plot <- conditional_effects(
  mod.cont$bv
  , effects = "probenum_prop"
  , conditions = fig_cond
)

### plot            =====
figs[["BV~PSD*time---cond"]] <-
  bv_plot[["probenum_prop"]] |>
  default_geoms() +
  z_score(ylabel="Changes in Z-score BV") +
  time() 

### Save            ====
conditional_save(
  figs[["BV~PSD*time---cond"]]
  , "BV - over Time and PSD---cond"
  , width = 4.5, height = 4.5
)


# AE plot         ======
## Generate         =====
ae_plot <- conditional_effects(
  mod.cont$ae
  , effects = "probenum_prop"
  , conditions = fig_cond
)

## Plot             =====
figs[["AE~PSD*time---cond"]] <-
  ae_plot[["probenum_prop"]] |>
  default_geoms() +
  z_score(ylabel = "Changes in Z-score AE") +
  time() +
  coord_cartesian(ylim=c(-.5,.5))

## Save             =====
conditional_save(
  figs[["AE~PSD*time---cond"]]
  , "AE - over Time and PSD---cond"
  , width = 4.5, height = 4.5
)

# Combined plot   =====
## Plot             =====
figs[["behaviour~PSD*time---cond"]] <-
  bv_plot[["probenum_prop"]] |>
  mutate(variable = "Behavioural variability", mw=NULL) |>
  bind_rows(
    ae_plot[["probenum_prop"]] |>
      mutate(variable = "Approximate entropy", mb=NULL)
  ) |>
  mutate(variable = fct_relevel(variable, "Behavioural variability")) |>
  default_geoms() +
  facet_wrap(~variable) +
  time() +
  z_score("Changes in behaviour") 

## Save             ====
conditional_save(
  figs[["behaviour~PSD*time---cond"]]
  , "Behaviour - over Time and PSD---cond"
  , width = 5.5, height = 4
)