# model plot      =====
## MW plot          =====
### Generate        =====
mw_plot <- conditional_effects(
  mod.cont$mw
  , effects = c("probenum_prop")
  , conditions = fig_cond
)

### Plot    =====
figs[["MW~PSD*time---cond"]] <-
  mw_plot[["probenum_prop"]] |>
  default_geoms() +
  time() + 
  probe("Changes in MW")

### Save      ====
conditional_save(
  figs[["MW~PSD*time---cond"]]
  , "MW - over Time and PSD---cond"
  , width = 4.5, height = 4.5
)

## MB plot          ======
### Generate      =====
mb_plot <- conditional_effects(
  mod.cont$mb
  , effects = "probenum_prop"
  , conditions = fig_cond
)

### Plot    =====
figs[["MB~PSD*time---cond"]] <-
  mb_plot[["probenum_prop"]] |>
  default_geoms() +
  time() + 
  probe("Changes in MB")

### Save      ====
conditional_save(
  figs[["MB~PSD*time---cond"]]
  , "MB - over time and PSD---cond"
  , width = 4.5, height = 4.5
)


## SMW plot         =====
### Generate          ======
smw_plot <- conditional_effects(
  mod.cont$smw
  , effects = "probenum_prop"
  , conditions = fig_cond
)

### Plot          =====
figs[["SMW~PSD*time---cond"]] <-
  smw_plot[["probenum_prop"]] |>
  default_geoms() +
  # scale_x_continuous(breaks = c(0.04, .2, .4, .6, .8, 1), labels = c(1, seq(5,26,5)))
  time() + 
  probe("Change in spontaneous mind wandering")

### Save          ====
conditional_save(
  figs[["SMW~PSD*time---cond"]]
  , "SMW - over Time and PSD---cond"
  , width = 4.5, height = 4.5
)


## Cobmined plot          =====
figs[["probes~PSD*time---cond"]] <-
  mw_plot[["probenum_prop"]] |>
  mutate(probe_type = "Mind wandering (MW)", mw=NULL) |>
  bind_rows(
    mb_plot[["probenum_prop"]] |>
      mutate(probe_type = "Mind blanking", mb=NULL)
  ) |>
  bind_rows(
    smw_plot[["probenum_prop"]] |>
      mutate(probe_type = "Spontaneous MW", smw=NULL)
  ) |>
  mutate(probe_type = fct_relevel(probe_type, "Mind wandering")) |>
  default_geoms() +
  facet_wrap(~probe_type) + 
  time() + 
  probe("Changes in probe response")


### Save    =====
conditional_save(
  figs[["probes~PSD*time---cond"]] 
  , "Probes - over Time and PSD---cond"
  , width = 7, height = 4
)
