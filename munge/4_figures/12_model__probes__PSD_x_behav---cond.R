# Mind wandering           =====
## Relationship to BV       =====
### Generate         ======
mw_plot <- conditional_effects(
  mod.cont$mw
  , effects = "zlogbv"
  , conditions = fig_cond
)

### plot      =====
figs[["MW~PSD*BV---cond"]] <-
  mw_plot[["zlogbv"]] |>
  default_geoms() +
  probe("Changes in mind wandering") +
  z_score(xlabel="Z-scored BV",axis="x") + 
  coord_cartesian(xlim=c(-1,1), ylim=c(1,4))

### Save      ====
conditional_save(
  figs[["MW~PSD*BV---cond"]]
  , "MW - over BV and PSD---cond"
  , width = 4.5, height = 4.5
)


## Relationship to AE       =====
### Generate         ======
mw_plot2 <- conditional_effects(
  mod.cont$mw
  , effects = "zlogapen"
  , conditions = fig_cond
)

### plot      =====
figs[["MW~PSD*AE---cond"]] <-
  mw_plot2[["zlogapen"]] |>
  default_geoms() +
  probe("Changes in mind wandering") +
  z_score(xlabel="Z-scored AE",axis="x") +
  coord_cartesian(xlim=c(-1,1), ylim=c(1,4))

### Save      ====
conditional_save(
  figs[["MW~PSD*AE"]]
  , "MW - over AE and PSD---cond"
  , width = 4.5, height = 4.5
)


## Combined MW plot         =====
### Plot       =====
figs[["MW~behav*PSD---cond"]] <-
  mw_plot[["zlogbv"]] |>
  mutate(variable = "Behavioural variability", mw=NULL) |>
  bind_rows(
    mw_plot2[["zlogapen"]] |>
      mutate(variable = "Approximate entropy", mb=NULL)
  ) |>
  mutate(variable = fct_relevel(variable, "Behavioural variability")) |>
  default_geoms() + 
  facet_wrap(~variable) +
  probe("Changes in mind wandering") +
  z_score(xlabel="Z-score behaviour", axis="x") + 
  coord_cartesian(xlim=c(-1,1), ylim=c(1,4))

### Save       ====
conditional_save(
  figs[["MW~behav*PSD---cond"]]
  , "MW - over behaviour and PSD---cond"
  , width = 5.5, height = 4
)

# Mind blanking     ======
## Relationship to AE       =====
### Generate   =====
mb_plot <- conditional_effects(
  mod.cont$mb
  , effects = "zlogapen"
  , conditions = fig_cond
)

### Plot     =====
figs[["MB~PSD*AE---cond"]] <-
  mb_plot[["zlogapen"]] |>
  default_geoms() +
  probe("Changes in mind wandering") +
  z_score(xlabel = "Z-score AE", axis="x") +
  coord_cartesian(xlim=c(-1,1), ylim=c(1,4))

### Save     =====
conditional_save(
  figs[["MB~PSD*AE---cond"]]
  , "MB - over PSD and AE---cond"
  , width = 4.5, height = 4.5
)

