## BV       ======
### Generate   =====
bv_plot <- conditional_effects(
  mod.cont$bv
  , effects = "pre_pos"
  , conditions = fig_cond
)

### Plot     =====
figs[["BV~PSD*pos---cond"]] <-
  bv_plot[["pre_pos"]] |>
  default_geoms() +
  mood_scale("pos","x") +
  z_score("Z-score BV") +
  coord_cartesian(ylim = c(-1,1), xlim=c(summarised_vals$mood_pos_m-summarised_vals$mood_pos_sd
                                         , summarised_vals$mood_pos_m+summarised_vals$mood_pos_sd))

### Save     =====
conditional_save(
  figs[["BV~PSD*pos---cond"]]
  , "BV - over PSD and pre_pos---cond"
  , width = 4.5, height = 4.5
)


## AE       ======
### Generate    =====
ae_plot <- conditional_effects(
  mod.cont$ae
  , effects = "pre_pos"
  , conditions = fig_cond
)

### Plot     =====
figs[["AE~PSD*pos---cond"]] <-
  ae_plot[["pre_pos"]] |>
  default_geoms() +
  mood_scale("pos","x") +
  z_score("Z-score AE") +
  coord_cartesian(ylim = c(-.5,.5), xlim=c(summarised_vals$mood_pos_m-summarised_vals$mood_pos_sd
                                           , summarised_vals$mood_pos_m+summarised_vals$mood_pos_sd))

### Save     =====
conditional_save(
  figs[["AE~PSD*pos---cond"]]
  , "AE - over PSD and pre_pos---cond"
  , width = 4.5, height = 4.5
)


## SMW      ======
### Generate    =====
smw_plot <- conditional_effects(
  mod.cont$smw
  , effects = "pre_pos"
  , conditions = fig_cond
)

### Plot     =====
figs[["SMW~PSD*pos---cond"]] <-
  smw_plot[["pre_pos"]] |>
  default_geoms() +
  mood_scale("pos","x") +
  probe("Changes in S-MW") +
  coord_cartesian(ylim = c(1,4), xlim=c(summarised_vals$mood_pos_m-summarised_vals$mood_pos_sd
                                        , summarised_vals$mood_pos_m+summarised_vals$mood_pos_sd))

### Save     =====
conditional_save(
  figs[["SMW~PSD*pos---cond"]]
  , "SMW - over PSD and pre_pos---cond"
  , width = 4.5, height = 4.5
)

##  MW           =======
### Generate    =====
mw_plot <- conditional_effects(
  mod.cont$mw
  , effects = "pre_pos"
  , conditions = fig_cond
)

### Plot     =====
figs[["MW~PSD*pos---cond"]] <-
  mw_plot[["pre_pos"]] |>
  default_geoms() +
  mood_scale("pos","x") +
  probe("Changes in MW") +
  coord_cartesian(ylim = c(1,4), xlim=c(summarised_vals$mood_pos_m-summarised_vals$mood_pos_sd
                                        , summarised_vals$mood_pos_m+summarised_vals$mood_pos_sd))

### Save     =====
conditional_save(
  figs[["MW~PSD*pos---cond"]]
  , "MW - over PSD and pre_pos---cond"
  , width = 4.5, height = 4.5
)


