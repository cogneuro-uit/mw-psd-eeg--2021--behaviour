library(ProjectTemplate)
load.project()

# This script is used to fix the disagreement between the researchers

library(tidyverse)
library(patchwork)

#' Sometimes, actigraphy and self-reported sleep are significantly different, 
#' but this does not have to be the case. A recent paper illustrate a simple 
#' hierarchical procedure to adjust the actigraphy's estimated sleep onset/wake. 
#' 
#' See: 
#' Follesø, H. S., Austad, S. B., Olsen, A., & Saksvik-Lehouillier, I. (2021). 
#' The development, inter-rater agreement and performance of a hierarchical 
#' procedure for setting the rest-interval in actigraphy data. Sleep Medicine, 
#' 85, 221–229. https://doi.org/10.1016/j.sleep.2021.07.025
#' 
#' We loosely followed the Follesø paper. Some cases were clearly wrong for 
#' both the self-report and the actigraphy, and in these cases we went with the 
#' significant change in activity (and considered light when present, albeit
#' light changes varied significantly and was not used as a starting point, but 
#' as supplemental). 



# Parameters =====
script_display_plots <- FALSE
#' **TRUE**

sleeptimes <- left_join(sleep_diary, actigraphy, by=c("subj","date"="date_end_ag"))

# Deviating sleep subjects      ======
# sleeptimes, because we want to adjust ALL subjects to see if their sleep times changes (i.e., if the exclusion changes) 
sleeptimes_check <- 
  sleeptimes |>
  filter(!(subj=="030")) |>
  #' Subject 030 **does not** have actigraphy data, hence the person is removed.
  #' 
  mutate(
    .before = 2, 
    pre_control_lead  = if_else(pre_control==1  | lead(pre_control==1),  1, 0),
    pre_sleepdep_lead = if_else(pre_sleepdep==1 | lead(pre_sleepdep==1), 1, 0),
  ) |>
  filter(pre_control_lead==1 | pre_sleepdep_lead==1)  |>
  mutate(
    .by = c(subj, pre_control_lead), 
    .before = 4,
    p_c_cs = cumsum(pre_control_lead),
    p_s_cs = cumsum(pre_sleepdep_lead),
    pre_cum = if_else(p_c_cs==0, p_s_cs, p_c_cs),
    p_c_cs = NULL, p_s_cs = NULL,
    date = ymd(date),
  ) |>
  #' By subject becausse some participants have overlapping dates, and 
  #' therefore, we do not want to filter out the wrong participants
  mutate(
    .by = subj,
    .before=1,
    wake_diff    = sleep_wake_ag_h - last_awaking_fix, # higher indicate LATER AG wake
    check_wake   = if_else(abs(wake_diff)  >= .25 & pre_cum != 1, TRUE, NA),
    onset_diff   = (sleep_onset_ag_h - sleep_time_cum), # higher indicate LATER AG onset
    check_onset  = if_else( abs(onset_diff) >= .25 & pre_cum != 1, TRUE, NA),
    check_onset_k  = if_else( check_onset | lead(check_onset), TRUE, check_onset)
    # keep extra
  )  |>
  filter( !is.na(check_wake) | !is.na(check_onset_k) )

# Get relevant actigraphy and transforms 
relevant_actigraphy <- 
  actigraphy_activity |>
  left_join(
    sleeptimes_check,
    by = join_by(subj==subj, date2==date)
  ) |> 
  filter( !is.na(check_wake) | !is.na(check_onset_k) ) 


# Wake FUNCS    =====
check_wake_activity <- function(..., our_num = NULL, start_time = NULL, end_time = NULL){
  
  s_subj <- ...[["subj"]]
  date   <- ...[["date"]]
  ag_num <- ...[["sleep_wake_ag_h"]]
  sr_num <- ...[["last_awaking_fix"]]
  
  if(is.null(start_time)) start_time <- floor(min(ag_num, sr_num) - .5)
  if(is.null(end_time)) end_time <- ceiling(max(ag_num, sr_num) + .5)
  
  d <- relevant_actigraphy |>
    filter(subj == s_subj, date2 == date, 
           time >= hms(str_glue("{start_time}:0:0")) & 
             time <= hms(str_glue("{end_time}:0:0")) ) |>
    mutate(time = as.numeric(time)/3600)
  
  o <- d |>
    ggplot(aes(time, Activity)) +
    labs(y = "Activity", x = "Hour (24h)") +
    theme_bw() +
    geom_line(alpha = .5)
  
  rest_x <- d |> 
    filter(`Interval Status`=="REST") |>
    summarise( m = min(time), mx = max(time) ) |> 
    mutate(across(1:2, ~if_else(is.infinite(.x), NA, .x)))
  
  
  if(!is.na(rest_x$m)){
    o <- o + geom_ribbon(aes(xmin = rest_x$m, xmax = rest_x$mx), fill = "orange", alpha = .2)
  }
  
  # AG if present
  if(!is.null(ag_num)){
    o <- o + 
      geom_ribbon(aes(xmin = ag_num - .25, xmax = ag_num + .25), fill="lightblue4", alpha=.1) +
      geom_vline(xintercept = ag_num, col="blue") 
  } 
  # SR if present
  if (!is.null(sr_num)) {
    o <- o +
      geom_ribbon(aes(xmin = sr_num - .25, xmax = sr_num + .25), fill="red4", alpha=.075) +
      geom_vline(xintercept = sr_num, col="red")  
  }
  # Our decision, if present
  if(!is.null(our_num)){
    o <- o + 
      geom_ribbon(aes(xmin = our_num - .25, xmax = our_num + .25), fill="green4", alpha=.125) +
      geom_vline(xintercept = our_num, col="green") # our decision
  } 
  o
}

check_wake_light <- function(... = _, our_num = NULL, start_time = NULL, end_time = NULL){
  
  s_subj <- ...[["subj"]]
  date   <- ...[["date"]]
  ag_num <- ...[["sleep_wake_ag_h"]]
  sr_num <- ...[["last_awaking_fix"]]
  
  if(is.null(start_time)) start_time <- floor(min(ag_num, sr_num) - .5)
  if(is.null(end_time)) end_time <- ceiling(max(ag_num, sr_num) + .5)
  
  d <- relevant_actigraphy |>
    filter(subj == s_subj, date2 == date, 
           time >= hms(str_glue("{start_time}:0:0")) & 
             time <= hms(str_glue("{end_time}:0:0")) ) |>
    mutate(time = as.numeric(time)/3600) |>
    pivot_longer(c(`Blue Light`, `Red Light`, `Green Light`, `White Light`))
  
  o <- d |>
    ggplot(aes(time, value, col=name )) +
    labs(y = "Log(lumen)", x = "Hour (24h)") +
    scale_y_log10() +
    theme_bw() +
    geom_line(alpha = .5) 
  
  rest_x <- d |> 
    filter(`Interval Status`=="REST") |>
    summarise( m = min(time), mx = max(time) ) |> 
    mutate(across(1:2, ~if_else(is.infinite(.x), NA, .x)))
  
  if(!is.na(rest_x$m)){
    o <- o + geom_ribbon(aes(col=NULL, xmin = rest_x$m, xmax = rest_x$mx), fill = "orange", alpha = .2)
  }
  
  # AG if present
  if(!is.null(ag_num)){
    o <- o + 
      geom_ribbon(aes(col=NULL, xmin = ag_num - .25, xmax = ag_num + .25), fill="lightblue4", alpha=.1) +
      geom_vline(xintercept = ag_num, col="blue") 
  } 
  # SR if present
  if (!is.null(sr_num)) {
    o <- o +
      geom_ribbon(aes(col=NULL, xmin = sr_num - .25, xmax = sr_num + .25), fill="red4", alpha=.075) +
      geom_vline(xintercept = sr_num, col="red")  
  }
  # Our decision, if present
  if(!is.null(our_num)){
    o <- o + 
      geom_ribbon(aes(col=NULL, xmin = our_num - .25, xmax = our_num + .25), fill="green4", alpha=.125) +
      geom_vline(xintercept = our_num, col="green") # our decision
  } 
  o
}

check_wake_act_lig <- function(..., our_num = NULL, start_time = NULL, end_time = NULL){
  o <- ...  |> check_wake_activity(our_num = our_num, start_time = start_time, 
                                   end_time = end_time)
  o2 <- ... |> check_wake_light(   our_num = our_num, start_time = start_time, 
                                   end_time = end_time)
  o + o2 + patchwork::plot_layout(nrow=2)
}

add_wake_change <- function(..., w, z, c, a){
  #' Add change implicitly use the globally set "row" variable and adds the 
  #' relevant variables to the set row. 
  
  ... |> mutate(
    # The adjusted wake time
    wake_time_adj = ifelse(row_number() == row, w, wake_time_adj),
    #' Confidence in adjustment
    #' Ranked on a 1-7 Likert scale, with 1 as low confidence, and 7 as high confidence
    wake_conf          = ifelse(row_number() == row, c, wake_conf),
    # Folleso adjustment
    wake_zone          = ifelse(row_number() == row, z, wake_zone),
    # The categorical zone
    wake_folleso_adj   = ifelse(row_number() == row, a, wake_folleso_adj))
}

# Onset FUNCS =====
check_onset_activity <- function(..., our = NULL, start_time = NULL, end_time = NULL){
  
  s_subj    <- ...[["subj"]]
  date      <- ...[["date"]] |> ymd() 
  date_bfr  <- date-days(1)
  ag_on     <- ...[["sleep_onset_ag_h"]]
  sr_on     <- ...[["sleep_time_cum"]]
  attempt_sleep <- ...[["tried_to_sleep_cum"]]
  
  if(is.null(start_time)) start_time <- floor(min(ag_on, sr_on) - .5)
  if(is.null(end_time)) end_time <- ceiling(max(ag_on, sr_on) + .5)
  
  d <- relevant_actigraphy |>
    filter(subj == s_subj, (date2 == date | date2 == date_bfr)) |>
    mutate(time = if_else(date2 == date_bfr, time, time + hms("24.0.0"))) |>
    filter((time >= hms(str_glue("{start_time}:0:0"))) & 
             (time <= hms(str_glue("{end_time}:0:0"))) ) |>
    mutate( time       = as.numeric(time)/3600 )
  
  o <- d |>
    ggplot(aes(time, Activity)) +
    theme_bw() +
    labs(x = "Time (24hr)", y="Activity") +
    geom_line(alpha = .7)
  
  # sleep_x <- d |> filter(`Interval Status`=="REST-S") |>
  #   summarise( m = min(time), mx = max(time) ) |>
  #   mutate(across(1:2, ~if_else(is.infinite(.x), NA, .x)))
  # 
  # if(!is.na(sleep_x$m)){
  #   o <- o + geom_ribbon(aes(xmin = sleep_x$m, xmax = sleep_x$mx), fill = "purple", alpha = .1)
  # }
  
  rest_x <- d |> filter(`Interval Status`=="REST") |>
    summarise( m = min(time), mx = max(time) ) |> 
    mutate(across(1:2, ~if_else(is.infinite(.x), NA, .x)))
  
  if(!is.na(rest_x$m)){
    o <- o + geom_ribbon(aes(xmin = rest_x$m, xmax = rest_x$mx), fill = "orange", alpha = .2)
  }
  
  # AG if present
  if(!is.null(ag_on)){
    o <- o + geom_ribbon(aes(xmin  = ag_on - .25, xmax = ag_on + .25), fill="blue3", alpha=.1) +
      geom_vline(xintercept = ag_on, col="blue3") 
  } 
  # SR if present
  if (!is.null(sr_on)) {
    o <- o + geom_ribbon(aes(xmin  = sr_on - .25, xmax = sr_on + .25), fill="red4", alpha=.075) +
      geom_vline(xintercept = sr_on, col="red4")  
  }
  # Our decision, if present
  if(!is.null(our)){
    o <- o + geom_ribbon(aes(xmin  = our - .25, xmax = our + .25), fill="green4", alpha=.125) +
      geom_vline(xintercept = our, col="green4") # our decision
  }
  
  if(!is.null(attempt_sleep)){
    o <- o + 
      geom_ribbon(aes(xmin  = attempt_sleep, xmax = sr_on), fill="orange3", alpha=.125)
  }
  o 
}

check_onset_light <- function(..., our = NULL, start_time = NULL, end_time = NULL){
  
  s_subj    <- ...[["subj"]]
  date      <- ...[["date"]] |> ymd()
  date_bfr  <- date-days(1)
  ag_on     <- ...[["sleep_onset_ag_h"]]
  sr_on     <- ...[["sleep_time_cum"]]
  
  attempt_sleep <- ...[["tried_to_sleep_cum"]]
  
  if(is.null(start_time)) start_time <- floor(min(ag_on, sr_on) - .5)
  if(is.null(end_time)) end_time <- ceiling(max(ag_on, sr_on) + .5)
  
  d <- relevant_actigraphy |>
    filter(subj == s_subj, (date2 == date | date2 == date_bfr)) |>
    mutate(time = if_else(date2==date_bfr, time, time+hms("24.0.0"))) |>
    filter(time > hms(str_glue("{start_time}:0:0")) & time < hms(str_glue("{end_time}:0:0")) )  |>
    pivot_longer(ends_with("Light")) |>
    mutate(time = as.numeric(time)/3600)
  
  o <- d |>
    ggplot(aes(time, value)) +
    theme_bw() +
    labs(x = "Time (24hr)", y="Log(Light)") +
    scale_color_manual(values=c("blue","green","red","gray")) +
    scale_y_log10() +
    geom_line(aes(col = name), alpha = .7)
  
  # sleep_x <- d |> filter(`Interval Status`=="REST-S") |>
  #   summarise( m = min(time), mx = max(time) ) |>
  #   mutate(across(1:2, ~if_else(is.infinite(.x), NA, .x)))
  # 
  # if(!is.na(sleep_x$m)){
  #   o <- o + geom_ribbon(aes(xmin = sleep_x$m, xmax = sleep_x$mx), fill = "pink", alpha = .1)
  # }
  # 
  
  rest_x <- d |> filter(`Interval Status`=="REST") |>
    summarise( m = min(time), mx = max(time) ) |>
    mutate(across(1:2, ~if_else(is.infinite(.x), NA, .x)))
  
  if(any(!is.na(rest_x$m))){
    o <- o + geom_ribbon(aes(xmin = rest_x$m, xmax = rest_x$mx), fill = "orange", alpha = .2)
  }
  
  if(any(!is.na(d$rest_str))){
    o <- o + geom_ribbon(aes(xmin = rest_str, xmax = rest_end),
                         fill = "lightblue1", alpha = .25) 
  }
  if(any(!is.na(d$sleep_str))){
    o <- o + geom_ribbon(aes(xmin = sleep_str, xmax = sleep_end),
                         fill = "lightblue4", alpha = .3)  
  }
  # AG if present
  if(!is.null(ag_on)){
    o <- o + geom_ribbon(
      aes(xmin  = ag_on - .25, xmax = ag_on + .25), 
      fill="blue4", alpha=.1) +
      geom_vline(xintercept = ag_on, col="blue4") 
  } 
  # SR if present
  if (!is.null(sr_on)) {
    o <- o + geom_ribbon(
      aes(xmin  = sr_on - .25, xmax = sr_on + .25), 
      fill="red4", alpha=.075) +
      geom_vline(xintercept = sr_on, col="red4")  
  }
  # Our decision, if present
  if(!is.null(our)){
    o <- o + geom_ribbon(aes(xmin  = our - .25, xmax = our + .25), 
                         fill="green4", alpha=.125) +
      geom_vline(xintercept = our, col="green4") # our decision
  } 
  if(!is.null(attempt_sleep)){
    o <- o + 
      geom_ribbon(aes(xmin  = attempt_sleep, xmax = sr_on), fill="orange3", alpha=.125)
  }
  o
}

check_onset_act_lig <- function(..., our = NULL, start_time = NULL, end_time = NULL){
  o  <- ... |> check_onset_activity(our = our, start_time = start_time, end_time = end_time)
  o2 <- ... |> check_onset_light(our = our, start_time = start_time, end_time = end_time)
  o + o2 + patchwork::plot_layout(nrow=2)
}

add_onset_change <- function(..., o, z, c, a){
  #' Add change implicitly use the globally set "row" variable and adds the 
  #' relevant variables to the set row. 
  
  ... |> mutate(
    # The adjusted wake time (our version)
    onset_time_adj      = ifelse(row_number() == row, o, onset_time_adj),
    #' Confidence in adjustment
    #' Ranked on a 1-7 Likert scale, with 1 as low confidence, and 7 as high confidence
    onset_conf          = ifelse(row_number() == row, c, onset_conf),
    
    # Follesø adjustment
    onset_folleso_adj   = ifelse(row_number() == row, a, onset_folleso_adj),
    # the categorical zone
    onset_zone          = ifelse(row_number() == row, z, onset_zone),
  )
}

#  Fixes   ====
##  Wake     =====
# par 4
row <- 1
p_l_wak[[4]][row,] |> check_wake_act_lig()
p_l_wak[[4]] <- p_l_wak[[4]] |> add_wake_change(w = 7.29, z="C",c=6, a = p_l_wak[[4]][row,"sleep_wake_ag_h"][[1]])

# Par5
row <- 3
p_l_wak[[5]][row,] |> check_wake_act_lig()
p_l_wak[[5]] <- p_l_wak[[5]] |> add_wake_change(w=7.5,c=5,z="D", a=7.5)
row <- 5
p_l_wak[[5]][row,] |> check_wake_act_lig(our_num = 7.64)
p_l_wak[[5]] <- p_l_wak[[5]] |> add_wake_change(w=7.64, c=6, z="D", a=7.64)
row <- 6
p_l_wak[[5]][row,] |> check_wake_act_lig(our_num = 6.62)
p_l_wak[[5]] <- p_l_wak[[5]] |> add_wake_change(w=6.62, c=6, z="D", a=6.62)

#par11
row <- 1
p_l_wak[[11]][row,] |> check_wake_act_lig(our_num = 10.27)
p_l_wak[[11]] <- p_l_wak[[11]] |> add_wake_change(w=10.27, z="D", c=6, a=10.27)
row <- 4
p_l_wak[[11]][row,] |> check_wake_act_lig(end_time = 11, our_num = 9.45)
p_l_wak[[11]] <- p_l_wak[[11]] |> add_wake_change(w=9.45, z="D", c=7, a=9.45)

#par12
row <- 3
p_l_wak[[12]][row,] |> check_wake_act_lig(our_num = 4.93)
p_l_wak[[12]] <- p_l_wak[[12]] |> add_wake_change(w=4.93, z="D", c=6, a=4.93)
row <- 5
p_l_wak[[12]][row,] |> check_wake_act_lig(our_num = 6.38)
p_l_wak[[12]] <- p_l_wak[[12]] |> add_wake_change(w=6.38, z="D", c=6, a=6.38)

#par13
row <- 4
p_l_wak[[13]][row,] |> check_wake_act_lig(our_num = 6.46)
p_l_wak[[13]] <- p_l_wak[[13]] |> add_wake_change(w=6.46, z="D", c=6, 
                                                  a=p_l_wak[[13]][row,"sleep_wake_ag_h"][[1]])
#par15
row <- 3
p_l_wak[[15]][row,] |> check_wake_act_lig(our_num = 7.85, start_time = 4)
p_l_wak[[15]] <- p_l_wak[[15]] |> add_wake_change(w=7.85, z="D", c=5, 
                                                  a=7.85)

#par16
#' all of 16 is here re-done as it did not have the correct application in the first round: 
# # Day 1
# row <- 1
# p_l_wak[[16]][row,] |> check_wake_act_lig(start_time=5, end_time = 9, our_num = 7.38)
# p_l_wak[[16]] <- p_l_wak[[16]] |> add_wake_change(w=7.38,  c=6, z="D-NO",a=p_l_wak[[15]][row,"sleep_wake_ag_h"][[1]])

# Day 1
row <- 1
p_l_wak[[16]][row,] |> check_wake_act_lig(our_num = 7.35)
p_l_wak[[16]] <- p_l_wak[[16]] |> add_wake_change(w=7.35,  c=6, z="D-NO",a=7.35)
# Day 3
row <- 3
p_l_wak[[16]][row,] |> check_wake_act_lig(our_num = 7.20)
p_l_wak[[16]] <- p_l_wak[[16]] |> add_wake_change(w=7.20, c=5, z="D-NO",a=7.20)
# Day 4
row <- 4
p_l_wak[[16]][row,] |> check_wake_act_lig(our_num  = 7.47)
p_l_wak[[16]] <- p_l_wak[[16]] |> add_wake_change(w=7.47, c=6, z="D-NO", a=7.47)

#par22
row <- 4
p_l_wak[[22]][row,] |> check_wake_act_lig(our_num  = 9.32)
p_l_wak[[22]] <- p_l_wak[[22]] |> add_wake_change(w=11, z="D", c=5, 
                                                  a=9.32)
#par23
row <- 1
p_l_wak[[23]][row,] |> check_wake_act_lig(our_num = 6.60)
p_l_wak[[23]] <- p_l_wak[[23]] |> add_wake_change(w=6.53, z="D-NO", c=7, a=6.60)
row <- 2
p_l_wak[[23]][row,] |> check_wake_act_lig(our_num = 7, end_time = 9)
p_l_wak[[23]] <- p_l_wak[[23]] |> add_wake_change(w=7,z="D", c=7, a=7)
row <- 3
p_l_wak[[23]][row,] |> check_wake_act_lig(our_num = 7.14)
p_l_wak[[23]] <- p_l_wak[[23]] |> add_wake_change(w=7.14, z="D", c=5, a=7.14)
# par 26
row <- 3
p_l_wak[[26]][row,] |> check_wake_act_lig(our_num = 10.7)
p_l_wak[[26]] <- p_l_wak[[26]] |> add_wake_change(w=10.7, z="D", c=4, a=10.7)
row <- 4
p_l_wak[[26]][row,] |> check_wake_act_lig(our_num = 7.98)
p_l_wak[[26]] <- p_l_wak[[26]] |> add_wake_change(w=7.98,
                                                  z="D", c=5, a=7.98)
# Why is par26-day 5 missing?, added:
# row <- 5
# p_l_wak[[26]][row,] |> check_wake_act_lig()
# p_l_wak[[26]] <- p_l_wak[[26]] |> add_wake_change(w=p_l_wak[[26]][row,"sleep_wake_ag_h"][[1]],
#                                                   z="D", c=5, a=p_l_wak[[26]][row,"sleep_wake_ag_h"][[1]])

#par 28
row <- 2
p_l_wak[[28]][row,] |> check_wake_act_lig(our_num = 6.62)
p_l_wak[[28]] <- p_l_wak[[28]] |> add_wake_change(w=6.62, z="C", c = 6, a= 6.62)

#par 32
row <- 4
p_l_wak[[032]][row,] |> check_wake_act_lig(our_num = 6.58)
p_l_wak[[032]] <- p_l_wak[[32]] |> add_wake_change(w=6.58, z="D", c=6, a=6.58)

## Onset  ======
# par 10
row <- 3
p_l_on[[10]][row,] |> check_onset_act_lig(our = 26.3)
p_l_on[[10]] <- p_l_on[[10]] |> add_onset_change(o = 26.3, c=4, z="D-NO", a=26.3)
# day 4
row <- 4
p_l_on[[10]][row,] |> check_onset_act_lig(our = 26.58)
p_l_on[[10]] <- p_l_on[[10]] |> add_onset_change(o = 26.58, c=6, z="D", a=26.58)

#par12
row <- 4
p_l_on[[12]][row,] |> check_onset_act_lig(our=23.55)
p_l_on[[12]] <- p_l_on[[12]] |> add_onset_change(o = 23.55, c=7, z="D", a=23.55)

##par13 
row <- 3
p_l_on[[13]][row,] |> check_onset_act_lig()
p_l_on[[13]] <- p_l_on[[13]] |> add_onset_change(o = p_l_on[[13]][row, "sleep_onset_ag_h"][[1]], c=7, z="D-NO", 
                                                 a = p_l_on[[13]][row, "sleep_onset_ag_h"][[1]])
row <- 4
p_l_on[[13]][row,] |> check_onset_act_lig(our = 25.1)
p_l_on[[13]] <- p_l_on[[13]] |> add_onset_change(o = 25.1, c=6, z="D-NO", 
                                                 a=p_l_on[[13]][row, "sleep_onset_ag_h"][[1]])
# day 5
row <- 5
p_l_on[[13]][row,] |> check_onset_act_lig(our = 25.4)
p_l_on[[13]] <- p_l_on[[13]] |> add_onset_change(o = 25.4, c=7, z="D-NO", 
                                                 a=p_l_on[[13]][row, "sleep_onset_ag_h"][[1]])
#par 16
row <- 1
p_l_on[[16]][row,] |> check_onset_act_lig(our = 23.25)
p_l_on[[16]] <- p_l_on[[16]] |> add_onset_change(o = 22.12, c=3, z="D-NO", a=23.25)
row <- 2
p_l_on[[16]][row,] |> check_onset_act_lig(our = 23.9)
p_l_on[[16]] <- p_l_on[[16]] |> add_onset_change(o = 22.02, c=3, z="D-NO", a=23.9)

#par18
row <- 2
p_l_on[[18]][row,] |> check_onset_act_lig(our = 26.5)
p_l_on[[18]] <- p_l_on[[18]] |> add_onset_change(o = 26.5, c=3, z="D-NO", a=26.5)
row <- 4
p_l_on[[18]][row,] |> check_onset_act_lig(our = 24.15)
p_l_on[[18]] <- p_l_on[[18]] |> add_onset_change(o = 24.1, c=3, z="D-NO", a=24.1)

#par 20
row <- 1
p_l_on[[20]][row,] |> check_onset_act_lig(our = 24.02, end_time = 27)
p_l_on[[20]] <- p_l_on[[20]] |> add_onset_change(o = 24.02, c=6, z="D", a=24.02)

#par22
row <- 2
p_l_on[[22]][row,] |> check_onset_act_lig(our = 26.35)
p_l_on[[22]] <- p_l_on[[22]] |> add_onset_change(o = 26.35, c=6, z="D-NO", a=26.35)

#par24
row <- 2
p_l_on[[24]][row,] |> check_onset_act_lig(our = 23.32, end_time = 26)
p_l_on[[24]] <- p_l_on[[24]] |> add_onset_change(o = 23.32, c=6, z="D-NO", a=23.32)

#par26 
row <- 1
p_l_on[[26]][row,] |> check_onset_act_lig(end_time = 30)
p_l_on[[26]] <- p_l_on[[26]] |> add_onset_change(o = p_l_on[[26]][row,"sleep_onset_ag_h"][[1]], c=6, z="D-NO", 
                                                 a=p_l_on[[26]][row,"sleep_onset_ag_h"][[1]])
#par27
row <- 1
p_l_on[[27]][row,] |> check_onset_act_lig(our = 24.15)
p_l_on[[27]] <- p_l_on[[27]] |> add_onset_change(o = 24.15, c=7, z="C", 
                                                 a=p_l_on[[27]][row,"sleep_onset_ag_h"][[1]])
#par28
row <- 2
p_l_on[[28]][row,] |> check_onset_act_lig(our = 23.27)
p_l_on[[28]] <- p_l_on[[28]] |> add_onset_change(o = 23.27, c=6, z="C", a=23.27)

row <- 4
p_l_on[[28]][row,] |> check_onset_act_lig(our = 26.17)
p_l_on[[28]] <- p_l_on[[28]] |> add_onset_change(o = 26.17, c=6, z="C", 
                                                 a=p_l_on[[28]][row,"sleep_onset_ag_h"][[1]])
# par32
row <- 1
p_l_on[[32]][row,] |> check_onset_act_lig(our = 23.95)
p_l_on[[32]] <- p_l_on[[32]] |> add_onset_change(o = 23.95, c=6, z="D-NO", a=23.95)
row <- 2
p_l_on[[32]][row,] |> check_onset_act_lig(our = 25.95)
p_l_on[[32]] <- p_l_on[[32]] |> add_onset_change(o = 25.95, c=6, z="D-NO", a=25.95)

# par34
row <- 3
p_l_on[[34]][row,] |> check_onset_act_lig(end_time = 29, our = 25.88)
p_l_on[[34]] <- p_l_on[[34]] |> add_onset_change(o = 25.88, c=6, z="D-NO", a=25.88)

# rename
p_l_on2 <- p_l_on
p_l_wak2 <- p_l_wak

# Save adjustment 
save(p_l_on2, p_l_wak2, file = "data/sleep_adjustment-fix_GC-SA.rdata")


