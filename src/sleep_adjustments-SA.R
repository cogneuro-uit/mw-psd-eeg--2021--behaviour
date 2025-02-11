library(ProjectTemplate)
load.project()

#' Sometimes, actigraphy and self-reported sleep are significantly different, 
#' but this does not have to be the case. A recent paper illustrate a simple 
#' hierarchical procedure to adjust the actigraphy's estimated sleep onset/wake. 
#' 
#' See: 
#' Follesø, H. S., Austad, S. B., Olsen, A., & Saksvik-Lehouillier, I. (2021). 
#' The development, inter-rater agreement and performance of a hierarchical 
#' procedure for setting the rest-interval in actigraphy data. Sleep Medicine, 
#' 85, 221–229. https://doi.org/10.1016/j.sleep.2021.07.025

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

relevant_actigraphy |> pull(subj) |> unique() |> length()
# Check 33 subj, everyone has something wrong (yikes)

p_l_wak <- list()
p_l_wak <- 
  map(actigraphy$subj|>unique(), \(x){
    print(x)
    p_l_wak[[as.numeric(x)]] <- 
      sleeptimes_check |>
      filter(subj==x, check_wake) |> 
      select(subj, date, sleep_wake_ag_h, last_awaking_fix) |> 
      mutate(wake_time_adj=NA, wake_zone=NA, wake_conf=NA, wake_folleso_adj = NA) |>
      unique() 
  })


## Check WAKE      =====
### Funcs    =====
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

### 001       =====
# Day 1
row <- 1
p_l_wak[[1]][row,] |> check_wake_act_lig(our_num = 5.6)
p_l_wak[[1]] <- p_l_wak[[1]] |> add_wake_change(w  = 5.6, c=1, z="C", a=5.6)
# Day 2  
row <- 2
p_l_wak[[1]][row,] |> check_wake_act_lig(our_num = 8.29)
p_l_wak[[1]] <- p_l_wak[[1]] |> add_wake_change(w  = 8.29, c=6, z="C", a = 8.29)

### 002       =====
# Day 1
row <- 1
p_l_wak[[2]][row,] |> check_wake_act_lig(our_num = 8.11)
p_l_wak[[2]] <- p_l_wak[[2]] |> add_wake_change(w= 7, c=5, z="C", a=8.11)
# Day 2 
row <- 2
p_l_wak[[2]][row,] |> check_wake_act_lig(our_num = 6.72)
p_l_wak[[2]] <- p_l_wak[[2]] |> add_wake_change(w  = 6.72, c=6, z="D", a=6.76)
# Day 3
row <- 3
p_l_wak[[2]][row,] |> check_wake_act_lig(end_time=10, our_num = 6.92)
p_l_wak[[2]] <- p_l_wak[[2]] |> add_wake_change(w=7, c=5, z="D", a= 6.92)

### 003       =====
# Day 1
row <- 1
p_l_wak[[3]][row,] |> check_wake_act_lig(start_time=4, end_time=10, our_num=8.8)
p_l_wak[[3]] <- p_l_wak[[3]] |> add_wake_change(w=8.8, c=5, z="C",a=8.75)
# Day 2
row <- 2
p_l_wak[[3]][row,] |> check_wake_act_lig(our_num =8.3)
p_l_wak[[3]] <- p_l_wak[[3]] |> add_wake_change(w=8.3, c=7, z="D", a=8.3)
# Day 3
row <- 3
p_l_wak[[3]][row,] |> check_wake_act_lig(end_time = 10, our_num = 8.85)
p_l_wak[[3]] <- p_l_wak[[3]] |> add_wake_change(w=8.85, c=5, z="D", a=8.85)

### 004       =======
# Day 1
row <- 1
p_l_wak[[4]][row,] |> check_wake_act_lig(end_time = 10, our_num = 7.79)
p_l_wak[[4]] <- p_l_wak[[4]] |> add_wake_change(w  = 7.79, z="C",c=6, a=7.79)
# Day 2
row <- 2
p_l_wak[[4]][row,] |> check_wake_act_lig(end_time = 10, our_num = 6.1)
p_l_wak[[4]] <- p_l_wak[[4]] |> add_wake_change(w =6.1, c=6,z="C",a=6.1)
# Day 3
row <- 3
p_l_wak[[4]][row,] |> check_wake_act_lig(end_time = 10, our_num = 7.2)
p_l_wak[[4]] <- p_l_wak[[4]] |> add_wake_change(w=7.2, z="C", c=5, a=7.76)

### 005       =======
# Day 1
row <- 1
p_l_wak[[5]][row,] |> check_wake_act_lig(end_time = 10, our_num = 7.72)
p_l_wak[[5]] <- p_l_wak[[5]] |> add_wake_change(w=7.45, z="D", c=5, a=7.72)
# Day 2
row <- 2
p_l_wak[[5]][row,] |> check_wake_act_lig(end_time = 10, our_num = 7)
p_l_wak[[5]] <- p_l_wak[[5]] |> add_wake_change(w=7,c=5, z="D", a=7)
# Day 3
row <- 3 
p_l_wak[[5]][row,] |> check_wake_act_lig(end_time=12, our_num=8)
p_l_wak[[5]] <- p_l_wak[[5]] |> add_wake_change(w=8,c=5,z="D",a=8)
# Day 4
row <- 4
p_l_wak[[5]][row,] |> check_wake_act_lig(end_time =10, our_num = 7.7)
p_l_wak[[5]] <- p_l_wak[[5]] |> add_wake_change(w =7.07, c=5, z="C",a=7.7)
# Day 5 
row <- 5
p_l_wak[[5]][row,] |> check_wake_act_lig(our_num =7.65)
p_l_wak[[5]] <- p_l_wak[[5]] |> add_wake_change(w=7.65, c=6, z="D", a=7.65)
# Day 6
row <- 6
p_l_wak[[5]][row,] |> check_wake_act_lig(end_time=12, our_num=6.6)
p_l_wak[[5]] <- p_l_wak[[5]] |> add_wake_change(w=6.6, c=6, z="D", a=6.6)

### 006       ======
# Day 1
row <- 1
p_l_wak[[6]][row,] |> check_wake_act_lig(end_time=11, our_num = 8.38)
p_l_wak[[6]] <- p_l_wak[[6]] |> add_wake_change(w=8.38, c=5, z="D",a=8.38)
# Day 2
row <- 2
p_l_wak[[6]][row,] |> check_wake_act_lig(our_num = 9.34)
p_l_wak[[6]] <- p_l_wak[[6]] |> add_wake_change(w = 9.34, z="D", c=6, a=9.34)
# Day 3
row <- 3
p_l_wak[[6]][row,] |> check_wake_act_lig(our_num = 8.33)
p_l_wak[[6]] <- p_l_wak[[6]] |> add_wake_change(w = 7.94, z="C", c=7, a=8.33)

### 007       ======
# Day 1
row <- 1
p_l_wak[[7]][row,] |> check_wake_act_lig(our_num = 8.88)
p_l_wak[[7]] <- p_l_wak[[7]] |> add_wake_change(w = 8.88, z="C", c=5,a=8.88)
# Day 2
row <- 2
p_l_wak[[7]][row,] |> check_wake_act_lig(our_num =8.23)
p_l_wak[[7]] <- p_l_wak[[7]] |> add_wake_change(w=8.23, z="D", c=6, a=p_l_wak[[7]][row, "sleep_wake_ag_h"][[1]])
# Day 3
row <- 3
p_l_wak[[7]][row,] |> check_wake_act_lig(our_num =8.97)
p_l_wak[[7]] <- p_l_wak[[7]] |> add_wake_change(w=8.97, c=6, z="C", a=8.97)

### 008       ======
# Day 1
row <- 1
p_l_wak[[8]][row,] |> check_wake_act_lig(end_time = 10, our_num = 7.95)
p_l_wak[[8]] <- p_l_wak[[8]] |> add_wake_change(w=9.1, z="D", c=6, a=7.95)
# Day 2
row <- 2
p_l_wak[[8]][row,] |> check_wake_act_lig(end_time = 10, our_num = 6.92)
p_l_wak[[8]] <- p_l_wak[[8]] |> add_wake_change(w=6.92, z="C", c=6, a=6.92)

### 009       ======
# Day 1
row <- 1
p_l_wak[[9]][row,] |> check_wake_act_lig(our_num =8.6)
p_l_wak[[9]] <- p_l_wak[[9]] |> add_wake_change(w=8.6, c=6, z="D",a = p_l_wak[[9]][row, "sleep_wake_ag_h"][[1]])
# Day 2
row <- 2
p_l_wak[[9]][row,] |> check_wake_act_lig(our_num = 8.82)
p_l_wak[[9]] <- p_l_wak[[9]] |> add_wake_change(w=8.82, z="C", c=6, a=8.82)
# Day 3
row <- 3
p_l_wak[[9]][row,] |> check_wake_act_lig(our_num = 7.77)
p_l_wak[[9]] <- p_l_wak[[9]] |> add_wake_change(w=7.78, z="D", c=5, a = 7.77)
# Day 4
row <- 4
p_l_wak[[9]][row,] |> check_wake_act_lig(end_time = 10, our_num = 7.55)
p_l_wak[[9]] <- p_l_wak[[9]] |> add_wake_change(w=7.55, z="D", c=5, a=7.55)
# Day 5
row <- 5
p_l_wak[[9]][row,] |> check_wake_act_lig(our_num = 7.67)
p_l_wak[[9]] <- p_l_wak[[9]] |> add_wake_change(w=7.67, z="D", c=6, a=7.67)

### 010       ======
# Day 1
row <- 1
p_l_wak[[10]][row,] |> check_wake_act_lig(our_num = 7.62)
p_l_wak[[10]] <- p_l_wak[[10]] |> add_wake_change(w=7.62, z="C", c=7, a=7.62)

# Day 2
row <- 2
p_l_wak[[10]][row,] |> check_wake_act_lig(end_time = 12, our_num = 8.86)
p_l_wak[[10]] <- p_l_wak[[10]] |> add_wake_change(w=8.86, z="D", c=5, a=8.86)

# Day 3
row <- 3
p_l_wak[[10]][row,] |> check_wake_act_lig(end_time = 10, our_num = 7.75)
p_l_wak[[10]] <- p_l_wak[[10]] |> add_wake_change(w=7.75, z="D", c=6, a=7.75)

### 011       ======
# Day 1
row <- 1
p_l_wak[[11]][row,] |> check_wake_act_lig(our_num = 10.3)
p_l_wak[[11]] <- p_l_wak[[11]] |> add_wake_change(w=10.3, z="D", c=6, a=10.3)

# Day 2
row <- 2
p_l_wak[[11]][row,] |> check_wake_act_lig(our_num = 9.39)
p_l_wak[[11]] <- p_l_wak[[11]] |> add_wake_change(w=9.39, z="D", c=6, a=9.39)

# Day 3
row <- 3
p_l_wak[[11]][row,] |> check_wake_act_lig(our_num = 9.12)
p_l_wak[[11]] <- p_l_wak[[11]] |> add_wake_change(w=9.15, z="D", c=7, a=9.12)

# Day 4
row <- 4
p_l_wak[[11]][row,] |> check_wake_act_lig(end_time = 12, our_num = 10.12)
p_l_wak[[11]] <- p_l_wak[[11]] |> add_wake_change(w=10.12, z="D", c=7, a=10.12)

### 012       ========
# Day 1
row <- 1
p_l_wak[[12]][row,] |> check_wake_act_lig(end_time = 10, our_num = 7.43)
p_l_wak[[12]] <- p_l_wak[[12]] |> add_wake_change(w=7.43, z="D", c=6, a=7.43)
# Day 2
row <- 2
p_l_wak[[12]][row,] |> check_wake_act_lig(end_time = 9, our_num = 7.65)
p_l_wak[[12]] <- p_l_wak[[12]] |> add_wake_change(w=7.65, z="D", c=6, a=7.65)
# Day 3
row <- 3
p_l_wak[[12]][row,] |> check_wake_act_lig(our_num = 4.92)
p_l_wak[[12]] <- p_l_wak[[12]] |> add_wake_change(w=4.95, z="D", c=6, a=4.92)
# Day 4
row <- 4
p_l_wak[[12]][row,] |> check_wake_act_lig(end_time = 10, our_num = 6.99)
p_l_wak[[12]] <- p_l_wak[[12]] |> add_wake_change(w=6.99, z="D", c=6, a=6.99)
# Day 5
row <- 5
p_l_wak[[12]][row,] |> check_wake_act_lig(our_num = 6.39)
p_l_wak[[12]] <- p_l_wak[[12]] |> add_wake_change(w=6.4, z="D", c=6, a=6.39)

### 013       ========
# Day 1
row <- 1
p_l_wak[[13]][row,] |> check_wake_act_lig(our_num  =6.48)
p_l_wak[[13]] <- p_l_wak[[13]] |> add_wake_change(w=6.45, z="C", c=7, a=6.48)
# Day 2
row <- 2
p_l_wak[[13]][row,] |> check_wake_act_lig(our_num = 8.99)
p_l_wak[[13]] <- p_l_wak[[13]] |> add_wake_change(w=8.99, z="C", c=6, a=8.99)
# Day 3
row <- 3
p_l_wak[[13]][row,] |> check_wake_act_lig(our_num = 6.45)
p_l_wak[[13]] <- p_l_wak[[13]] |> add_wake_change(w=6.45, z="D", c=6, a=6.45)
# Day 4
row <- 4
p_l_wak[[13]][row,] |> check_wake_act_lig(our_num = 6.87)
p_l_wak[[13]] <- p_l_wak[[13]] |> add_wake_change(w=6.46, z="D", c=6, a=6.46)

### 014       ========
# Day 1
row <- 1
p_l_wak[[14]][row,] |> check_wake_act_lig(our_num  =6.2)
p_l_wak[[14]] <- p_l_wak[[14]] |> add_wake_change(w=6.2, z="D", c=6, a=6.11)
# Day 2
row <- 2
p_l_wak[[14]][row,] |> check_wake_act_lig(our_num = 7.95)
p_l_wak[[14]] <- p_l_wak[[14]] |> add_wake_change(w=7.59, z="C", c=6, a=7.95)
# Day 3
row <- 3
p_l_wak[[14]][row,] |> check_wake_act_lig(end_time = 8, our_num = 4.9)
p_l_wak[[14]] <- p_l_wak[[14]] |> add_wake_change(w=4.9, z="D", c=4, a=4.9)

### 015       ========
# Day 1
row <- 1
p_l_wak[[15]][row,] |> check_wake_act_lig(end_time = 9, our_num = 6.2)
p_l_wak[[15]] <- p_l_wak[[15]] |> add_wake_change(w=6.2, z="D", c=6, a=6.2)
# Day 2
row <- 2
p_l_wak[[15]][row,] |> check_wake_act_lig(end_time = 11, our_num = 9.45)
p_l_wak[[15]] <- p_l_wak[[15]] |> add_wake_change(w=9.5, z="D", c=5, a=9.45)
# Day 3
row <- 3
p_l_wak[[15]][row,] |> check_wake_act_lig(end_time = 11, our_num = 8.9)
p_l_wak[[15]] <- p_l_wak[[15]] |> add_wake_change(w=8.9, z="D", c=5, a=8.9)
# Day 4
# DATA IS MISSING HERE, DO NOT SET
row <- 4
p_l_wak[[15]][row,] |> check_wake_act_lig(end_time = 12, our_num = 6.85)
p_l_wak[[15]] <- p_l_wak[[15]] |> add_wake_change(w=6.85, z="D-NO", c=6, a=p_l_wak[[15]][row,"sleep_wake_ag_h"][[1]])
# Day 5
row <- 5
p_l_wak[[15]][row,] |> check_wake_act_lig(end_time = 11, our_num = 9.46)
p_l_wak[[15]] <- p_l_wak[[15]] |> add_wake_change(w=9.5, z="D-NO", c=6, a=p_l_wak[[15]][row,"sleep_wake_ag_h"][[1]])

### 016       ======
# Day 1
row <- 1
p_l_wak[[16]][row,] |> check_wake_act_lig(start_time=5, end_time = 9, our_num = 7.38)
p_l_wak[[16]] <- p_l_wak[[16]] |> add_wake_change(w=7.38,  c=6, z="D-NO",a=p_l_wak[[15]][row,"sleep_wake_ag_h"][[1]])
# Day 2
row <- 2
p_l_wak[[16]][row,] |> check_wake_act_lig(our_num = 9.37)
p_l_wak[[16]] <- p_l_wak[[16]] |> add_wake_change(w=8.9, c=6, z="C",a=p_l_wak[[15]][row,"sleep_wake_ag_h"][[1]])
# Day 3
row <- 3
p_l_wak[[16]][row,] |> check_wake_act_lig(our_num  =7.22)
p_l_wak[[16]] <- p_l_wak[[16]] |> add_wake_change(w=7.22, c=5, z="D-NO",a=p_l_wak[[15]][row,"sleep_wake_ag_h"][[1]])
# Day 4
row <- 4
p_l_wak[[16]][row,] |> check_wake_act_lig(our_num  =7.5, end_time = 10)
p_l_wak[[16]] <- p_l_wak[[16]] |> add_wake_change(w=7.5, c=6, z="D-NO", a=p_l_wak[[15]][row,"sleep_wake_ag_h"][[1]])


### 017  ?????      ======
# SOMETHING IS WRONG HERE
actigraphy_activity  |> 
  filter(subj == "017") |>
  ggplot(aes(as.numeric(time)/3600, Activity)) + 
  facet_wrap(~date2) +
  geom_line()
# Day 1
row <- 1
p_l_wak[[17]][row,] |> check_wake_act_lig(our_num = 13, start_time = 3)
p_l_wak[[17]] <- p_l_wak[[17]] |> add_wake_change(w=13, c=6, z="D",a=13)
p_l_wak[[17]] <- p_l_wak[[17]] |> add_wake_change(w=NA, c=NA, z=NA,a=NA)


### 018 - NO       =======
p_l_wak[[18]]
#none#
### 019  what      =====
# Day 1
row <- 1
p_l_wak[[19]][row,] |> check_wake_act_lig(start_time = 5, end_time = 10, our_num = 8.75)
p_l_wak[[19]] <- p_l_wak[[19]] |> add_wake_change(w=8.75, z="D-NO", c=3, a=8.75)

# What is going on here
actigraphy_activity |>
  filter(subj=="019", )|>
  ggplot(aes(as.numeric(time)/3600, Activity)) +
  facet_wrap(~date2) +
  geom_line()

### 020       ======
# Day 1
row <- 1
p_l_wak[[20]][row,] |> check_wake_act_lig(our_num = 7.)
p_l_wak[[20]] <- p_l_wak[[20]] |> add_wake_change(w=7.5,c=7, z="C",a=7.5)
# Day 2
row <- 2
p_l_wak[[20]][row,] |> check_wake_act_lig(our_num = 7.68)
p_l_wak[[20]] <- p_l_wak[[20]] |> add_wake_change(w=7.68, c=5, z="D-NO", a=p_l_wak[[20]][row,"sleep_wake_ag_h"][[1]])

### 021       ======
# Day 1
row <- 1
p_l_wak[[21]][row,] |> check_wake_act_lig(end_time = 10, our_num = 8.65)
p_l_wak[[21]] <- p_l_wak[[21]] |> add_wake_change(w=8.65, z="D", c=5, a = 8.62)

### 022       =====
# Day 1
row <- 1
p_l_wak[[22]][row,] |> check_wake_act_lig(end_time = 13,our_num = 8.9)
p_l_wak[[22]] <- p_l_wak[[22]] |> add_wake_change(w=8.9, z="C", c=4, a=8.9)
# Day 2
row <- 2
p_l_wak[[22]][row,] |> check_wake_act_lig(our_num = 8.73)
p_l_wak[[22]] <- p_l_wak[[22]] |> add_wake_change(w=8.73, z="D-NO", c=6, a=p_l_wak[[22]][row,"sleep_wake_ag_h"][[1]])
# Day 3
row <- 3
p_l_wak[[22]][row,] |> check_wake_act_lig(our_num = 9.48)
p_l_wak[[22]] <- p_l_wak[[22]] |> add_wake_change(w=9.48, z="D", c=6, a=9.48)
# Day 4
row <- 4
p_l_wak[[22]][row,] |> check_wake_act_lig(end_time = 14, our_num = 12.11)
p_l_wak[[22]] <- p_l_wak[[22]] |> add_wake_change(w=12.11, z="D", c=5, a=12.11)
# Day 5
row <- 5
p_l_wak[[22]][row,] |> check_wake_act_lig(our_num = 8.5)
p_l_wak[[22]] <- p_l_wak[[22]] |> add_wake_change(w=8.5, z="C", c=6, a=8.5)

### 023       =====
# Day 1
row <- 1
p_l_wak[[23]][row,] |> check_wake_act_lig(our_num = 6.3)
p_l_wak[[23]] <- p_l_wak[[23]] |> add_wake_change(w=6.3, z="D-NO", c=7, a=p_l_wak[[23]][row,"sleep_wake_ag_h"][[1]])
# Day 2
row <- 2
p_l_wak[[23]][row,] |> check_wake_act_lig(end_time = 10, our_num = 7.0)
p_l_wak[[23]] <- p_l_wak[[23]] |> add_wake_change(w=7., z="D", c=7, a=7)
# Day 3
row <- 3
p_l_wak[[23]][row,] |> check_wake_act_lig(our_num = 6.8)
p_l_wak[[23]] <- p_l_wak[[23]] |> add_wake_change(w=6.8, z="D", c=5, a=6.8)
# Day 4
row <- 4
p_l_wak[[23]][row,] |> check_wake_act_lig(end_time = 10, our_num = 7.43)
p_l_wak[[23]] <- p_l_wak[[23]] |> add_wake_change(w=7.43, z="D", c=6, a=7.43)
### 024       =====
# Day 1
row <- 1
p_l_wak[[24]][row,] |> check_wake_act_lig(end_time = 9, our_num = 6.8)
p_l_wak[[24]] <- p_l_wak[[24]] |> add_wake_change(w=6.8, z="D", c=7, a=6.8)

### 025       =====
# Day 1
row <- 1
p_l_wak[[25]][row,] |> check_wake_act_lig(our_num = 8.89)
p_l_wak[[25]] <- p_l_wak[[25]] |> add_wake_change(w=8.89, z="C", c=7, a=8.89)
# Day 2
row <- 2
p_l_wak[[25]][row,] |> check_wake_act_lig(our_num = 8.93)
p_l_wak[[25]] <- p_l_wak[[25]] |> add_wake_change(w=8.93, z="D-NO", c=5, a=p_l_wak[[25]][row,"sleep_wake_ag_h"][[1]])
### 026       =====
# Day 1
row <- 1
p_l_wak[[26]][row,] |> check_wake_act_lig(end_time =15, our_num = 12.2)
p_l_wak[[26]] <- p_l_wak[[26]] |> add_wake_change(w=12.2, z="D-NO", c=4, a=p_l_wak[[26]][row,"sleep_wake_ag_h"][[1]])
# Day 2
row <- 2
p_l_wak[[26]][row,] |> check_wake_act_lig(our_num = 8.75)
p_l_wak[[26]] <- p_l_wak[[26]] |> add_wake_change(w=8.75, z="C", c=7, a=9)
# Day 3
row <- 3
p_l_wak[[26]][row,] |> check_wake_act_lig(our_num = 10.3)
p_l_wak[[26]] <- p_l_wak[[26]] |> add_wake_change(w=10.3, z="D", c=4, a=10.3)
# Day 4
row <- 4
p_l_wak[[26]][row,] |> check_wake_act_lig(our_num = 8, start_time = 5)
p_l_wak[[26]] <- p_l_wak[[26]] |> add_wake_change(w=8, z="D", c=5, a=p_l_wak[[26]][row,"sleep_wake_ag_h"][[1]])
# Day 5
row <- 5
p_l_wak[[26]][row,] |> check_wake_act_lig(our_num = 9.5)
p_l_wak[[26]] <- p_l_wak[[26]] |> add_wake_change(w=9.5, z="D", c=6, a=p_l_wak[[26]][row,"sleep_wake_ag_h"][[1]])

# Day 6
row <- 6
p_l_wak[[26]][row,] |> check_wake_act_lig(our_num = 10.72)
p_l_wak[[26]] <- p_l_wak[[26]] |> add_wake_change(w=10.72, z="D", c=6, a=10.72)

### 027       =====
# Day 1
row <- 1
p_l_wak[[27]][row,] |> check_wake_act_lig(our_num = 7.86)
p_l_wak[[27]] <- p_l_wak[[27]] |> add_wake_change(w=7.86, z="C", c=7, a=7.86)
# Day 2
row <- 2
p_l_wak[[27]][row,] |> check_wake_act_lig(our_num = 7.69)
p_l_wak[[27]] <- p_l_wak[[27]] |> add_wake_change(w=7.69, z="C", c=7, a=7.69)

### 028       =====
# Day 1
row <- 1
p_l_wak[[28]][row,] |> check_wake_act_lig(our_num  =9.46)
p_l_wak[[28]] <- p_l_wak[[28]] |> add_wake_change(w=9.46, z="D", c=7, a=9.46)
# Day 2
row <- 2
p_l_wak[[28]][row,] |> check_wake_act_lig(our_num  =7, start_time = 6)
p_l_wak[[28]] <- p_l_wak[[28]] |> add_wake_change(w=7, z="C", c = 6, a= 7)
# Day 3
row <- 3
p_l_wak[[28]][row,] |> check_wake_act_lig(our_num = 9.72)
p_l_wak[[28]] <- p_l_wak[[28]] |> add_wake_change(w=9.72, z="D", c=6, a=9.72)

### 029       =====
#' NOTE. there is lacking data, but the fact that the watch "stopped" record
#' suggests that the watch was removed, and therefore the person much have been awake....
# Day 1
row <- 1
p_l_wak[[29]][row,] |> check_wake_act_lig(our_num = 7.93)
p_l_wak[[29]] <- p_l_wak[[29]] |> add_wake_change(w=7.93, z="C", c=6, a=7.93)
### 030 -non      =====
# none
### 031 -non      =====
# none
### 032       ======
# day 1
row <- 1
p_l_wak[[032]][row,] |> check_wake_act_lig(our_num = 8.7)
p_l_wak[[032]] <- p_l_wak[[32]] |> add_wake_change(w=8.7, z="D", c=5, a=p_l_wak[[032]][row,"sleep_wake_ag_h"][[1]])
# day 2
row <- 2
p_l_wak[[032]][row,] |> check_wake_act_lig(our_num = 7.75)
p_l_wak[[032]] <- p_l_wak[[32]] |> add_wake_change(w=7.75, z="D", c=6, a=p_l_wak[[032]][row,"sleep_wake_ag_h"][[1]])
# day 3
row <- 3
p_l_wak[[032]][row,] |> check_wake_act_lig(our_num = 8.4)
p_l_wak[[032]] <- p_l_wak[[32]] |> add_wake_change(w=8.42, z="D", c=6, a=8.4)
# day 4
row <- 4
p_l_wak[[032]][row,] |> check_wake_act_lig(end_time =10, our_num = 6.58)
p_l_wak[[032]] <- p_l_wak[[32]] |> add_wake_change(w=6.58, z="D", c=6, a=6.58)
### 033       ======
# day 1
row <- 1
p_l_wak[[033]][row,] |> check_wake_act_lig(our_num  =8.95)
p_l_wak[[033]] <- p_l_wak[[33]] |> add_wake_change(w=8.95, z="C", c=7, a=8.95)
# day 2
row <- 2
p_l_wak[[033]][row,] |> check_wake_act_lig(our_num  =13.45)
p_l_wak[[033]] <- p_l_wak[[33]] |> add_wake_change(w=13.45, z="D", c=3, a=13.45)
# day 3  --- ????
row <- 3
p_l_wak[[033]][row,] |> check_wake_act_lig(our_num = 10.75, end_time = 13)
p_l_wak[[033]] <- p_l_wak[[33]] |> add_wake_change(w=10.75, z="D", c=2, a = p_l_wak[[033]][row,"sleep_wake_ag_h"][[1]])

# actigraphy_activity |>
#   filter(subj=="033", date2=="2022-03-21") |> view()

### 034       ======
# day 1
row <- 1
p_l_wak[[034]][row,] |> check_wake_act_lig(our_num = 11.77)
p_l_wak[[034]] <- p_l_wak[[34]] |> add_wake_change(w=11.77, z="D-NO", c=3, a=p_l_wak[[034]][row,"sleep_wake_ag_h"][[1]])
# day 2
row <- 2
p_l_wak[[034]][row,] |> check_wake_act_lig(our_num = 10.35)
p_l_wak[[034]] <- p_l_wak[[34]] |> add_wake_change(w=10.35, z="D", c=6, a=10.5)
# day 3
row <- 3
p_l_wak[[034]][row,] |> check_wake_act_lig(our_num  =8.3)
p_l_wak[[034]] <- p_l_wak[[34]] |> add_wake_change(w=8.3, z="D-NO", c=5, a=p_l_wak[[034]][row,"sleep_wake_ag_h"][[1]])
# day 4
row <- 4
p_l_wak[[034]][row,] |> check_wake_act_lig(our_num = 9.83)
p_l_wak[[034]] <- p_l_wak[[34]] |> add_wake_change(w=9.83, z="D-NO", c=6, a=p_l_wak[[034]][row,"sleep_wake_ag_h"][[1]])

## Check onset      ======

#' When it comes to the actigraphy. The software automatically use the "sleep time"
#' as the previous night. I.e., if you fell asleep on 09.09, then it will use the 
#' previous night 08.09 to estimate the sleep onset. This may work on an 18 to 18 
#' clock basis, instead of a 0-24 hours, exactly because people usually go to sleep
#' late during the day.
#' 
#' Because of this, the "relevant actigraphy" needs to contain the *previous day*
#' which will be used
  
p_l_on <- list()
p_l_on <- 
  map(actigraphy$subj|>unique(), \(x){
    print(x)
    p_l_on[[as.numeric(x)]] <- 
      sleeptimes_check |>
      filter(subj==x, check_onset) |> 
      select(subj, date, sleep_onset_ag_h, sleep_time_cum, tried_to_sleep_cum) |> 
      mutate(onset_time_adj=NA, onset_conf=NA, onset_zone=NA, onset_folleso_adj = NA) |>
      unique() 
  })


### FUNCS =====
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


### 001 - non      ======
### 002         ======
# day 1
row <- 1
p_l_on[[2]][row,] |> check_onset_act_lig(our = 22.67)
p_l_on[[2]] <- p_l_on[[2]] |> add_onset_change(o=22.65, c=7, z="C", a=22.67)

### 003         ======
# day 1
row <- 1
p_l_on[[3]][row,] |> check_onset_act_lig(our=24.12)
p_l_on[[3]] <- p_l_on[[3]] |> add_onset_change(o = 24.12, c=5, z="C", a = 24.12)
# day 2
row <- 2
p_l_on[[3]][row,] |> check_onset_act_lig(our = 26.4)
p_l_on[[3]] <- p_l_on[[3]] |> add_onset_change(o=26.4, c=6, z="C", a = 26.33)

### 004         ======
# day 1
row <- 1
p_l_on[[4]][row,] |> check_onset_act_lig(our = 22.87)
p_l_on[[4]] <- p_l_on[[4]] |> add_onset_change(o = 22.87, c=5, z="D-NO", a=p_l_on[[4]][row,"sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[4]][row,] |> check_onset_act_lig(our = 23.74)
p_l_on[[4]] <- p_l_on[[4]] |> add_onset_change(o = 23.74, c=7, z="D-NO", a=p_l_on[[4]][row,"sleep_onset_ag_h"][[1]])

### 005         ======
# day 1
row <- 1
p_l_on[[5]][row,] |> check_onset_act_lig(our = 22.35)
p_l_on[[5]] <- p_l_on[[5]] |> add_onset_change(o = 22.35, c=5, z="D-NO", a=p_l_on[[5]][row, "sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[5]][row,] |> check_onset_act_lig(our = 24.38)
p_l_on[[5]] <- p_l_on[[5]] |> add_onset_change(o = 24.38, c=6, z="D-NO", a=p_l_on[[5]][row, "sleep_onset_ag_h"][[1]])
# day 3
row <- 3
p_l_on[[5]][row,] |> check_onset_act_lig(our = 24.92)
p_l_on[[5]] <- p_l_on[[5]] |> add_onset_change(o = 24.92, c=7, z="D-NO", a=p_l_on[[5]][row, "sleep_onset_ag_h"][[1]])
# day 4
row <- 4
p_l_on[[5]][row,] |> check_onset_act_lig(our = 22)
p_l_on[[5]] <- p_l_on[[5]] |> add_onset_change(o=22, c=6, z="D-NO", a=p_l_on[[5]][row, "sleep_onset_ag_h"][[1]])
# day 5
row <- 5
p_l_on[[5]][row,] |> check_onset_act_lig(our = 23.18)
p_l_on[[5]] <- p_l_on[[5]] |> add_onset_change(o=23.3, c=7, z="C", a=23.18)
# day 6
row <- 6
p_l_on[[5]][row,] |> check_onset_act_lig(our = 22.7)
p_l_on[[5]] <- p_l_on[[5]] |> add_onset_change(o=22.7, c=6, z="D", a=p_l_on[[5]][row, "sleep_onset_ag_h"][[1]])


### 006         ======
# day 1
row <- 1
p_l_on[[6]][row,] |> check_onset_act_lig(our = 26.23)
p_l_on[[6]] <- p_l_on[[6]] |> add_onset_change(o = 26.23, c=7, z="D-NO", a=p_l_on[[6]][row, "sleep_onset_ag_h"][[1]])
                                               #=
# day 2
row <- 2
p_l_on[[6]][row,] |> check_onset_act_lig(our=26.93)
p_l_on[[6]] <- p_l_on[[6]] |> add_onset_change(o = 26.93, c=5, z="C", a=26.93)
                                               #=p_l_on[[5]][row, "sleep_onset_ag_h"][[1]])
# day 3
row <- 3
p_l_on[[6]][row,] |> check_onset_act_lig(our = 24.9)
p_l_on[[6]] <- p_l_on[[6]] |> add_onset_change(o = 24.9, c=5, z="D-NO", a=25)
                                               #=p_l_on[[5]][row, "sleep_onset_ag_h"][[1]])
# day 4
row <- 4
p_l_on[[6]][row,] |> check_onset_act_lig(our = 25)
p_l_on[[6]] <- p_l_on[[6]] |> add_onset_change(o = 25, c=4, z="D-NO", a=p_l_on[[6]][row, "sleep_onset_ag_h"][[1]])
                                               #=p_l_on[[5]][row, "sleep_onset_ag_h"][[1]])
### 007         ======
# day 1
row <- 1
p_l_on[[7]][row,] |> check_onset_act_lig(our =27.33)
p_l_on[[7]] <- p_l_on[[7]] |> add_onset_change(o = 27.33, c=6, z="D-NO", a=p_l_on[[7]][row, "sleep_onset_ag_h"][[1]])

# day 2
row <- 2
p_l_on[[7]][row,] |> check_onset_act_lig(our = 24.38)
p_l_on[[7]] <- p_l_on[[7]] |> add_onset_change(o = 24.38, c=5, z="D-NO", a=p_l_on[[7]][row, "sleep_onset_ag_h"][[1]])
                                               
# day 3
row <- 3
p_l_on[[7]][row,] |> check_onset_act_lig(our=25.82)
p_l_on[[7]] <- p_l_on[[7]] |> add_onset_change(o = 25.82, c=6, z="D-NO", a=p_l_on[[7]][row, "sleep_onset_ag_h"][[1]])

### 008 -no         ======
### 009         ======
# day 1
row <- 1
p_l_on[[9]][row,] |> check_onset_act_lig(our=26.75)
p_l_on[[9]] <- p_l_on[[9]] |> add_onset_change(o = 26.75, c=7, z="D-NO", a=p_l_on[[9]][row, "sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[9]][row,] |> check_onset_act_lig(our = 24)
p_l_on[[9]] <- p_l_on[[9]] |> add_onset_change(o = 24, c=2, z="D-NO", a=p_l_on[[9]][row, "sleep_onset_ag_h"][[1]])
# day 3
row <- 3
p_l_on[[9]][row,] |> check_onset_act_lig(our = 26.67)
p_l_on[[9]] <- p_l_on[[9]] |> add_onset_change(o = 26.67, c=4, z="D-NO", a=26.67)
                                               #=
### 010         ======
# day 1
row <- 1
p_l_on[[10]][row,] |> check_onset_act_lig(our = 23.45)
p_l_on[[10]] <- p_l_on[[10]] |> add_onset_change(o = 23.45, c=6, z="D", a=p_l_on[[10]][row, "sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[10]][row,] |> check_onset_act_lig(our = 24.02)
p_l_on[[10]] <- p_l_on[[10]] |> add_onset_change(o = 24.02, c=6, z="D-NO", a=p_l_on[[10]][row, "sleep_onset_ag_h"][[1]])
# day 3
row <- 3
p_l_on[[10]][row,] |> check_onset_act_lig(our = 24.1)
p_l_on[[10]] <- p_l_on[[10]] |> add_onset_change(o = 24.1, c=4, z="D-NO", a=p_l_on[[10]][row, "sleep_onset_ag_h"][[1]])
# day 4
row <- 4
p_l_on[[10]][row,] |> check_onset_act_lig(our = 26.58)
p_l_on[[10]] <- p_l_on[[10]] |> add_onset_change(o = 26.58, c=6, z="D", a=p_l_on[[10]][row, "sleep_onset_ag_h"][[1]])

### 011         ======
# day 1
row <- 1
p_l_on[[11]][row,] |> check_onset_act_lig(our = 23.45)
p_l_on[[11]] <- p_l_on[[11]] |> add_onset_change(o = 24.45, c=5, z="D-NO", a=p_l_on[[11]][row, "sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[11]][row,] |> check_onset_act_lig(our = 24.02)
p_l_on[[11]] <- p_l_on[[11]] |> add_onset_change(o = 24.02, c=6, z="D-NO", a=p_l_on[[11]][row, "sleep_onset_ag_h"][[1]])
# day 3
row <- 3
p_l_on[[11]][row,] |> check_onset_act_lig(our = 24.1)
p_l_on[[11]] <- p_l_on[[11]] |> add_onset_change(o = 24.1, c=6, z="D-NO", a=p_l_on[[11]][row, "sleep_onset_ag_h"][[1]])

### 012         =======
# day 1
row <- 1
p_l_on[[12]][row,] |> check_onset_act_lig(our = 26.2, end_time = 28)
p_l_on[[12]] <- p_l_on[[12]] |> add_onset_change(o = 26.2, c=4, z="D", a=26.2)
# day 2
row <- 2
p_l_on[[12]][row,] |> check_onset_act_lig(our = 26.12)
p_l_on[[12]] <- p_l_on[[12]] |> add_onset_change(o = 26.12, c=5, z="D", a=26.12)
# day 3
row <- 3
p_l_on[[12]][row,] |> check_onset_act_lig(our = 26.2)
p_l_on[[12]] <- p_l_on[[12]] |> add_onset_change(o = 26.2, c=6, z="C", a=26.2)
# day 4
row <- 4
p_l_on[[12]][row,] |> check_onset_act_lig(our=22.35)
p_l_on[[12]] <- p_l_on[[12]] |> add_onset_change(o = 22.35, c=7, z="D", a=22.35)
# day 5
row <- 5
p_l_on[[12]][row,] |> check_onset_act_lig(our = 22.75)
p_l_on[[12]] <- p_l_on[[12]] |> add_onset_change(o = 22.75, c=6, z="D-NO", a=p_l_on[[12]][row, "sleep_onset_ag_h"][[1]])


### 013         ======
# day 1
row <- 1
p_l_on[[13]][row,] |> check_onset_act_lig(our = 23.95)
p_l_on[[13]] <- p_l_on[[13]] |> add_onset_change(o = 24.08, c=7, z="D", a=p_l_on[[13]][row, "sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[13]][row,] |> check_onset_act_lig(our = 22.34)
p_l_on[[13]] <- p_l_on[[13]] |> add_onset_change(o = 22.34, c=7, z="C", a=22.34)
# day 3
row <- 3
p_l_on[[13]][row,] |> check_onset_act_lig(our = 22.8)
p_l_on[[13]] <- p_l_on[[13]] |> add_onset_change(o = 22.8, c=6, z="D-NO", a=p_l_on[[12]][row, "sleep_onset_ag_h"][[1]])
# day 4
row <- 4
p_l_on[[13]][row,] |> check_onset_act_lig(our = 25.1)
p_l_on[[13]] <- p_l_on[[13]] |> add_onset_change(o = 25.1, c=6, z="D-NO", a=p_l_on[[12]][row, "sleep_onset_ag_h"][[1]])
# day 5
row <- 5
p_l_on[[13]][row,] |> check_onset_act_lig(our = 25.4)
p_l_on[[13]] <- p_l_on[[13]] |> add_onset_change(o = 25.4, c=7, z="D-NO", a=p_l_on[[12]][row, "sleep_onset_ag_h"][[1]])

### 014         ======
# day 1
row <- 1
p_l_on[[14]][row,] |> check_onset_act_lig(our = 26.3)
p_l_on[[14]] <- p_l_on[[14]] |> add_onset_change(o = 26.3, c=7, z="D-NO", a=26.3)
# day 2
row <- 2
p_l_on[[14]][row,] |> check_onset_act_lig(our = 25.1)
p_l_on[[14]] <- p_l_on[[14]] |> add_onset_change(o = 25.1, c=7, z="D-NO", a=25.1)
# day 3
row <- 3
p_l_on[[14]][row,] |> check_onset_act_lig(our = 25.16)
p_l_on[[14]] <- p_l_on[[14]] |> add_onset_change(o = 25.16, c=6, z="D-NO", a=25.1)

### 015         ======
# day 1
row <- 1
p_l_on[[15]][row,] |> check_onset_act_lig(our = 23.9)
p_l_on[[15]] <- p_l_on[[15]] |> add_onset_change(o = 23.9, c=7, z="D-NO", a=p_l_on[[15]][row, "sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[15]][row,] |> check_onset_act_lig(our = 24.56)
p_l_on[[15]] <- p_l_on[[15]] |> add_onset_change(o = 24.56, c=7, z="D-NO", a=p_l_on[[15]][row, "sleep_onset_ag_h"][[1]])
# day 3
row <- 3
p_l_on[[15]][row,] |> check_onset_act_lig(our = 24.15)
p_l_on[[15]] <- p_l_on[[15]] |> add_onset_change(o = 24.15, c=7, z="D-NO", a=p_l_on[[15]][row, "sleep_onset_ag_h"][[1]])
### 016         ======
# day 1
row <- 1
p_l_on[[16]][row,] |> check_onset_act_lig(our = 22.12, end_time = 26)
p_l_on[[16]] <- p_l_on[[16]] |> add_onset_change(o = 22.12, c=3, z="D-NO", a=p_l_on[[16]][row, "sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[16]][row,] |> check_onset_act_lig(our = 22.02)
p_l_on[[16]] <- p_l_on[[16]] |> add_onset_change(o = 22.02, c=3, z="D-NO", a=p_l_on[[16]][row, "sleep_onset_ag_h"][[1]])
# day 3
row <- 3
p_l_on[[16]][row,] |> check_onset_act_lig(our = 22.82)
p_l_on[[16]] <- p_l_on[[16]] |> add_onset_change(o = 22.82, c=6, z="D-NO", a=p_l_on[[16]][row, "sleep_onset_ag_h"][[1]])
# day 4
row <- 4
p_l_on[[16]][row,] |> check_onset_act_lig(our = 24.15)
p_l_on[[16]] <- p_l_on[[16]] |> add_onset_change(o = 24.15, c=7, z="D", a=24.15)
# day 5
row <- 5
p_l_on[[16]][row,] |> check_onset_act_lig(our = 23.85)
p_l_on[[16]] <- p_l_on[[16]] |> add_onset_change(o = 23.85, c=7, z="D-NO", a=p_l_on[[16]][row, "sleep_onset_ag_h"][[1]])
# day 6
row <- 6
p_l_on[[16]][row,] |> check_onset_act_lig(our=24.07)
p_l_on[[16]] <- p_l_on[[16]] |> add_onset_change(o = 24.07, c=5, z="D-NO", a=p_l_on[[16]][row, "sleep_onset_ag_h"][[1]])

### 017         ======
# day 1
row <- 1
p_l_on[[17]][row,] |> check_onset_act_lig(end_time = 45, our = 37)
p_l_on[[17]] <- p_l_on[[17]] |> add_onset_change(o = 37, c=3, z="D-NO", a=p_l_on[[17]][row, "sleep_onset_ag_h"][[1]])

### 018         ======
# day 1
row <- 1
p_l_on[[18]][row,] |> check_onset_act_lig(our = 26.75)
p_l_on[[18]] <- p_l_on[[18]] |> add_onset_change(o = 26.75, c=6, z="D-NO", a=p_l_on[[18]][row, "sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[18]][row,] |> check_onset_act_lig(our = 26.5)
p_l_on[[18]] <- p_l_on[[18]] |> add_onset_change(o = 26.5, c=2, z="D-NO", a=p_l_on[[18]][row, "sleep_onset_ag_h"][[1]])
# day 3
row <- 3
p_l_on[[18]][row,] |> check_onset_act_lig(our = 26.12)
p_l_on[[18]] <- p_l_on[[18]] |> add_onset_change(o = 26.12, c=7, z="D", a=p_l_on[[18]][row, "sleep_onset_ag_h"][[1]])
# day 4
row <- 4
p_l_on[[18]][row,] |> check_onset_act_lig(our = 23.75)
p_l_on[[18]] <- p_l_on[[18]] |> add_onset_change(o = 23.75, c=7, z="D", a=p_l_on[[18]][row, "sleep_onset_ag_h"][[1]])
# day 5
row <- 5
p_l_on[[18]][row,] |> check_onset_act_lig(our = 24.27)
p_l_on[[18]] <- p_l_on[[18]] |> add_onset_change(o = 24.27, c=4, z="D", a=24.27)

### 019 - no ag       ======
### 020         ======
row <- 1
p_l_on[[20]][row,] |> check_onset_act_lig(our = 24)
p_l_on[[20]] <- p_l_on[[20]] |> add_onset_change(o = 24, c=6, z="D", a=p_l_on[[20]][row, "sleep_onset_ag_h"][[1]])

### 021         =======
# day 1
row <- 1
p_l_on[[21]][row,] |> check_onset_act_lig(our = 24.55)
p_l_on[[21]] <- p_l_on[[21]] |> add_onset_change(o = 24.55, c=6, z="D", a=24.55)

### 022         ======
# day 1
row <- 1
p_l_on[[22]][row,] |> check_onset_act_lig(our = 28.25)
p_l_on[[22]] <- p_l_on[[22]] |> add_onset_change(o = 28.55, c=6, z="D", a=p_l_on[[22]][row, "sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[22]][row,] |> check_onset_act_lig(our = 24)
p_l_on[[22]] <- p_l_on[[22]] |> add_onset_change(o = 24, c=6, z="D-NO", a=24)

### 023         ========
# day 1
row <- 1
p_l_on[[23]][row,] |> check_onset_act_lig(our = 24.1)
p_l_on[[23]] <- p_l_on[[23]] |> add_onset_change(o = 24.1, c=6, z="C", a=24.1)
# day 2
row <- 2
p_l_on[[23]][row,] |> check_onset_act_lig(our = 26.35)
p_l_on[[23]] <- p_l_on[[23]] |> add_onset_change(o = 26.35, c=6, z="C", a=26.35)
# day 3
row <- 3
p_l_on[[23]][row,] |> check_onset_act_lig(our = 25.05)
p_l_on[[23]] <- p_l_on[[23]] |> add_onset_change(o = 25.05, c=4, z="C", a=25.5)

### 024         =======
# day 1
row <- 1
p_l_on[[24]][row,] |> check_onset_act_lig(our = 22.58)
p_l_on[[24]] <- p_l_on[[24]] |> add_onset_change(o = 24.4, c=6, z="D-NO", a=p_l_on[[24]][row,"sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[24]][row,] |> check_onset_act_lig(our = 23.05)
p_l_on[[24]] <- p_l_on[[24]] |> add_onset_change(o = 23.4, c=5, z="C", a=23.05)

### 025         =======
# day 1
row <- 1
p_l_on[[25]][row,] |> check_onset_act_lig(our = 23.81)
p_l_on[[25]] <- p_l_on[[25]] |> add_onset_change(o = 23.81, c=7, z="D-NO", a=p_l_on[[25]][row,"sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[25]][row,] |> check_onset_act_lig(our = 24.12)
p_l_on[[25]] <- p_l_on[[25]] |> add_onset_change(o = 24.12 , c=7, z="D-NO", a=p_l_on[[25]][row,"sleep_onset_ag_h"][[1]])

### 026         =======
# day 1
row <- 1
p_l_on[[26]][row,] |> check_onset_act_lig(our = 28.5)
p_l_on[[26]] <- p_l_on[[26]] |> add_onset_change(o = 28.5, c=7, z="D-NO", a=p_l_on[[25]][row,"sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[26]][row,] |> check_onset_act_lig(our = 28)
p_l_on[[26]] <- p_l_on[[26]] |> add_onset_change( o=28 , c=5, z="C", a=p_l_on[[26]][row,"sleep_onset_ag_h"][[1]] )
# day 3
row <- 3
p_l_on[[26]][row,] |> check_onset_act_lig(our = 27.45)
p_l_on[[26]] <- p_l_on[[26]] |> add_onset_change(o = 27.45 , c=6, z="D-NO", a=p_l_on[[26]][row,"sleep_onset_ag_h"][[1]])
# day 4
row <- 4
p_l_on[[26]][row,] |> check_onset_act_lig(our = 27.55)
p_l_on[[26]] <- p_l_on[[26]] |> add_onset_change(o = 27.55 , c=6, z="C", a=27.55)

### 027         =======
# day 1
row <- 1
p_l_on[[27]][row,] |> check_onset_act_lig(our = 24.55)
p_l_on[[27]] <- p_l_on[[27]] |> add_onset_change(o = 24.55, c=7, z="C", a=24.55)
# day 2
row <- 2
p_l_on[[27]][row,] |> check_onset_act_lig(our = 24.73)
p_l_on[[27]] <- p_l_on[[27]] |> add_onset_change(o = 24.73, c=7, z="C", a=24.73)
# day 3
row <- 3
p_l_on[[27]][row,] |> check_onset_act_lig(our = 27.5)
p_l_on[[27]] <- p_l_on[[27]] |> add_onset_change(o = 27.5, c=7, z="C", a=27.5)
# day 4
row <- 4
p_l_on[[27]][row,] |> check_onset_act_lig(our = 27.6)
p_l_on[[27]] <- p_l_on[[27]] |> add_onset_change(o = 27.6, c=7, z="C", a=27.6)

### 028         =======
# day 1
row <- 1
p_l_on[[28]][row,] |> check_onset_act_lig(our = 23)
p_l_on[[28]] <- p_l_on[[28]] |> add_onset_change(o = 22.65, c=5, z="C", a=23)
# day 2
row <- 2
p_l_on[[28]][row,] |> check_onset_act_lig(our = 21.9)
p_l_on[[28]] <- p_l_on[[28]] |> add_onset_change(o = 21.9, c=6, z="D-NO", a=p_l_on[[28]][row, "sleep_onset_ag_h"][[1]])
# day 3
row <- 3
p_l_on[[28]][row,] |> check_onset_act_lig(our = 18)
p_l_on[[28]] <- p_l_on[[28]] |> add_onset_change(o = 18, c=7, z="D-NO", a=p_l_on[[28]][row, "sleep_onset_ag_h"][[1]])
# day 4
row <- 4
p_l_on[[28]][row,] |> check_onset_act_lig(our = 26.45)
p_l_on[[28]] <- p_l_on[[28]] |> add_onset_change(o = 26.45, c=6, z="C", a=26.45)
# day 5
row <- 5
p_l_on[[28]][row,] |> check_onset_act_lig(our=25.51)
p_l_on[[28]] <- p_l_on[[28]] |> add_onset_change(o = 25.51, c=7, z="D-NO", a=p_l_on[[28]][row, "sleep_onset_ag_h"][[1]])
# day 6
row <- 6
p_l_on[[28]][row,] |> check_onset_act_lig(our = 24.9)
p_l_on[[28]] <- p_l_on[[28]] |> add_onset_change(o=24.9, c=7, z="D-NO", a=p_l_on[[28]][row, "sleep_onset_ag_h"][[1]])

### 029         =======
# day 1
row <- 1
p_l_on[[29]][row,] |> check_onset_act_lig(our = 24.92)
p_l_on[[29]] <- p_l_on[[29]] |> add_onset_change(o = 24.92, c=6, z="C", a=24.92)

### 030         =======
### 031         =======
# day 1
row <- 1
p_l_on[[31]][row,] |> check_onset_act_lig(our = 25.1)
p_l_on[[31]] <- p_l_on[[31]] |> add_onset_change(o = 25.1, c=6, z="D-NO", a=p_l_on[[31]][row, "sleep_onset_ag_h"][[1]])

### 032         =======
# day 1
row <- 1
p_l_on[[32]][row,] |> check_onset_act_lig(our = 23.95)
p_l_on[[32]] <- p_l_on[[32]] |> add_onset_change(o = 23.95, c=6, z="D-NO", a=p_l_on[[32]][row, "sleep_onset_ag_h"][[1]])
row <- 2
p_l_on[[32]][row,] |> check_onset_act_lig(our = 26.25)
p_l_on[[32]] <- p_l_on[[32]] |> add_onset_change(o = 25.88, c=6, z="D-NO", a=26.25)
row <- 3
p_l_on[[32]][row,] |> check_onset_act_lig(our = 24.27)
p_l_on[[32]] <- p_l_on[[32]] |> add_onset_change(o = 24.27, c=6, z="D-NO", a=p_l_on[[32]][row,"sleep_onset_ag_h"][[1]])
row <- 4
p_l_on[[32]][row,] |> check_onset_act_lig(our = 24.73)
p_l_on[[32]] <- p_l_on[[32]] |> add_onset_change(o = 24.73, c=6, z="D-NO", a=24.73)


### 033         =======
# day 1
row <- 1
p_l_on[[33]][row,] |> check_onset_act_lig(our = 28.75)
p_l_on[[33]] <- p_l_on[[33]] |> add_onset_change(o = 28.75, c=6, z="D-NO", a=p_l_on[[33]][row, "sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[33]][row,] |> check_onset_act_lig(our = 26.6)
p_l_on[[33]] <- p_l_on[[33]] |> add_onset_change(o = 26.6, c=6, z="D-NO", a=p_l_on[[33]][row, "sleep_onset_ag_h"][[1]])
# day 3
row <- 3
p_l_on[[33]][row,] |> check_onset_act_lig(our = 29.87)
p_l_on[[33]] <- p_l_on[[33]] |> add_onset_change(o = 29.87, c=6, z="D-NO", a=p_l_on[[33]][row, "sleep_onset_ag_h"][[1]])


### 034         =======
# day 1
row <- 1
p_l_on[[34]][row,] |> check_onset_act_lig(our = 24.85)
p_l_on[[34]] <- p_l_on[[34]] |> add_onset_change(o = 24.85, c=6, z="D-NO", a=p_l_on[[34]][row, "sleep_onset_ag_h"][[1]])
# day 2
row <- 2
p_l_on[[34]][row,] |> check_onset_act_lig(our = 27)
p_l_on[[34]] <- p_l_on[[34]] |> add_onset_change(o = 27, c=6, z="D-NO", a=p_l_on[[34]][row, "sleep_onset_ag_h"][[1]])
# day 3
row <- 3
p_l_on[[34]][row,] |> check_onset_act_lig(end_time = 27, our = 25.88)
p_l_on[[34]] <- p_l_on[[34]] |> add_onset_change(o = 25.88, c=6, z="D-NO", a=p_l_on[[34]][row, "sleep_onset_ag_h"][[1]])
# day 4
row <- 4
p_l_on[[34]][row,] |> check_onset_act_lig(our = 23.75)
p_l_on[[34]] <- p_l_on[[34]] |> add_onset_change(o = 23.75, c=5, z="D-NO", a=p_l_on[[34]][row, "sleep_onset_ag_h"][[1]])


## Save =====
save(p_l_on, p_l_wak, file = "data/sleep_adjustment-SA.rdata")
