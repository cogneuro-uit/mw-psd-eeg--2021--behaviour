library(shiny)
library(tidyverse)
library(patchwork)

# Parameters 
debug <- TRUE

# Load data     =======
print(getwd())
load("data/actigraphy_activity.RData")
load("data/actigraphy.RData")
load("data/sleep_diary.RData")


## Prep data        ======
# combine actigraphy and self-report data
sleeptimes <- left_join(sleep_diary, actigraphy, by=c("subj","date"="date_end_ag"))

#' Get out only the relevant days to check 
#' (the actigraphy data contains a lot of data, therefore we want to reduce the amout we work with)
sleeptimes_check <- 
  sleeptimes |>
  filter(!(subj=="030")) |>
  #' Subject 030 **does not** have actigraphy data, hence the person is removed.
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
  #' By subject because some participants have overlapping dates, and 
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

# Get only the relevant actigraphy data
relevant_actigraphy <- actigraphy_activity |>
  left_join(
    sleeptimes_check,
    by = join_by(subj==subj, date2==date)
  ) |> 
  filter( !is.na(check_wake) | !is.na(check_onset_k) ) 

## Check wake      =====
# List of participants (and days) to check
p_l_wak <- list()
p_l_wak <- map(actigraphy$subj|> unique(), \(x){
    print(x)
    p_l_wak[[as.numeric(x)]] <- 
      sleeptimes_check |>
      filter(subj==x, check_wake) |> 
      select(subj, date, sleep_wake_ag_h, last_awaking_fix) |> 
      mutate(wake_time_adj=NA, wake_zone=NA, wake_conf=NA, wake_falleso_adj = NA) |>
      unique() 
  })

## Check onset      ======
p_l_on <- list()
p_l_on <- map(actigraphy$subj|>unique(), \(x){
    print(x)
    p_l_on[[as.numeric(x)]] <- 
      sleeptimes_check |>
      filter(subj==x, check_onset) |> 
      select(subj, date, sleep_onset_ag_h, sleep_time_cum, tried_to_sleep_cum) |> 
      mutate(onset_time_adj=NA, onset_conf=NA, onset_zone=NA, onset_folleso_adj = NA) |>
      unique() 
  })

#  FRONT END        ======
ui <- fluidPage(
  tags$style(type="text/css", "#participant, #day { font-size: 20px; }"),
  
  titlePanel("Actigraphy Adjustmnets"),
  
  sidebarLayout(
    sidebarPanel(
      # Onset/wake
      selectInput( "select_onset_or_wake", "Wake or onset?", c("Wake", "Onset"), selected = "Wake" ),
      
      # Participant
      numericInput("participant", "Participant:", 1, min = 1, max = 34),
      
      # Day count 
      numericInput("day", "Day:", 1, min = 1, max = 7),
      
      # Set time
      sliderInput("set_hour", "Set the hour:", min = 1, max = 32, value = 5),
      sliderInput("set_minutes", "Set the minutes:", min = 0, max = 60, value = 0),
      
      # Confidence
      sliderInput("confidence", "Confidence in sleep adjustment:", min = 1, max = 7, value = 4),
      
      # Store action....
      fluidRow(
        titlePanel("What time values should be stored?"),
        actionButton("store_self_selected", "Use Sliders", width = "30%"),
        actionButton("use_actigraphy", "Use Actigraphy", width = "30%"),
        actionButton("use_self_report", "Use Self-reported", width = "30%"),
      ),
      
      # Export to .RData
      titlePanel("Export data"),
      actionButton("export_data", "Export data", width = "85%")
    ),
    
    # Main Part
    mainPanel(
      # Main plot
      plotOutput("plot"),
      
      # Text output / text
      titlePanel("Messages:"),
      textOutput("text_output"),
      
      titlePanel("If data is stored for this participant/day, it will show below:"),
      tableOutput("tbl_output"),
      height = "90%"
    ),
  )
)

#  SERVER      ======
server <- function(input, output, session) {
  # Time  
  v <- reactiveValues( time = 15, just_switch = FALSE,
                       participant = 1, day = 1)
  
  time_adj <- tibble(
    subj=NA, date=NA, time_adj = NA, conf = NA, wake_or_onset = NA)
  
  observeEvent(input$participant, {
    v$participant <- input$participant
    if(v$participant > 34) updateNumericInput(session, "participant", value=34)
    output$text_output <- renderText("ERROR:  CANNOT SELECT PARTICIPANT GREATER THAN 34 !")
  })
  
  observeEvent(input$day, {
    v$day <- input$day
    if(v$day > 6) updateNumericInput(session, "day", value=6)
    output$text_output <- renderText("ERROR:  CANNOT SELECT DAY GREATER THAN 6 !")
  })
  
  observeEvent(list(input$set_hour, input$set_minutes), {
    v$time <<- input$set_hour + input$set_minutes/60
    if(debug) cat("Day: ", v$time,"\n")
  })
  
  observeEvent(input$select_onset_or_wake, {
    v$just_switch <- TRUE
    cat("WHAT IS SWITCH NOW? ", v$just_switch, "\n")
    updateSliderInput(session, "participant", value = 1)
    updateSliderInput(session, "day", value = 1)
  })
  
  # Plot
  output$plot <- renderPlot({
    
    if(v$participant > 34){
      # reset to highest known 
      v$participant <- 34 
      v$day <- 4
      updateNumericInput(session, "participant", value=34)
      updateNumericInput(session, "day", value=4)
      output$text_output <- renderText("ERROR:  CANNOT SELECT PARTICIPANT GREATER THAN 34 !")
      
    }
    
    if(v$day > 6){
      v$day <- 6
      updateNumericInput(session, "day", value=6)
      output$text_output <- renderText("ERROR:  CANNOT SELECT DAY GREATER THAN 6 !")
    }
    
    cat("participant after:", v$participant)
    
    
    # Render plot based on wake or sleep condition
    ### Onset plot     =====
    if( input$select_onset_or_wake == "Onset" ){
      if(v$just_switch){
        updateSliderInput(session, "set_hour", value = 21)
        v$just_switch <- FALSE
      }
      
      # Get data
      data <- p_l_on[[ v$participant ]][ v$day, ]
      
      # If subject is missing increment participant by + 1
      if(is.na(data$subj)){
        # if automatically update participant, set day to 0
        updateNumericInput(session, "participant", value =  v$participant +1)
        if(v$day != 1){
          updateNumericInput(session, "day", value = 1)
        }
      }
      
      # Get values
      s_subj    <- data[["subj"]]
      date      <- data[["date"]] |> ymd()
      date_bfr  <- date-days(1)
      ag_on     <- data[["sleep_onset_ag_h"]]
      sr_on     <- data[["sleep_time_cum"]]
      attempt_sleep <- data[["tried_to_sleep_cum"]]
      
      # Start end time
      start_time <- floor(min(ag_on, sr_on) - .5)
      end_time <- ceiling(max(ag_on, sr_on) + .5)
      
      # Data filtering
      d <- relevant_actigraphy |>
        filter(subj == s_subj, (date2 == date | date2 == date_bfr)) |>
        mutate( 
          time2 = as.numeric(time)/3600,
          time2 = if_else(date2 == date_bfr, time2, time2 + 24 ) 
        ) |>
        filter( time2 >= start_time & time2 <= end_time ) |>
        pivot_longer( ends_with("Light") )
      
      print(d)
      # Activity PLOT
      o <- d |>
        ggplot(aes(time2, Activity)) +
        theme_bw() +
        labs(x = "Time (24hr)", y="Activity") +
        geom_line(alpha = .7)
      
      # Light PLOT
      o2 <- d |>
        ggplot(aes(time2, value)) +
        theme_bw() +
        labs(x = "Time (24hr)", y="Log(Light)") +
        scale_color_manual(values=c("blue","green","red","gray")) +
        scale_y_log10() +
        geom_line(aes(col = name), alpha = .5)
      
      # Rest interval
      rest_x <- d |> 
        filter(`Interval Status`=="REST") |>
        summarise( m = min(time2), mx = max(time2) ) |> 
        mutate(across(1:2, ~if_else(is.infinite(.x), NA, .x)))
      
      if(!is.na(rest_x$m)){
        o <- o + geom_ribbon(aes(xmin = rest_x$m, xmax = rest_x$mx), fill = "orange", alpha = .2)
        o2 <- o2 + geom_ribbon(aes(col = NULL, xmin = rest_x$m, xmax = rest_x$mx), fill = "orange", alpha = .2)
      }
      
      # AG if present
      if(!is.null(ag_on)){
        o <- o + 
          geom_ribbon(aes(xmin  = ag_on - .25, xmax = ag_on + .25), fill="blue4", alpha=.1) +
          geom_vline(xintercept = ag_on, col="blue4") 
        o2 <- o2 + 
          geom_ribbon(aes(col = NULL, xmin  = ag_on - .25, xmax = ag_on + .25), fill="blue4", alpha=.1) +
          geom_vline(xintercept = ag_on, col="blue4") 
      } 
      
      # SR if present
      if (!is.null(sr_on)) {
        o <- o + 
          geom_ribbon(aes(xmin  = sr_on - .25, xmax = sr_on + .25), fill="red4", alpha=.075) +
          geom_vline(xintercept = sr_on, col="red4")  
        o2 <- o2 +
          geom_ribbon(aes(col = NULL, xmin  = sr_on - .25, xmax = sr_on + .25), fill="red4", alpha=.075) +
          geom_vline(xintercept = sr_on, col="red4")  
      }
      
      # Our decision, if present
      if(!is.null(v$time)){
        o <- o + 
          geom_ribbon(aes(xmin  = v$time - .25, xmax = v$time + .25), fill="green4", alpha=.125) +
          geom_vline(xintercept = v$time, col="green4") # our decision
        o2 <- o2 +
          geom_ribbon(aes(col = NULL, xmin  = v$time - .25, xmax = v$time + .25), fill="green4", alpha=.125) +
          geom_vline(xintercept = v$time, col="green4") # our decision
      } 
      
      if(!is.null(attempt_sleep)){
        o <- o + geom_ribbon(aes(xmin  = attempt_sleep, xmax = sr_on), fill="orange3", alpha=.125)
        o2 <- o2 + geom_ribbon(aes(col = NULL, xmin  = attempt_sleep, xmax = sr_on), fill="orange3", alpha=.125)
      }
    } 
    
    ### Wake plot     ======
    if( input$select_onset_or_wake == "Wake" ){ 
      if(v$just_switch){
        updateSliderInput(session, "set_hour", value = 5)
        v$just_switch <- FALSE
      }
      
      
      print("IS IT HERE WE CRASH?  PLOT")
      # Get data
      data <- p_l_wak[[  v$participant ]][ v$day, ] 
      
      # If subject is missing increment participant by + 1
      if(is.na(data$subj)){
        # if automatically update participant, set day to 0
        updateNumericInput(session, "participant", value =  v$participant+1)
        if(v$day != 1){
          updateNumericInput(session, "day", value = 1)
        }
      }
      
      # WAKE RELATED CODE
      s_subj <- data[["subj"]]
      date <- data[["date"]]
      ag_num <- data[["sleep_wake_ag_h"]]
      sr_num <- data[["last_awaking_fix"]]
      
      start_time <- floor( min(ag_num, sr_num) - .5)
      end_time <- ceiling( max(ag_num, sr_num) + .5)

      # Data
      d <- relevant_actigraphy |>
        filter(subj == s_subj, date2 == date) |>
        mutate( time2 = as.numeric(time)/3600 ) |>
        filter( time2 >= start_time & time2 <= end_time ) |>
        pivot_longer( ends_with("Light") )
      print(d)
      
      # Activity plot
      o <- d |>
        ggplot(aes(time2, Activity)) +
        labs(y = "Activity", x = "Hour (24h)") +
        theme_bw() +
        geom_line(alpha = .5)
      
      # Light plot
      o2 <- d |>
        ggplot(aes(time2, value, col=name )) +
        theme_bw() +
        labs(y = "Log(lumen)", x = "Hour (24h)") +
        scale_color_manual(values=c("blue","green","red","gray")) +
        scale_y_log10() +
        geom_line(alpha = .5) 
      
      # Rest interval
      rest_x <- d |> 
        filter(`Interval Status`=="REST") |>
        summarise( m = min(time2), mx = max(time2) ) |> 
        mutate(across(1:2, ~if_else(is.infinite(.x), NA, .x)))
      
      print(rest_x)
      
      if(!is.na(rest_x$m)){
        o <- o + geom_ribbon(mapping = aes(col = NULL, xmin = rest_x$m, xmax = rest_x$mx), fill = "orange", alpha = .2)
        o2 <- o2 + geom_ribbon(aes(col = NULL, xmin = rest_x$m, xmax = rest_x$mx), fill = "orange", alpha = .2)
      }
      
      # AG if present
      if(!is.null(ag_num)){
        o <- o +
          geom_ribbon(aes(xmin = ag_num - .25, xmax = ag_num + .25), fill="lightblue4", alpha=.1) +
          geom_vline(xintercept = ag_num, col="blue")
        o2 <- o2 +
          geom_ribbon(aes(col = NULL, xmin = ag_num - .25, xmax = ag_num + .25), fill="lightblue4", alpha=.1) +
          geom_vline(xintercept = ag_num, col="blue")
      }
      
      # SR if present
      if (!is.null(sr_num)) {
        o <- o +
          geom_ribbon(aes(xmin = sr_num - .25, xmax = sr_num + .25), fill="red4", alpha=.075) +
          geom_vline(xintercept = sr_num, col="red")
        o2 <- o2 +
          geom_ribbon(aes(col = NULL, xmin = sr_num - .25, xmax = sr_num + .25), fill="red4", alpha=.075) +
          geom_vline(xintercept = sr_num, col="red")
      }
      
      # Our decision, if present
      if(!is.null(v$time)){
        o <- o +
          geom_ribbon(aes(xmin = v$time - .25, xmax = v$time + .25), fill="green4", alpha=.125) +
          geom_vline(xintercept = v$time, col="green") # our decision
        o2 <- o2 +
          geom_ribbon(aes(col = NULL, xmin = v$time - .25, xmax = v$time + .25), fill="green4", alpha=.125) +
          geom_vline(xintercept = v$time, col="green") # our decision
      }
    }
    
    o + o2 + plot_layout(nrow=2)
  })
  
  ### Display data    =====
  observeEvent(list(v$participant, v$day, v$select_onset_or_wake), {
    
    if(v$participant > 34){
      # reset to highest known 
      v$participant <- 34 
      v$day <- 4
      updateNumericInput(session, "participant", value=34)
      updateNumericInput(session, "day", value=4)
      output$text_output <- renderText("ERROR:  CANNOT SELECT PARTICIPANT GREATER THAN 34 !")
      
    }
    
    if(v$day > 6){
      v$day <- 6
      updateNumericInput(session, "day", value=6)
      output$text_output <- renderText("ERROR:  CANNOT SELECT DAY GREATER THAN 6 !")
    }
    
    print("IS IT HERE WE CRASH? this is observe event")
    # Get raw data based on wake/onset
    if(input$select_onset_or_wake == "Wake") data <- p_l_wak[[  v$participant ]][ v$day, ]
    if(input$select_onset_or_wake == "Onset") data <- p_l_on[[  v$participant ]][ v$day, ]
    
    

    # Get saved data
    print_data <- time_adj |> 
      filter( subj == data$subj, 
              date == data$date,
              wake_or_onset == input$select_onset_or_wake ) 
    
    # Print table depending on the presence of data
    if(nrow(print_data == 1)){
      output$tbl_output <- renderTable(print_data)
    } else {
      output$tbl_output <- renderTable(tibble(empty_table=NA))
    }
  })
  
  
  ### Save      =====
  #### Store self-selected  =====
  observeEvent(input$store_self_selected, {
    
    # Save data according to state
    if( input$select_onset_or_wake == "Wake" ){ 
      d <- p_l_wak[[ v$participant ]][ v$day, ]
      time_adj <<- 
        time_adj |>
        add_row(
          subj = d$subj,
          date = d$date,
          time_adj = v$time,
          conf = input$confidence,
          wake_or_onset = input$select_onset_or_wake,
        )
    }
  
    if( input$select_onset_or_wake == "Onset" ) {
      d <- p_l_on[[ v$participant ]][ v$day, ]
      time_adj <<- 
        time_adj |>
        add_row(
          subj = d$subj,
          date = d$date,
          time_adj = v$time,
          conf = input$confidence,
          wake_or_onset = input$select_onset_or_wake,
        )
    }
    
    print( time_adj )
    output$text_output <- renderText({ "Time has been stored using the slider values!" })
    updateNumericInput(session, "day", value = v$day + 1)
  })
  
  #### Store actigraphy  =====
  observeEvent(input$use_actigraphy,{
    if( input$select_onset_or_wake == "Wake" ){ 
      d <- p_l_wak[[ v$participant ]][ v$day, ]
      time_adj <<- 
        time_adj |>
        add_row(
          subj = d$subj,
          date = d$date,
          time_adj = d$sleep_wake_ag_h,
          conf = input$confidence,
          wake_or_onset = input$select_onset_or_wake,
        )
    }
    
    if( input$select_onset_or_wake == "Onset" ) {
      d <- p_l_on[[ v$participant ]][ v$day, ]
      time_adj <<- 
        time_adj |>
        add_row(
          subj = d$subj,
          date = d$date,
          time_adj = d$sleep_onset_ag_h,
          conf = input$confidence,
          wake_or_onset = input$select_onset_or_wake,
        )
    }
    
    print( time_adj )
    output$text_output <- renderText({ "Time has been stored using participants actigraphy time!" })
    updateNumericInput(session, "day", value = v$day + 1)
  })
  
  #### Store self-reported  =====
  observeEvent(input$use_self_report,{
    if( input$select_onset_or_wake == "Wake" ){ 
      d <- p_l_wak[[ v$participant ]][ v$day, ]
      time_adj <<- 
        time_adj |>
        add_row(
          subj = d$subj,
          date = d$date,
          time_adj = d$last_awaking_fix,
          conf = input$confidence,
          wake_or_onset = input$select_onset_or_wake,
        )
      print( p_l_wak[[ input$participant ]] )
    }
    
    if( input$select_onset_or_wake == "Onset" ) {
      d <- p_l_on[[ v$participant ]][ v$day, ]
      time_adj <<- 
        time_adj |>
        add_row(
          subj = d$subj,
          date = d$date,
          time_adj = d$sleep_time_cum,
          conf = input$confidence,
          wake_or_onset = input$select_onset_or_wake,
        )
    }
    
    print(time_adj)
    output$text_output <- renderText({ "Time has been stored using participants self-reported time!" })
    updateNumericInput(session, "day", value = v$day + 1)
  })
  
  
  ### export data (i.e., save to file)  =====
  observeEvent(input$export_data, {
    save(time_adj, file = "sleep_adjustment.rdata")
    
    output$text_output <- renderText({ "DATA HAS BEEN EXPORTED!\n\nYou may now exit." })
  })
}

# Run Application  =====
shinyApp(ui = ui, server = server)
