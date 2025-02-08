# fix norwegian letters
Sys.setlocale("LC_ALL", "no_NO.UTF-8")     # ALL

list.files("data/baseline+diary", full.names = T) -> fnames

fnames[!str_detect(fnames, "~")] -> fnames

# Read and get participants feedback        =======

question_names <- c("gloves", "AE_motivation", "MW_confidence", "task_strategy", 
                    "comment_task", "off_task_thought", "comment_other",
                    "quiz_fail","which_quet")

participant_feedback <- data.frame()
for(x in 1:length(fnames)){ 

  read_xlsx(fnames[x],sheet = "Feedback-comments", col_names = F) -> d
  
  par_df <- data.frame() # new for each par
  
  # 4 first are different 
  if(x %in% 1:4){
    for(y in 1:50){ # long
      d$...1[y] -> y1
      if(!is.na(y1) & str_detect(y1,"Q")){
        if(str_detect(y1,"Quiz")){
          d$...1[y] -> fb_quiz # quiz mistake
          d$...2[y] -> fb_res  # *res*
          d$...1[y+1] -> fb_quiz_fail # which question?
          d$...2[y+1] -> fb_quiz_fail_r # *res*
          c(fb_quiz,fb_quiz_fail) -> fb_quiz # collect
          c(fb_res,fb_quiz_fail_r) -> fb_res # collect
        } else {
        d$...1[y] -> fb_quiz
        d$...1[y+1] -> fb_res
        }
        rbind(par_df,tibble(fb_quiz,fb_res)) -> par_df
      }
    }
    # add proper names
    which(str_detect(par_df$fb_quiz, "ilket spørsmål")) -> end_1 # to quiz
    end_1[2] -> end_2
    end_1[1] -> end_1
    par_df$fb_quiz[(end_1-(end_1-1)):end_1] <- question_names
    par_df$fb_quiz[(end_2-(end_1-1)):end_2] <- question_names
    
    # Add participant code
    strsplit(fnames[x], "\\/|\\_")[[1]][3] -> par_df$par_code 
    # Add the different sessions
    par_df$session <- rep(1:2,each=end_1) 
    # Add group
    read_xlsx(fnames[x], sheet = "DataSheet", 
              range = "D6", col_names = F) |> 
      mutate(...1 = ifelse(...1==0,"ESD","LSD")) |>
      pull(...1) -> code_
    
    # Sleepdep first?
    ifelse(code_=="ESD", 1,0) -> fc1
    ifelse(fc1==1, 0,1) -> fc2
    c(rep(fc1,end_1), rep(fc2,(end_2-end_1))) -> par_df$sleepdep
    # Session
    par_df$session <- rep(1:2,each=(end_1)) 
  }
  else {
  # For every other case:  # I hope
    for(y in 1:33){ # shorter
      d$...1[y] -> y1
      if(!is.na(y1) & str_detect(y1,"Q")){
        if(str_detect(y1,"Quiz")){
          d$...1[y] -> fb_quiz
          d$...2[y] -> fb_res
          d$...1[y+1] -> fb_quiz_fail
          d$...2[y+1] -> fb_quiz_fail_r
          c(fb_quiz,fb_quiz_fail) -> fb_quiz
          c(fb_res,fb_quiz_fail_r) -> fb_res
        } else {
          d$...1[y] -> fb_quiz
          d$...2[y] -> fb_res
        }
        rbind(par_df,tibble(fb_quiz,fb_res)) -> par_df
      }
    }
    # Fix names (to column names)
    which(str_detect(par_df$fb_quiz, "ilket spørsmål")) -> end_1 # we get 2
    end_1[2] -> end_2 # last
    end_1[1] -> end_1 # first
    par_df$fb_quiz[(end_1-(end_1-1)):end_1] <- question_names
    par_df$fb_quiz[(end_2-(end_1-1)):end_2] <- question_names
    
    # Add participant code
    strsplit(fnames[x], "\\/|\\_")[[1]][3] -> par_df$par_code 
    # add group
    read_xlsx(fnames[1], sheet = "DataSheet", 
              range = "D6", col_names = F) |> 
      mutate(...1 = ifelse(...1==0,"ESD","LSD")) |>
      pull(...1) -> code_
    
    # Sleepdep first?
    ifelse(code_=="ESD", 1,0) -> fc1
    ifelse(fc1==1, 0,1) -> fc2
    c(rep(fc1,end_1), rep(fc2,(end_2-end_1))) -> par_df$sleepdep
    # Session
    par_df$session <- rep(1:2,each=(end_1))
  }
  # Bind
  rbind(participant_feedback, par_df) -> participant_feedback
}

# save(participant_feedback, file="data/participant_feedback.rdata")


# Researchers comment to the experiment       ======
research_comments <- 
  map_df(fnames, \(x){
    d <- 
      tibble(
        name = c("Protocol compliance/probs", "EEG prob", "task-related", "else", NA,
                 "Protocol compliance/probs", "EEG prob", "task-related", "else")
      ) |>
      add_column(read_xlsx(x, sheet = "DataSheet", range="D29:D37", col_names="resp")) 
    
    if(is.null(d[["resp"]])){
      d[["resp"]] <- ""
    }
    
    d |>
      filter(!is.na(name)) |>
      mutate(seq = rep(c("T1","T2"), each=4)) |>
      pivot_wider(names_from=seq, values_from=resp)
})

# save(research_comments, file = "data/research_comments.Rdata") 
