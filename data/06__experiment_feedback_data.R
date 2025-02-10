# fix norwegian letters
# Sys.setlocale("LC_ALL", "no_NO.UTF-8")     # ALL

fnames <- 
  list.files("data/baseline+diary", pattern = ".xlsx", full.names = T)
#' filter out open xlsx files:
fnames <- fnames[!str_detect(fnames, "~")]

# Read and get participants feedback        =======
participant_feedback <- map(fnames, \(fname){
  print(fname)
  subj <- str_split(fname, "[/_]")[[1]][3]
  
  d <- read_xlsx(fname, sheet = "Feedback-comments", col_names = F)
  
  if(as.numeric(subj) < 5){
    d |> 
      mutate(
        session = case_when( ...1 == "T1" ~ "S1", ...1 == "T2" ~ "S2" )
      ) |> 
      fill(session) |>
      filter(
        !(`...1` %in% c("T1","Feedback", "Open Feedback", "T2", "Quiz feil", "Hvilket spørsmål?", "Hvilke spørsmål?"))
      ) |> 
      mutate(
        q_cat = case_when(
          str_starts(`...1`, "Q")        ~ "Question",
          lag( str_starts(`...1`, "Q") ) ~ "Answer",
        ),
        q_que = case_when( 
          q_cat == "Question" ~ str_remove(...1, "Q\\d - "),
          lag( q_cat == "Question" ) ~ lag( str_remove(...1, "Q\\d - ") )
        ), 
        q_que = case_when(
          str_starts(q_que, "vennligst evaluer ") ~ "gloves",
          str_starts(q_que, "Vennligst vurder hvor") ~ "AE_motivation",
          str_starts(q_que, "Når du svarte på spørsmålet") ~ "MW_confidence",
          str_starts(q_que, "Bruke du noen spesiell strategi") ~ "task_strategy",
          str_starts(q_que, "Har du noen bemerkninger") ~ "comment_task",
          str_starts(q_que, "Når du rapporterte å ikke være") ~ "off_task_thought",
          str_starts(q_que, "Har du noen andre kommentarer?") ~ "comment_other",
        ),
        q_ans = if_else(q_cat=="Answer", ...1, NA)
      ) |> 
      filter(!is.na(q_que) & q_cat=="Answer") |>
      select(-...1, -...2, -q_cat)  |> 
      mutate(subj = subj) |>
      pivot_wider(names_from = c(session, q_que), values_from = q_ans) 
} else {
    d |>
      mutate(
        session = case_when( ...1 == "T1" ~ "S1", ...1 == "T2" ~ "S2" )
      ) |> 
      fill(session) |>
      filter(
        (`...1` %in% c("Q1","Q2","Q3","Q4"))
      ) |> 
      mutate(q_que = rep(
        c("gloves", "AE_motivation", "MW_confidence", "task_strategy",
          "comment_task", "off_task_thought", "comment_other"), 2),
        subj = subj
      ) |> 
      rename("Answer"=...2) |>
      select("Answer", "session", "q_que", "subj") |> 
      pivot_wider(names_from = c(session, q_que), values_from = "Answer")
  }
}) |> list_rbind()


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
