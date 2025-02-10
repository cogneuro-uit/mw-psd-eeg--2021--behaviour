# fix norwegian letters
# Sys.setlocale("LC_ALL", "no_NO.UTF-8")     # ALL

fnames <- list.files("data/baseline+diary", pattern = ".xlsx", full.names = T)
#' filter out open xlsx files:
fnames <- fnames[!str_detect(fnames, "~")]

feedback <- map(fnames, \(fname){
  print(fname)
  subj <- str_split(fname, "[/_]")[[1]][3]
  
  par <- read_xlsx(fname, sheet = "Feedback-comments", col_names = F)
  
  if(as.numeric(subj) < 5){
    par <- par |> 
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
        q_ans = if_else(q_cat=="Answer", ...1, NA),
        par = "Par",
      ) |> 
      filter(!is.na(q_que) & q_cat=="Answer") |>
      select(-...1, -...2, -q_cat)  |> 
      mutate(subj = subj) |>
      pivot_wider(names_from = c(session, par, q_que), values_from = q_ans) 
} else {
    par <- par |>
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
        subj = subj,
        par = "Par",
      ) |> 
      rename("Answer"=...2) |>
      select("Answer", "session", "q_que", "par", "subj") |> 
      pivot_wider(names_from = c(session, par, q_que), values_from = "Answer")
  }
  
  res <- read_xlsx(fname, sheet = "DataSheet", range="B29:D37", col_names = FALSE) 
  
  res <- res |>
    select(...1, ...3) |>
    rename(que = ...1, ans = ...3) |>
    filter(!is.na(que)) |>
    mutate(
      subj = subj, 
      res = "exp",
      seq = rep(c("S1","S2"), each=4),
      que = case_when(
        str_starts(que, "Protocol")  ~ "Protocol_compliance__side_effects", 
        str_starts(que, "EEG") ~ "EEG_problems", 
        str_starts(que, "Task-") ~ "Task_related", 
        str_starts(que, "Else") ~ "Else", 
      ),
    ) |>
    pivot_wider(names_from = c(seq, res, que), values_from = ans)
  
  par |> left_join(res, "subj")
}) |> list_rbind()

