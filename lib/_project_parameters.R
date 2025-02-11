#'                    **PROJECT PARAMETERS**                              ======

#' Project related settings: 
project <- list(
  custom = list(
    ggplot = TRUE,
    ggplot_col = c("#F8766D", "#619CFF"),
    gt = TRUE
  ),
  
  #'     **BAYES RELATED**
  bayes = list(
    run_models = FALSE,
    
    # Set various settings
    set = list(
      diagnostic_feedback = FALSE,
      backend = "cmdstanr",
      
      # Parallel settings: 
      parallel = TRUE,
      cores = .5
      #' Number of cores.
      #' Can be a whole number (*10*), a percent (*50%* or *.5*)
    ),
    
    # Save settings
    save = list(
      to_file = FALSE,
      with_date_time = TRUE
    )
  ),
  
  #'    **TABLES**
  tbls = list(
    save = list(
      to_file = FALSE,
      formats = c("pdf", "html"),
      with_date_time = TRUE
    )
  ),
  #'    **FIGURES**
  figs = list(
    save = list(
      to_file = FALSE,
      formats = c("svg", "jpeg"),
      with_date_time = TRUE
    )
  )
)


#'                       **SET VARIABLES**                                   =======

#' All outputs (figures/tables) should be stored in the same place.
outputs <- list(
  figs = list(),
  tbls = list()
) 

#' Set parameters for AE/BV calculation
nback=25
which.apen=2

#' experiment information
experiment_information <- list(
  no = list(
    # probe info
    probe_information = list(
      questions = list(
        probe1 = "I hvilken grad fokuserte du på oppgaven rett før dette spørsmålet?",
        probe2 = "I den grad du ikke fokuserte på oppgaven, tenkte du på ingenting eller tenkte du på noe spesifikt?",
        probe3 = "Var du bevisst på hvor du fokusserte oppmerksomheten din (enten oppgaverelatert eller annet) eller var den spontan?"
      ),
      descriptions = list(
        probe1 = "1 = helt klart IKKE fokussert; 4 = helt klart fokussert",
        probe2 = "1 = helt klart ingenting; 4 = helt klart noe spesifikt",
        probe3 = "1 = helt klart spontan; 4 = helt klart bevisst")
    ),
    
    # Participant feedback
    participant_feedback = list(
      questions = list(
        gloves = "vennligst evaluer i hvilken grad du ble distrahert av å ha på deg engangshansker i løpet av oppgaven:",
        ae_motivation = "Vennligst vurder hvor motivert du var til å produsere tilfeldige sekvenser da eksperimentet startet:",
        mw_confidence = "Når du svarte på spørsmålet om hvor fokusert du var på oppgaven, hvor sikker var du på svaret ditt?",
        task_strategy = "Bruke du noen spesiell strategi for å lage en tilfeldig sekvens?",
        comment_task = "Har du noen bemerkninger angående oppgaven i seg selv",
        off_task_thought = "Når du rapporterte å ikke være fokusert på oppgaven (OFF-TASK), hva tenkte du på da?",
        comment_other = "Har du noen andre kommentarer?"),
      description = list(
        glove = "1 - ikke i det hele tatt distrahert, 7 - ekstremt distrahert",
        ae_motivation = "1 - ikke motivert, 7 - veldig motivert",
        mw_confidence = "1 - ikke sikker i det hele tatt, 7 - ekstremt sikker",
        task_strategy = "Text answer",
        comment_task = "Text answer",
        off_task_thought = "Text answer",
        comment_other = "Text answer")
    )
  ),
  
  en = list(
    probe_information = list(
      questions = list(
        probe1 = "To what extent were you focused on the task right before this question?",
        probe2 = "To the extent that you were not focused on the task, were you thinking about nothing or were you thinking about something specific?",
        probe3 = "Were you aware of where you were directing your attention (either task-related or otherwise) or was it spontaneous?"
      ),
      descriptions = list(
        probe1 = "1 = definitely NOT focused; 4 = definitely focused",
        probe2 = "1 = definitely nothing; 4 = definitely something specific",
        probe3 = "1 = definitely spontaneous; 4 = definitely aware"
      )
    ),
    
    participant_feedback = list(
      questions = list(
        gloves = "Please evaluate to what extent you were distracted by wearing disposable gloves during the task:",
        ae_motivation = "Please assess how motivated you were to produce random sequences when the experiment started:",
        mw_confidence = "When you answered the question about how focused you were on the task, how confident were you in your answer?",
        task_strategy = "Did you use any particular strategy to create a random sequence?",
        comment_task = "Do you have any remarks regarding the task itself?",
        off_task_thought = "When you reported not being focused on the task (OFF-TASK), what were you thinking about?",
        comment_other = "Do you have any other comments?"
      ),
      description = list(
        glove = "1 - not at all distracted, 7 - extremely distracted",
        ae_motivation = "1 - not motivated, 7 - very motivated",
        mw_confidence = "1 - not at all confident, 7 - extremely confident",
        task_strategy = "Text answer",
        comment_task = "Text answer",
        off_task_thought = "Text answer",
        comment_other = "Text answer"
      )
    )
  )
)

# Set date_time         ======
#' Set the relevant date-time for storing outputs.
if( project[["tbls"]][["save"]][["with_date_time"]] ){
  project[["tbls"]][["save"]][["date_time"]] <- format( Sys.time(), "_%Y-%m-%d_%H-%M-%S_")
} else {
  project[["tbls"]][["save"]][["date_time"]] <- NULL
} 

if( project[["figs"]][["save"]][["with_date_time"]] ){
  project[["figs"]][["save"]][["date_time"]] <- format( Sys.time(), "_%Y-%m-%d_%H-%M-%S_")
} else {
  project[["figs"]][["save"]][["date_time"]] <- NULL
}

if( project[["bayes"]][["save"]][["with_date_time"]] ){
  project[["bayes"]][["save"]][["date_time"]] <- format( Sys.time(), "_%Y-%m-%d_%H-%M-%S_")
} else {
  project[["bayes"]][["save"]][["date_time"]] <- NULL
}

# Custom colours      =====
#'  custom colours for ggplot (if enabled)
if( project[["custom"]][["ggplot"]] ){
  options(ggplot2.continous.colour = project[["custom"]] )
  options(ggplot2.continous.fill = rg_colours)
  options(ggplot2.discrete.colour = rg_colours)
  options(ggplot2.discrete.fill = rg_colours)
  theme_set( theme_bw() )
}

# Set bayesian cores     ======
if( project[["bayes"]][["set"]][["parallel"]] ){
  ..cores = parallel::detectCores()
  
  if( project[["bayes"]][["set"]][["cores"]] |> str_detect("%") ){
    project[["bayes"]][["set"]][["cores"]] <-
      project[["bayes"]][["set"]][["cores"]] |>
      str_remove("%") |> 
      as.numeric() / 100 * ..cores |> 
      floor()
  }
  
  if( project[["bayes"]][["set"]][["cores"]] < 1 ){
    project[["bayes"]][["set"]][["cores"]] <-
      floor( project[["bayes"]][["set"]][["cores"]] * ..cores )
  }
  
  if( project[["bayes"]][["set"]][["cores"]] > ..cores & 
      ..cores >= 100){
    warning("Core value set higher than detected available cores... Setting as percentage.")
    project[["bayes"]][["set"]][["cores"]] <-
      floor( project[["bayes"]][["set"]][["cores"]]/100 * ..cores )
  }
  
  if( project[["bayes"]][["set"]][["cores"]] < ..cores ){
    project[["bayes"]][["set"]][["cores"]] <-
      floor( project[["bayes"]][["set"]][["cores"]]/100 * ..cores )
    
  } else {
    warning("Could not interpret bayes cores, setting to half...")
    project[["bayes"]][["set"]][["cores"]] <- floor(..cores * .5)
  }
  
    
  options( mc.cores = project[["bayes"]][["set"]][["cores"]] )
}




#'                  **Warning**                  =======
project_warning <- function(){
  warning(
    "\n============================================",
    "\n           PROJECT PARAMETERS     ",
    "\n--------------------------------------------",
    "\n", "Custom ggplot: ", project[["custom"]][["ggplot"]],
    "\n", "   colours: ", paste(project[["custom"]][["ggplot_col"]], collapse = " & "),
    "\n", "Custom gt: ",  project[["custom"]][["gt"]],
    "\n", "Save tables:",
    "\n", "   Locally: ", project[["tbls"]][["save"]][["to_file"]],
    "\n", "   With time/date: ", project[["tbls"]][["save"]][["with_date_time"]],
    "\n", "   Formats formats: ", paste(project[["tbls"]][["save"]][["formats"]], collapse = " & "),
    "\n", "Save figures:",
    "\n", "   Locally: ", project[["figs"]][["save"]][["to_file"]],
    "\n", "   With date/time:  ", project[["figs"]][["save"]][["with_date_time"]],
    "\n", "   Formats: ", paste(project[["figs"]][["save"]][["with_date_time"]], collapse = " & "), 
    "\n", "Bayesian:",
    "\n", "   Run models:  ", project[["bayes"]][["run_models"]],
    if (project[["bayes"]][["run_models"]] ){
      paste0("\n", "\n \U0002757  WILL TAKE SOME TIME  \U0002757",
             "\n", "   Save models: ", project[["bayes"]][["save"]][["to_file"]],
             "\n", "   Save models with date/time: ", project[["bayes"]][["save"]][["with_date_time"]])
    },
    "\n============================================",
    "\n", "Waiting 5 seconds..."
  )
}

project_warning()

Sys.sleep(5)
message("Starting...")