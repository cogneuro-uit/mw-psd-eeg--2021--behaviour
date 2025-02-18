#'                    **PROJECT PARAMETERS**                              ======

message("Setting project settings...")

#' Set general project parameters
options(
  # Get current date/time
  project_date_time = format( Sys.time(), "_%Y-%m-%d_%H-%M-%S_"),

  # Figure related settings
  project_save_figs_to_file = TRUE,
  project_save_figs_location = "/outputs/figs/",
  project_save_figs_formats = c(".svg", ".jpeg"),
  project_save_figs_with_date_time = TRUE,

  # Table related settings
  project_save_tbls_location = "/outputs/tbls/",
  project_save_tbls_to_file = TRUE,
  project_save_tbls_formats = c(".html", ".docx"),
  project_save_tbls_with_date_time = TRUE,
  
  # Aesthetics: 
  project_custom_ggplot = TRUE,
  project_custom_ggplot_colour = c("#F8766D", "#619CFF"),

  # Bayesian options:
  project_bayes_run_models = FALSE,
  project_bayes_save_to_file = FALSE,
  project_bayes_save_with_date_time = TRUE,
  project_bayes_diagnostics = TRUE,
  
  mc.cores = 6,   #' or as many as you have
  brms.backend = "cmdstanr",

    
  #' Because partial matching exists, warn whenever such an instance occur, 
  #' as this can be especially problematic for statistical analysis.
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)

#' If conditions can take vectors but only evaluate the first on, 
#' this will add a warning. 
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_"=1)



# Custom colours      =====
#'  custom colours for ggplot (if enabled)
if( getOption("project_custom_ggplot") ){
  options(
    ggplot2.continous.colour = getOption("project_custom_ggplot_colour"),
    ggplot2.continous.fill   = getOption("project_custom_ggplot_colour"), 
    ggplot2.discrete.colour  = getOption("project_custom_ggplot_colour"), 
    ggplot2.discrete.fill    = getOption("project_custom_ggplot_colour")
  )
  theme_set( theme_bw() )
}




#'                       **SET VARIABLES**                                   =======

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


#'                  **Warning**                  =======
# project_warning <- function(){
#   warning(
#     "\n============================================",
#     "\n           PROJECT PARAMETERS     ",
#     "\n--------------------------------------------",
#     "\n", "Custom ggplot: ", project[["custom"]][["ggplot"]],
#     "\n", "   colours: ", paste(project[["custom"]][["ggplot_col"]], collapse = " & "),
#     "\n", "Custom gt: ",  project[["custom"]][["gt"]],
#     "\n", "Save tables:",
#     "\n", "   Locally: ", project[["tbls"]][["save"]][["to_file"]],
#     "\n", "   With time/date: ", project[["tbls"]][["save"]][["with_date_time"]],
#     "\n", "   Formats formats: ", paste(project[["tbls"]][["save"]][["formats"]], collapse = " & "),
#     "\n", "Save figures:",
#     "\n", "   Locally: ", project[["figs"]][["save"]][["to_file"]],
#     "\n", "   With date/time:  ", project[["figs"]][["save"]][["with_date_time"]],
#     "\n", "   Formats: ", paste(project[["figs"]][["save"]][["formats"]], collapse = " & "), 
#     "\n", "Bayesian:",
#     "\n", "   Run models:  ", project[["bayes"]][["run_models"]],
#     if (project[["bayes"]][["run_models"]] ){
#       paste0("\n", "\n \U0002757  WILL TAKE SOME TIME  \U0002757",
#              "\n", "   Save models: ", project[["bayes"]][["save"]][["to_file"]],
#              "\n", "   Save models with date/time: ", project[["bayes"]][["save"]][["with_date_time"]])
#     },
#     "\n============================================",
#     "\n", "Waiting 5 seconds..."
#   )
# }
# project_warning()

# Sys.sleep(5)
message("Done...")