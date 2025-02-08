#'                         **PARAMETERS**                                    ======

#' Project related settings: 
project <- list(
  
  custom_ggplot = TRUE,
  custom_gt = TRUE,
  
  #'     **BAYES RELATED**
  bayes = list(
    run_models = FALSE,
    
    # Set various settings
    set = list(
      backend = "cmdstanr",
      
      # Parallel settings: 
      parallel = TRUE,
      cores = .8
      #' Number of cores.
      #' Can be a whole number (*10*), a percent (*50%* or *.5*)
    ),
    
    # Save settings
    save = list(
      to_file = FALSE,
      with_date_time = FALSE
    )
  ),
  
  #'    **TABLES**
  tbls = list(
    save = list(
      to_file = FALSE,
      formats = c("pdf", "html"),
      with_date_time = FALSE
    )
  ),
  #'    **FIGURES**
  figs = list(
    save = list(
      to_file = FALSE,
      formats = c("svg", "jpeg"),
      with_date_time = FALSE
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

#' Probe question information
probe_information <- list(
  no = list(
    questions = list(
      probe1 = "I hvilken grad fokuserte du på oppgaven rett før dette spørsmålet?",
      probe2 = "I den grad du ikke fokuserte på oppgaven, tenkte du på ingenting eller tenkte du på noe spesifikt?",
      probe3 = "Var du bevisst på hvor du fokusserte oppmerksomheten din (enten oppgaverelatert eller annet) eller var den spontan?"
    ),
    descriptions = list(
      probe1 = "1 = helt klart IKKE fokussert; 4 = helt klart fokussert",
      probe2 = "1 = helt klart ingenting; 4 = helt klart noe spesifikt",
      probe3 = "1 = helt klart spontan; 4 = helt klart bevisst"
    )
  ),
  en = list(
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
  )
)

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

# Set custom colours for ggplot (if enabled)
if( project[["custom_ggplot"]] ){
  
  rg_colours <- c("#F8766D", "#619CFF")
  options(ggplot2.continous.colour = rg_colours)
  options(ggplot2.continous.fill = rg_colours)
  options(ggplot2.discrete.colour = rg_colours)
  options(ggplot2.discrete.fill = rg_colours)
  
  theme_set( theme_bw() )
}



# Set bayesian cores: 
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

warning(
  "\n============================================",
  "\n           PROJECT PARAMETERS     ",
  "\n", "SAVE outputs (figs and tbls) locally: ", project_save_outputs_to_file,
  "\n", "   Table formats: ", paste(project_save_tbl_formats, collapse = " & "),
  "\n", "   Figure formats: ", paste(project_save_fig_formats, collapse = " & "), 
  "\n", "SAVE outputs with relative time/date: ", project_save_outputs_with_date_time,
  "\n", "SET custom colours: ", project_set_custom_colours,
  "\n", "RUN Bayesian models:  ", project_run_bayesian_models,
  if(project_run_bayesian_models) "\n  \U0002757  WILL TAKE SOME TIME  \U0002757",
  "\n", "SAVE GENERATED Bayesian models?  ", project_save_bayesian_model,
  "\n",
  "\n", "Waiting 5 seconds..."
)

Sys.sleep(5)
message("Starting...")