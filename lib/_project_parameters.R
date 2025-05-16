message("Setting project options.")
options(
  #' Date & time (of run)
  project_date_time = format( Sys.time(), "_%Y-%m-%d_%H-%M-%S_")
  
  #' **Data related settings **
  , project_save_data_to_file        = TRUE 
  , project_save_data_location       = "data/"
  , project_save_data_formats        = c(".RData")
  , project_save_data_with_date_time = FALSE 

  #' **Figure related settings**
  , project_save_figs_to_file        = TRUE
  , project_save_figs_location       = "figures/"
  , project_save_figs_formats        = c(".svg", ".jpeg")
  , project_save_figs_with_date_time = FALSE

  #' **Table related settings**
  , project_save_tbls_to_file        = TRUE
  , project_save_tbls_location       = "tables/"
  , project_save_tbls_formats        = c(".html", ".docx")
  , project_save_tbls_with_date_time = FALSE
  
  #' **Bayesian related settings**
  , project_bayes_run_models          = FALSE
  , project_bayes_save_to_file        = FALSE
  , project_bayes_save_with_date_time = TRUE
  , project_bayes_diagnostics         = TRUE
  , mc.cores                          = 6 
  , brms.backend                      = "cmdstanr"

  #' **GGplot**
  , project_custom_ggplot = TRUE
  
  #' Generally, leave the below alone.
  , project_custom_colours = list(
    B = "#000000", W = "#ffffff"
    , `/G` = "#F5F5F5", `-k` = "#D3D3D3", k = "#808080", `+K` = "#404040", `*K` = "#1A1A1A"
    , `/r` = "#ffd6d2", `-r` = "#F8A290", r = "#F8766D", `+r` = "#b20b00", `*r` = "#8c0000"
    , `/b` = "#d6e9ff", `-b` = "#8EBEFF", b = "#78ADFF", `+b` = "#00348C", `*b` = "#00246D"
    , `/g` = "#c9ffd6", `-g` = "#7AD686", g = "#00BA38", `+g` = "#006400", `*g` = "#004B00"
    , `/o` = "#ffe0c2", `-o` = "#FFC490", o = "#FF9E4A", `+o` = "#B25000", `*o` = "#8C3D00"
    , `/y` = "#fffbd6", `-y` = "#FFF090", y = "#FFE359", `+y` = "#D1B000", `*y` = "#AA8F00"
    , `/p` = "#e9d6ff", `-p` = "#D0B6FF", p = "#B476FF", `+p` = "#7436B3", `*p` = "#5A1A99"
  )
  # project_custom_colours_rrR      = c("#ffd6d2", "#F8766D", ),
  # project_custom_colours_rgy      = c("#F8766D", "#619CFF", "#f8ea6d"),
  # project_custom_colours_pale_bBrR = c("#4477AA","#114477","#EE6677","#991122"),

  #' Because partial matching exists, warn whenever such an instance occur, 
  #' as this can be especially problematic for statistical analysis.
  , warnPartialMatchArgs = TRUE
  , warnPartialMatchAttr = TRUE
  , warnPartialMatchDollar = TRUE
)

#' If conditions can take vectors but only evaluate the first on, 
#' this will add a warning. 
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = 1)
