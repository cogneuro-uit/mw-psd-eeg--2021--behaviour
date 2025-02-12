load_project <- function(relative_path){
  #' Function to load libraries, data, and munging for .qmds.
  
  # Get higher-order libraries
  load_libraries <- lapply(
    read.dcf(paste0(relative_path, "config/global.dcf"))[,"libraries"], 
    \(x){ strsplit(x, ", ") } )[[1]][[1]]
  
  # Load higher-order libraries
  lapply(load_libraries, \(library){
    cat("Loading library:", library, "\n")
    library(library, character.only = T)
  })
  
  # Get local libraries, data, and munge paths...
  get_path <- function(path, pattern){
    list.files(paste0(relative_path, path), pattern = pattern, 
               ignore.case = T, full.names = T)
  }
  # Paths:
  load_path <- c(  get_path("lib",   "\\.r"), 
                   get_path("cache", "\\.rdata"), 
                   get_path("data",  "\\.rdata"),
                   get_path("munge", "\\.r") )
  
  # Load rccp
  lapply(list.files(
    paste0(relative_path,"lib"), pattern = "*\\.cpp", full.names = TRUE),
    \(x) Rcpp::sourceCpp(x) )
  
  # Load local data/functions/pre-processing
  lapply(load_path, \(path){
    message("Loading ", path)
    
    if( str_ends(path, regex("\\.rdata", ignore_case=T)) ){
      load(path, envir = .GlobalEnv)
    } else {
      source(path)
    }
  })
}
