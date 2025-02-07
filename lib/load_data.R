
load_data <- function(path, pattern = NULL, envir = .GlobalEnv){
  #' Load data. 
  #' 
  #' Function to load data from the specified (relative) path. 
  #' The function automatically searches for **.rdata** (in any form).
  #' Additionally search parameters can be added to @pattern. 
  #' 
  #' @param path The path to search for files
  #' @param pattern Add a pattern to search for files in the specified path. 
  
  # Pattern handling for loading data 
  if( !is.null(pattern) ){
    pattern <- c( "[.]rdata", pattern )
  } else { pattern <- "[.]rdata" }
  
  # File names:
  path_fnames <- list.files(path, pattern = pattern, full.names = T, ignore.case = T) 
  fnames <- list.files(path, pattern = pattern, ignore.case = T )

  # Load the data...
  purrr::walk( 1:length(fnames) , \(fnum){ 
    message(paste(
      "Loading", fnames[fnum],  "...")
    )
    load( path_fnames[fnum], envir = envir )
  })
  message("Done!\n")
}

