#' Establish Future Execution Plan for Parallel Processing
#'
#' Configures the future package's execution plan for parallel processing across
#' local or remote workers. Supports YAML configuration files, named lists, or
#' simple connection strings. Designed for brms models requiring multiple cores
#' per worker (default: 6 cores for parallel chains).
#'
#' @param remotes Character string path to YAML config file, named list of server
#'   configurations, or character vector of connection strings. Default: "config/remotes.yaml"
#' @param cores Integer. Number of cores required per worker (e.g., for brms parallel chains).
#'   Default: 6
#' @param max_load Numeric between 0 and 1. Default maximum core load fraction for
#'   auto-detection. Can be overridden per-server. Default: 1/2
#' @param remote_worker_num Named numeric vector. Explicitly set number of workers
#'   for specific servers. Overrides YAML and auto-detection. 
#'   Example: `c("hpc1" = 10, "hpc2" = 8)`. Default: NULL
#' @param remote_max_load Named numeric vector. Explicitly set max_load for specific
#'   servers. Overrides YAML max_load but not remote_worker_num.
#'   Example: `c("hpc1" = 0.8, "lab_server" = 0.5)`. Default: NULL
#'
#' @return Invisibly returns TRUE after configuring the future plan
#'
#' @details
#' **Priority Hierarchy for Worker Count:**
#' 1. `remote_worker_num` argument (highest)
#' 2. YAML `workers` field
#' 3. Auto-detection: floor((remote_cores * max_load) / cores)
#'
#' **Priority Hierarchy for max_load:**
#' 1. `remote_max_load` argument (highest)
#' 2. YAML `max_load` field
#' 3. Function `max_load` parameter
#'
#' **YAML Structure:**
#' ```yaml
#' servers:
#'   server_name:
#'     connection: "user@host.com"
#'     workers: 8                    # optional
#'     max_load: 0.7                 # optional
#'     cores_per_worker: 6           # optional
#' ```
#'
#' If no remote workers can be established, falls back to local multisession.
#'
#' @examples
#' \dontrun{
#' # Use YAML configuration with defaults
#' futr_establish("config/remotes.yaml")
#'
#' # Override specific server worker counts
#' futr_establish(
#'   "config/remotes.yaml",
#'   remote_worker_num = c("hpc1" = 15, "hpc2" = 6)
#' )
#'
#' # Use simple connection strings
#' futr_establish(c("user@server1.edu", "user@server2.edu"))
#'
#' # Local execution only
#' 
#' futr_establish(remotes = NULL)
#' }
#'
#' @export
futr_establish <- function(
    remotes              = "config/remotes.yaml"
    , cores              = getOption("mc.cores", 5) # cores per worker (for brms)
    , max_load           = getOption("project_future_max_core_load", 1/2)
    , remote_worker_num  = NULL  # named vector: c("server1" = 4, "server2" = 8)
    , remote_max_load    = NULL  # named vector: c("server1" = 0.5, "server2" = 0.7)
) {
  # Store call args for futr_refresh()
  .future_last_args <<- list(
    remotes           = remotes,
    cores             = cores,
    max_load          = max_load,
    remote_worker_num = remote_worker_num,
    remote_max_load   = remote_max_load
  )
  
  # if relative path
  test_remote = paste0(relative_path, remotes)
  
  # Parse input: YAML file, named list, or vector of connection strings
  if (is.character(test_remote) && length(test_remote) == 1 && stringr::str_detect(test_remote, "\\.yaml$")) {
    if (file.exists(test_remote)) {
      config <- yaml::read_yaml(test_remote)
      available_remotes <- config$servers
    } else {
      stop("Remote server configuration file '", remotes, "' not found.")
    }
  } else if (is.list(remotes) && !is.null(names(remotes))) {
    # Already a named list
    available_remotes <- remotes
  } else if (is.vector(remotes) || is.character(remotes)) {
    # Convert vector of connection strings to named list
    message("Converting connection strings to server configs...")
    available_remotes <- setNames(
      lapply(remotes, function(r) list(connection = r)),
      paste0("server", seq_along(remotes))
    )
  } else {
    stop("Invalid 'remotes' input format.")
  }
  
  remote_workers <- purrr::map(names(available_remotes), function(server_name) {
    server_config <- available_remotes[[server_name]]
    connection <- server_config$connection
    
    message("Configuring remote: ", server_name, " (", connection, ")")
    
    if (!is.null(remote_worker_num) && server_name %in% names(remote_worker_num)) {
      # PRIORITY 1: Check for function argument override
      n_server_workers <- remote_worker_num[[server_name]]
      message("  Using argument-specified workers: ", n_server_workers)
      
    } else if (!is.null(server_config$workers)) {
      # PRIORITY 2: Use YAML-specified workers
      n_server_workers <- server_config$workers
      message("  Using YAML-configured workers: ", n_server_workers)
      
    } else {
      # PRIORITY 3: Auto-detect based on cores and load
      tryCatch({
        future::plan(future::cluster, workers = connection)
        remote_cores <- future::availableCores(methods = "system") |> purrr::pluck(1)
        
        # Determine max_load: argument > YAML > default
        if (!is.null(remote_max_load) && server_name %in% names(remote_max_load)) {
          server_max_load <- remote_max_load[[server_name]]
          message("  Using argument-specified max_load: ", server_max_load)
        } else if (!is.null(server_config$max_load)) {
          server_max_load <- server_config$max_load
          message("  Using YAML-configured max_load: ", server_max_load)
        } else {
          server_max_load <- max_load
          message("  Using default max_load: ", server_max_load)
        }
        
        cores_per_worker <- server_config$cores_per_worker %||% cores
        
        # Calculate workers based on available cores and load
        n_server_workers <- floor((remote_cores * server_max_load) / cores_per_worker)
        
        message("  Detected ", remote_cores, " cores, allocating ", 
                n_server_workers, " workers (", cores_per_worker, " cores each)")
        
      }, error = function(e) {
        warning("Failed to connect to ", server_name, ": ", e$message)
        return(NULL)
      })
    }
    
    if (is.null(n_server_workers) || n_server_workers < 1) {
      warning("Server ", server_name, " has insufficient cores or invalid config. Skipping.")
      return(NULL)
    }
    
    # Return connection string repeated for each worker
    return(rep(connection, n_server_workers))
  }) |> 
    purrr::compact() |> # Remove NULLs from failed connections
    purrr::list_c()
  
  if (length(remote_workers) > 0) {
    future::plan(future::cluster, workers = remote_workers)
    message("\nConfigured ", length(remote_workers), " total remote workers")
  } else {
    # LOCAL fallback
    message("No remote workers available. Falling back to local execution.")
    local_cores  <- parallelly::availableCores(methods = "system")
    worker_limit <- floor((local_cores * max_load) / cores)
    
    if (worker_limit < 1) {
      warning("Low core count! Using 1 worker, but performance may be suboptimal.")
      worker_limit <- 1
    }
    
    future::plan(future::multisession, workers = worker_limit)
  }

  futr_status()
  return(invisible(TRUE))
}


#' Refresh the Current Future Plan
#'
#' Tears down the existing future cluster and re-establishes it using the same
#' arguments passed to the most recent `futr_establish()` call. Useful when
#' remote connections drop or become unresponsive.
#'
#' @return Invisibly returns the result of `futr_establish()`.
#' @export
futr_refresh <- function() {
  if (!exists(".future_last_args", envir = .GlobalEnv)) {
    stop("No previous futr_establish() call found. Run futr_establish() first.")
  }
  message("Refreshing future plan (tearing down existing connections)...")
  future::plan(future::sequential)  # cleanly shut down existing workers
  do.call(futr_establish, .future_last_args)
}


#' Close the Future Connection and Revert to Local Execution
#'
#' Shuts down any active remote cluster workers and reverts to a simple local
#' plan. Defaults to `future::sequential` (single-threaded), which is the
#' safest fallback. Set `multisession = TRUE` to keep using multiple local cores.
#'
#' @param multisession Logical. If TRUE, fall back to `future::multisession`
#'   (multiple local cores) instead of `future::sequential`. Default: FALSE.
#' @param workers Integer. Number of local workers when `multisession = TRUE`.
#'   Defaults to half the available local cores (rounded down, minimum 1).
#'
#' @return Invisibly returns TRUE.
#' @export
futr_close <- function(multisession = FALSE, workers = NULL) {
  message("Closing future connection...")
  future::plan(future::sequential)  # shuts down remote/local cluster workers

  if (multisession) {
    if (is.null(workers)) {
      local_cores <- parallelly::availableCores(methods = "system")
      workers <- max(1L, floor(local_cores / 2L))
    }
    future::plan(future::multisession, workers = workers)
    message("Reverted to local multisession with ", workers, " workers.")
  } else {
    message("Reverted to local sequential execution.")
  }

  futr_status()
  return(invisible(TRUE))
}


#' Query Status of the Current Future Plan
#'
#' Displays the active future plan type, number of workers, and — for remote
#' cluster plans — the R version and hostname of each unique worker node.
#' Automatically called by `futr_establish()` and `futr_close()`, but can
#' also be invoked directly at any time.
#'
#' @param per_worker Logical. If TRUE, list every individual worker with its
#'   PID rather than grouping by node. Default: FALSE.
#' @param sysinfo Logical. If TRUE, add OS platform and CPU architecture for
#'   each node. Default: FALSE.
#'
#' @return Invisibly returns a data frame of worker info (or NULL for sequential).
#' @export
futr_status <- function(per_worker = FALSE, sysinfo = FALSE) {
  plan_info <- future::plan()
  n_workers <- future::nbrOfWorkers()
  plan_type <- class(plan_info)[1]

  message("\n-- Future Status ", strrep("-", 30))
  message("Plan    : ", plan_type)
  message("Workers : ", n_workers)

  if (!inherits(plan_info, c("sequential", "uniprocess"))) {
    worker_data <- tryCatch({
      furrr::future_map(
        seq_len(n_workers),
        ~ {
          info <- Sys.info()
          c(
            list(
              worker    = .x,
              hostname  = info[["nodename"]],
              r_version = R.version.string,
              pid       = Sys.getpid()
            ),
            if (sysinfo) list(
              os        = info[["sysname"]],
              arch      = R.version[["arch"]]
            )
          )
        },
        .options = furrr::furrr_options(seed = NULL)
      )
    }, error = function(e) {
      warning("Could not query worker info: ", e$message)
      NULL
    })

    if (!is.null(worker_data)) {
      result <- do.call(rbind, lapply(worker_data, as.data.frame))

      if (per_worker) {
        message("\nWorker details:")
        for (i in seq_len(nrow(result))) {
          row <- result[i, ]
          line <- paste0(
            "  [", row$worker, "]  ", row$hostname,
            "  |  ", row$r_version,
            "  |  PID ", row$pid
          )
          if (sysinfo) line <- paste0(line, "  |  ", row$os, " (", row$arch, ")")
          message(line)
        }
      } else {
        # Group by node (hostname + r_version + optional sysinfo)
        group_cols <- c("hostname", "r_version", if (sysinfo) c("os", "arch"))
        keys <- apply(result[, group_cols, drop = FALSE], 1, paste, collapse = "\t")

        message("\nWorker nodes:")
        for (k in unique(keys)) {
          n_on  <- sum(keys == k)
          parts <- strsplit(k, "\t", fixed = TRUE)[[1]]
          line  <- paste0("  ", parts[1], "  |  ", parts[2], "  |  ", n_on, " worker(s)")
          if (sysinfo) line <- paste0(line, "  |  ", parts[3], " (", parts[4], ")")
          message(line)
        }
      }

      message(strrep("-", 43), "\n")
      return(invisible(result))
    }
  }

  # Local fallback info
  info <- Sys.info()
  line <- paste0("  ", info[["nodename"]], "  |  ", R.version.string, "  (local)")
  if (sysinfo) line <- paste0(line, "  |  ", info[["sysname"]], " (", R.version[["arch"]], ")")
  message(line)
  message(strrep("-", 43), "\n")
  return(invisible(NULL))
}

