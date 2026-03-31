# Check that local model files match the ones on OSF
# Project: xq6wr  |  OSF path: Data/derivatives/Models
#
# Defined as a function so it can be called from data/ with ProjectTemplate's
# relative_path variable, keeping the logic out of the auto-load pipeline.

check_models_vs_osf <- function(relative_path = "") {

  if (!requireNamespace("osfr", quietly = TRUE)) install.packages("osfr")
  library(osfr)

  OSF_PROJECT_ID <- "xq6wr"
  OSF_FOLDER     <- "Data/derivatives/Models"
  LOCAL_DIR      <- paste0(relative_path, "data")

  # ── Helpers ─────────────────────────────────────────────────────────────────

  osf_navigate <- function(node, path) {
    parts   <- strsplit(path, "/")[[1]]
    current <- node
    for (part in parts) {
      contents <- osf_ls_files(current, type = "folder")
      idx      <- which(contents$name == part)
      if (length(idx) == 0)
        stop("OSF folder not found: '", part, "'")
      current <- contents[idx, ]
    }
    current
  }

  pluck_meta <- function(meta, ...) {
    for (path in list(...)) {
      val <- tryCatch(Reduce(`[[`, path, init = meta[[1]]), error = function(e) NULL)
      if (!is.null(val) && length(val) == 1) return(val)
    }
    NULL
  }

  osf_file_info <- function(remote_files) {
    get_size <- function(meta) {
      val <- pluck_meta(meta,
        c("attributes", "size"),
        c("data", "attributes", "size"),
        c("size")
      )
      if (is.null(val)) NA_real_ else as.numeric(val)
    }
    get_md5 <- function(meta) {
      val <- pluck_meta(meta,
        c("attributes", "extra", "hashes", "md5"),
        c("data", "attributes", "extra", "hashes", "md5"),
        c("extra", "hashes", "md5")
      )
      if (is.null(val)) NA_character_ else as.character(val)
    }
    df <- data.frame(
      name = remote_files$name,
      size = vapply(remote_files$meta, get_size, numeric(1)),
      md5  = vapply(remote_files$meta, get_md5,  character(1)),
      stringsAsFactors = FALSE
    )
    if (all(is.na(df$size)))
      message("[WARN] Could not extract file sizes from OSF metadata; ",
              "run str(remote_raw$meta[[1]], max.level = 4) to inspect. ",
              "Falling back to filename-only comparison.")
    df
  }

  # ── Remote ───────────────────────────────────────────────────────────────────

  message("Connecting to OSF project: ", OSF_PROJECT_ID)
  project    <- osf_retrieve_node(OSF_PROJECT_ID)
  folder     <- osf_navigate(project, OSF_FOLDER)
  remote_raw <- osf_ls_files(folder, type = "file", n_max = Inf)
  message(nrow(remote_raw), " remote file(s) found.")
  remote <- osf_file_info(remote_raw)

  # ── Local ────────────────────────────────────────────────────────────────────

  if (!dir.exists(LOCAL_DIR))
    stop("Local directory does not exist: ", LOCAL_DIR)

  local_full  <- list.files(LOCAL_DIR, pattern = "\\.[Rr][Dd]ata$",
                             full.names = TRUE, recursive = TRUE)
  local_names <- basename(local_full)

  dups <- duplicated(local_names)
  if (any(dups)) {
    message("[INFO] Duplicate filenames found locally (keeping first occurrence):\n",
            paste0("  ", local_full[dups], collapse = "\n"))
    local_full  <- local_full[!dups]
    local_names <- local_names[!dups]
  }

  local_paths <- setNames(local_full, local_names)
  local <- data.frame(
    name = local_names,
    size = file.size(local_paths),
    md5  = tools::md5sum(local_paths),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # ── Compare ──────────────────────────────────────────────────────────────────

  only_remote    <- setdiff(remote$name, local$name)
  only_local     <- setdiff(local$name,  remote$name)
  in_both        <- intersect(remote$name, local$name)
  size_available <- !all(is.na(remote$size))

  check_both <- do.call(rbind, lapply(in_both, function(nm) {
    r <- remote[remote$name == nm, ]
    l <- local[local$name   == nm, ]
    if (!size_available) {
      status <- "NAME_MATCH"
    } else {
      size_match <- isTRUE(r$size[1] == l$size[1])
      md5_match  <- isTRUE(!is.na(r$md5[1])) && isTRUE(!is.na(l$md5[1])) &&
                    isTRUE(r$md5[1] == l$md5[1])
      status <- if (md5_match) "OK" else if (size_match) "SIZE_MATCH_MD5_DIFFER" else "MISMATCH"
    }
    data.frame(
      name        = nm,
      local_size  = l$size[1],
      remote_size = r$size[1],
      local_md5   = l$md5[1],
      remote_md5  = if (is.na(r$md5[1])) "(not available)" else r$md5[1],
      status      = status,
      stringsAsFactors = FALSE
    )
  }))

  # ── Report ───────────────────────────────────────────────────────────────────

  problems <- 0

  cat("\n========== OSF vs Local Model Check ==========\n")
  cat("OSF:   ", OSF_PROJECT_ID, " /", OSF_FOLDER, "\n")
  cat("Local: ", LOCAL_DIR, " (recursive)\n\n")

  if (length(only_remote) == 0) {
    cat("[OK] No files missing locally.\n")
  } else {
    cat("[MISSING] Files on OSF but NOT local (", length(only_remote), "):\n", sep = "")
    cat(paste0("  - ", only_remote, "\n"), sep = "")

    answer <- readline(prompt = "Download missing file(s) from OSF now? [y/N]: ")
    if (tolower(trimws(answer)) == "y") {
      missing_rows <- remote_raw[remote_raw$name %in% only_remote, ]
      for (i in seq_len(nrow(missing_rows))) {
        message("[", i, "/", nrow(missing_rows), "] Downloading: ", missing_rows$name[i])
        osf_download(missing_rows[i, ], path = LOCAL_DIR, conflicts = "skip", verbose = TRUE)
      }
    } else {
      problems <- problems + length(only_remote)
    }
  }

  if (length(only_local) == 0) {
    cat("[OK] No extra local RData files (not on OSF).\n")
  } else {
    cat("[INFO] Local RData files not on OSF (", length(only_local), "):\n", sep = "")
    cat(paste0("  - ", only_local, "\n"), sep = "")
  }

  if (nrow(check_both) > 0) {
    if (!size_available) {
      cat("[INFO] ", nrow(check_both), " file(s) matched by name (size unavailable from OSF).\n", sep = "")
    } else {
      ok  <- check_both[check_both$status %in% c("OK", "SIZE_MATCH_MD5_DIFFER", "NAME_MATCH"), ]
      bad <- check_both[check_both$status == "MISMATCH", ]
      problems <- problems + nrow(bad)
      cat("[OK] ", nrow(ok), " file(s) match.\n", sep = "")
      if (nrow(bad) > 0) {
        cat("[MISMATCH] ", nrow(bad), " file(s) differ:\n", sep = "")
        for (i in seq_len(nrow(bad))) {
          b <- bad[i, ]
          cat("  -", b$name, "\n")
          cat("      status     :", b$status, "\n")
          cat("      local size :", b$local_size,  "bytes\n")
          cat("      remote size:", b$remote_size, "bytes\n")
          cat("      local md5  :", b$local_md5, "\n")
          cat("      remote md5 :", b$remote_md5, "\n")
        }
      }
    }
  }

  cat("===============================================\n\n")

  if (problems > 0)
    stop("Check FAILED: ", problems, " problem(s) found.")

  message("Check PASSED: all OSF models accounted for locally.")
  invisible(TRUE)
}
