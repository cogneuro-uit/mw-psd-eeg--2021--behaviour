# Download model files from OSF
# Project: xq6wr  |  OSF path: Data/Models
# Files are downloaded to data/ and skipped if they already exist locally.

if (!requireNamespace("osfr", quietly = TRUE)) install.packages("osfr")
library(osfr)

# ── Config ────────────────────────────────────────────────────────────────────

OSF_PROJECT_ID <- "xq6wr"
OSF_FOLDER     <- "Data/derivatives/Models"   # path inside the OSF project (case-sensitive)
LOCAL_DIR      <- here::here("data")   # destination directory

# ── Helpers ───────────────────────────────────────────────────────────────────

# Traverse nested OSF folders by splitting a path string like "Data/Models"
osf_navigate <- function(node, path) {
  parts <- strsplit(path, "/")[[1]]
  current <- node
  for (part in parts) {
    contents <- osf_ls_files(current, type = "folder")
    idx <- which(contents$name == part)
    if (length(idx) == 0)
      stop("OSF folder not found: '", part, "'")
    current <- contents[idx, ]
  }
  current
}

# ── Main ──────────────────────────────────────────────────────────────────────

message("Connecting to OSF project: ", OSF_PROJECT_ID)
project <- osf_retrieve_node(OSF_PROJECT_ID)

message("Navigating to: ", OSF_FOLDER)
folder <- osf_navigate(project, OSF_FOLDER)

message("Listing remote files...")
remote_files <- osf_ls_files(folder, type = "file", n_max = Inf)

if (nrow(remote_files) == 0) {
  message("No files found in ", OSF_FOLDER)
  quit(status = 0)
}

message(nrow(remote_files), " file(s) found on OSF.")

# Check which files are missing locally
local_paths  <- file.path(LOCAL_DIR, remote_files$name)
already_here <- file.exists(local_paths)

if (all(already_here)) {
  message("All files already present locally — nothing to download.")
  quit(status = 0)
}

to_download <- remote_files[!already_here, ]
message(
  sum(already_here), " file(s) skipped (already exist), ",
  nrow(to_download), " file(s) to download."
)

for (i in seq_len(nrow(to_download))) {
  fname <- to_download$name[i]
  message("[", i, "/", nrow(to_download), "] Downloading: ", fname)
  osf_download(
    to_download[i, ],
    path      = LOCAL_DIR,
    conflicts = "skip",   # belt-and-suspenders: never overwrite
    verbose   = TRUE
  )
}

message("Done. Models stored in: ", LOCAL_DIR)
