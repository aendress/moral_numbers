# verify_bootstrap_dimensions.R
#
# Compares dimensions of all R objects stored in bootstrap output files
# between code/output (new run) and the safety copy directory
# (output.save.all.subj.no.constant.no.fast.exclusions).
#
# Memory strategy:
#   - Each file is unloaded (rm + gc) immediately after its metadata is extracted.
#   - For regular .rds files both objects are kept in memory simultaneously only
#     long enough to call obj_dims(); they are removed before the next file.
#   - dat.moral.numbers.bootstrap.samples.rds (~2.8 GB) is treated specially:
#     loaded last, new and safe are loaded one at a time, metadata extracted and
#     the object removed before the other file is loaded, so the two giants are
#     never in memory together.
#
# Usage: source(here::here("code", "verify_bootstrap_dimensions.R"))

dir_new  <- here::here("code", "output")
dir_safe <- here::here("code", "output_from_backup_23-03-26")

LARGE_FILE <- "dat.moral.numbers.bootstrap.samples.rds"

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Summarise the shape of an R object one level deep
obj_dims <- function(x) {
  if (is.data.frame(x)) {
    list(type = "data.frame", nrow = nrow(x), ncol = ncol(x), colnames = colnames(x))
  } else if (is.matrix(x)) {
    list(type = "matrix", nrow = nrow(x), ncol = ncol(x))
  } else if (is.list(x)) {
    # For lists, capture length/names and, for any data-frame elements, their dims
    elem_dims <- purrr::map(x, function(el) {
      if (is.data.frame(el)) list(nrow = nrow(el), ncol = ncol(el), colnames = colnames(el))
      else if (is.list(el))  list(length = length(el))
      else                   list(length = length(el))
    })
    list(type = "list", length = length(x), names = names(x), elem_dims = elem_dims)
  } else {
    list(type = class(x)[1], length = length(x))
  }
}

# Return TRUE iff two dimension summaries represent the same shape
dims_equal <- function(d1, d2) {
  check_field <- function(field) {
    v1 <- d1[[field]]
    v2 <- d2[[field]]
    if (is.null(v1) && is.null(v2)) return(TRUE)
    if (is.null(v1) || is.null(v2)) return(FALSE)
    identical(v1, v2)
  }
  fields <- c("nrow", "ncol", "colnames", "length", "names", "elem_dims")
  all(purrr::map_lgl(fields, check_field))
}

# Format a dimension summary as a short string for display
fmt_dims <- function(d) {
  if (!is.null(d$nrow)) {
    sprintf("%s  [%d x %d]", d$type, d$nrow, d$ncol)
  } else if (d$type == "list") {
    sprintf("list  [length=%d, names: %s]", d$length,
            if (is.null(d$names)) "<unnamed>" else paste(d$names, collapse = ", "))
  } else {
    sprintf("%s  [length=%d]", d$type, d$length)
  }
}

# Load an .RData file into a fresh environment and return the environment
load_rdata <- function(path) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  e
}

# ---------------------------------------------------------------------------
# Compare one ordinary .rds file; both objects loaded, metadata extracted,
# both removed, gc() called before returning.
# ---------------------------------------------------------------------------
compare_rds <- function(fname) {
  path_new  <- file.path(dir_new,  fname)
  path_safe <- file.path(dir_safe, fname)

  cat("  Loading new ... ")
  obj_new <- readRDS(path_new)
  cat("done\n")
  d_new <- obj_dims(obj_new)
  rm(obj_new)

  cat("  Loading safe ... ")
  obj_safe <- readRDS(path_safe)
  cat("done\n")
  d_safe <- obj_dims(obj_safe)
  rm(obj_safe)

  gc(verbose = FALSE)

  if (dims_equal(d_new, d_safe)) {
    cat(sprintf("  [OK]  %s\n", fmt_dims(d_new)))
    "OK"
  } else {
    cat("  [MISMATCH]\n")
    cat(sprintf("    new:  %s\n", fmt_dims(d_new)))
    cat(sprintf("    safe: %s\n", fmt_dims(d_safe)))
    "MISMATCH"
  }
}

# ---------------------------------------------------------------------------
# Compare the large .rds file: load new, extract metadata, unload; then load
# safe, extract metadata, unload; then compare. Never both in memory at once.
# ---------------------------------------------------------------------------
compare_large_rds <- function(fname) {
  path_new  <- file.path(dir_new,  fname)
  path_safe <- file.path(dir_safe, fname)

  cat("  Loading new (large file) ... ")
  obj <- readRDS(path_new)
  cat("done\n  Extracting metadata ... ")
  d_new <- obj_dims(obj)
  rm(obj)
  gc(verbose = FALSE)
  cat("done, object unloaded\n")

  cat("  Loading safe (large file) ... ")
  obj <- readRDS(path_safe)
  cat("done\n  Extracting metadata ... ")
  d_safe <- obj_dims(obj)
  rm(obj)
  gc(verbose = FALSE)
  cat("done, object unloaded\n")

  if (dims_equal(d_new, d_safe)) {
    cat(sprintf("  [OK]  %s\n", fmt_dims(d_new)))
    "OK"
  } else {
    cat("  [MISMATCH]\n")
    cat(sprintf("    new:  %s\n", fmt_dims(d_new)))
    cat(sprintf("    safe: %s\n", fmt_dims(d_safe)))
    "MISMATCH"
  }
}

# ---------------------------------------------------------------------------
# Compare one .RData file; both environments loaded, metadata extracted,
# both removed, gc() called before returning.
# ---------------------------------------------------------------------------
compare_rdata <- function(fname) {
  path_new  <- file.path(dir_new,  fname)
  path_safe <- file.path(dir_safe, fname)

  cat("  Loading new ... ")
  env_new  <- load_rdata(path_new)
  cat("done\n  Loading safe ... ")
  env_safe <- load_rdata(path_safe)
  cat("done\n")

  obj_names_safe <- ls(env_safe)
  obj_names_new  <- ls(env_new)
  file_ok <- TRUE

  for (obj_name in sort(obj_names_safe)) {
    if (!obj_name %in% obj_names_new) {
      cat(sprintf("  [MISSING] %s  (in safe copy but absent from new file)\n", obj_name))
      file_ok <- FALSE
    } else {
      d_new  <- obj_dims(get(obj_name, envir = env_new))
      d_safe <- obj_dims(get(obj_name, envir = env_safe))

      if (dims_equal(d_new, d_safe)) {
        cat(sprintf("  [OK]       %-55s  %s\n", obj_name, fmt_dims(d_new)))
      } else {
        cat(sprintf("  [MISMATCH] %s\n", obj_name))
        cat(sprintf("    new:  %s\n", fmt_dims(d_new)))
        cat(sprintf("    safe: %s\n", fmt_dims(d_safe)))
        file_ok <- FALSE
      }
    }
  }

  extra <- sort(setdiff(obj_names_new, obj_names_safe))
  if (length(extra) > 0) {
    cat(sprintf("  [EXTRA]   Objects in new file not in safe copy: %s\n",
                paste(extra, collapse = ", ")))
  }

  rm(env_new, env_safe)
  gc(verbose = FALSE)

  if (file_ok) "OK" else "MISMATCH"
}

# ---------------------------------------------------------------------------
# Build file list: large file always last
# ---------------------------------------------------------------------------

files_new  <- list.files(dir_new,  pattern = "\\.(RData|rds)$", ignore.case = FALSE)
files_safe <- list.files(dir_safe, pattern = "\\.(RData|rds)$", ignore.case = FALSE)
files_both <- sort(intersect(files_new, files_safe))

# Move LARGE_FILE to end
files_both <- c(
  files_both[files_both != LARGE_FILE],
  files_both[files_both == LARGE_FILE]
)

cat(sprintf(
  "Directories:\n  new:  %s\n  safe: %s\n\nComparing %d files (%s processed last)\n\n",
  dir_new, dir_safe, length(files_both), LARGE_FILE
))

results <- character(0)

# ---------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------

for (fname in files_both) {
  cat(sprintf("=== %s ===\n", fname))

  tryCatch({
    results[fname] <- if (fname == LARGE_FILE) {
      compare_large_rds(fname)
    } else if (grepl("\\.rds$", fname, ignore.case = TRUE)) {
      compare_rds(fname)
    } else {
      compare_rdata(fname)
    }
  }, error = function(e) {
    cat(sprintf("  [ERROR] %s\n", conditionMessage(e)))
    results[fname] <<- "ERROR"
  })

  cat("\n")
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

cat("=== SUMMARY ===\n")
n_ok      <- sum(results == "OK")
n_problem <- sum(results != "OK")

cat(sprintf("  OK:       %d / %d files\n", n_ok, length(results)))
cat(sprintf("  Problems: %d / %d files\n", n_problem, length(results)))

if (n_problem > 0) {
  cat("\nFiles with problems:\n")
  for (f in names(results[results != "OK"])) {
    cat(sprintf("  [%s]  %s\n", results[f], f))
  }
} else {
  cat("\nAll checked files match the safety copy.\n")
}
