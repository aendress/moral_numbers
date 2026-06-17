# verify_bootstrap_mismatches.R
#
# Investigates the three mismatches found in verify_bootstrap_dimensions.R:
#
#   1. bootstrap.fits.RData      — same dimensions but mismatch: check column names
#   2. dat.moral.numbers.RData   — row/column count differs: check experiments and columns
#
# (bootstrap.asymptote.exp8/10.RData are confirmed correct from code inspection:
#  16 conditions x 1000 samples = 16000 safe; 16 x 10000 = 160000 new.)
#
# Usage: source(here::here("code", "verify_bootstrap_mismatches.R"))

dir_new  <- here::here("code", "output")
dir_safe <- here::here("code", "output_from_backup_23-03-26")

load_rdata <- function(path) {
    e <- new.env(parent = emptyenv())
    load(path, envir = e)
    e
}

# ── 1. bootstrap.fits.RData: column names ─────────────────────────────────────

cat("================================================================\n")
cat("1. bootstrap.fits.RData — column name comparison\n")
cat("================================================================\n\n")

cat("Loading new ... ")
e_new  <- load_rdata(file.path(dir_new,  "bootstrap.fits.RData"))
cat("done\nLoading safe ... ")
e_safe <- load_rdata(file.path(dir_safe, "bootstrap.fits.RData"))
cat("done\n\n")

for (obj in c("dat.moral.numbers.bootstrap.1param.summary",
              "dat.moral.numbers.bootstrap.2param.summary")) {
    cn_new  <- colnames(get(obj, envir = e_new))
    cn_safe <- colnames(get(obj, envir = e_safe))

    cat(sprintf("--- %s ---\n", obj))

    only_new  <- setdiff(cn_new,  cn_safe)
    only_safe <- setdiff(cn_safe, cn_new)

    if (length(only_new) == 0 && length(only_safe) == 0) {
        cat("  Column names identical.\n")
    } else {
        cat("  Only in new: ",  paste(only_new,  collapse = ", "), "\n")
        cat("  Only in safe:", paste(only_safe, collapse = ", "), "\n")
    }
    cat("\n")
}

rm(e_new, e_safe)
gc(verbose = FALSE)

# ── 2. dat.moral.numbers.RData: experiments and columns ───────────────────────

cat("================================================================\n")
cat("2. dat.moral.numbers.RData — experiment counts and column diff\n")
cat("================================================================\n\n")

cat("Loading new ... ")
e_new  <- load_rdata(file.path(dir_new,  "dat.moral.numbers.RData"))
cat("done\nLoading safe ... ")
e_safe <- load_rdata(file.path(dir_safe, "dat.moral.numbers.RData"))
cat("done\n\n")

cat("--- Row counts by experimentID ---\n")
cat("new:\n")
print(sort(table(e_new$dat.moral.numbers$experimentID), decreasing = TRUE))
cat("\nsafe:\n")
print(sort(table(e_safe$dat.moral.numbers$experimentID), decreasing = TRUE))

cat("\n--- Columns only in safe (missing from new) ---\n")
only_safe <- setdiff(colnames(e_safe$dat.moral.numbers), colnames(e_new$dat.moral.numbers))
if (length(only_safe) == 0) cat("  (none)\n") else print(only_safe)

cat("\n--- Columns only in new (not in safe) ---\n")
only_new <- setdiff(colnames(e_new$dat.moral.numbers), colnames(e_safe$dat.moral.numbers))
if (length(only_new) == 0) cat("  (none)\n") else print(only_new)

rm(e_new, e_safe)
gc(verbose = FALSE)

cat("\nDone.\n")
