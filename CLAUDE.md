# Project rules

## R code style

### Explicit package calls
Always use explicit `package::function()` calls. Never use unqualified function names from non-base packages. Examples:
- `dplyr::mutate()` not `mutate()`
- `stringr::str_detect()` not `str_detect()`
- `ggplot2::ggplot()` not `ggplot()`

### Idiomatic tidyverse
- Use `dplyr::mutate()`, `dplyr::filter()`, `dplyr::select()` etc. for data manipulation
- Use `tidyr::pivot_longer()` / `tidyr::pivot_wider()` (not `gather()`/`spread()`)
- Use `purrr::map()` family instead of `lapply()`/`sapply()`
- Pipe with `%>%`
- Functions that operate on a piped data frame should use `function(dat = .)` — never `function(.)` with `. %>% ...` in the body, as magrittr will interpret `. %>%` as a functional sequence rather than piping the parameter value

### R utility library
- `~/R.ansgar/ansgarlib/R/tt.R` — custom utility library providing helper functions for t-tests, Wilcoxon tests, ANOVAs, effect sizes, data manipulation, and custom violin plots (`violin_plot_template()`, `add_sig_labels()`)


## Safety copies
Before modifying any file, save a timestamped safety copy:
```r
file.copy("path/to/file.R", paste0("path/to/file.R.", format(Sys.time(), "%Y%m%d_%H%M%S")))
```
Or in bash: `cp file.R file.R.$(date +%Y%m%d_%H%M%S)`

After completing a task for which a safety copy was made, verify that the only differences between the modified file and its safety copy are the intended edits (e.g. using `diff file.R file.R.<timestamp>`).

## Project files

### Helper functions
- `code/helper_functions/` — R helper files sourced automatically at startup; see `code/helper_functions/README.md` for a full description of each file and function
- **Keep `README.md` up to date**: whenever a function is added to or removed from a helper file, or a new helper file is created, update `code/helper_functions/README.md` accordingly

### R Markdown documents
- `code/moral_numbers.Rmd` — **MAIN FILE**: the primary paper document containing all experiments, analyses, model code, and manuscript text
- `code/child_rmds/moral_numbers_LLMs.Rmd` — LLM comparison analyses (humans vs. frontier and local LLMs across experiments)
- `code/child_rmds/loss_aversion.Rmd` — loss aversion modelling and illustrations (S-curve, loss/gain ratio plots, comparison to Tversky & Kahneman 1992)
- `code/child_rmds/` — child RMD files knitted into the main document

