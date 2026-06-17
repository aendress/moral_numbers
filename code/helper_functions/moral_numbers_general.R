# ----- Data input/output -----

get.question.labels.from.qsf <- function(f, filter.str = "scenario", show.offending.lines = FALSE){
    
    if(filter.str == "scenario"){
        
        warning("filter.str is set to the default value of 'scenario'. If you are missing some expected question IDs, set filter.str to '.' and then look for the filter.str you actually want to use.", immediate. = TRUE)
        
    }
    
    # Open file connections
    con.questionLabels <- file (f, open = "r")     
    
    dat.questionLabels <- data.frame (line = readLines(con.questionLabels)) 
    
    close (con.questionLabels)
    
    # Extract scenarios
    ## Check if there are occurrences of '"QuestionID":' before '"DataExportTag":'
    dat.questionLabels.offending.lines <- dat.questionLabels %>% 
        dplyr::filter(str_detect(line, "QuestionID.*\"DataExportTag"))
    if(nrow(dat.questionLabels.offending.lines) > 0) {
        
        if(show.offending.lines){
            # Assume we are running Rstudio
            
            view(dat.questionLabels.offending.lines)
        }
        
        stop("Some Question ID's occurred before the export tag; did you insert line breaks before \'{\"SurveyID\":\' in the QSF file? The offending file is ", f, " There are ", nrow(dat.questionLabels.offending.lines), " offending lines.")
    }
    
    ## Now extract the labels
    dat.questionLabels <- 
        dat.questionLabels %>% 
        # Find lines where DataExportTag precedes QuestionID
        dplyr::filter(str_detect(line, "DataExportTag.*QuestionID")) %>% 
        # Replace everything from the beginning to `"DataExportTag":"` with the following string up to and INCLUDING `"`
        dplyr::mutate(line = str_replace(line, "^.*(\"DataExportTag\":\")(.*\")", "\\2")) %>% 
        # Replace everything from `",` until `QuestionID":"(QID\\d+)` and then until the end; keep the question ID, and insert a comma before
        dplyr::mutate(line = str_replace(line, "\",.*QuestionID\":\"(QID\\d+).*$", ",\\1")) %>% 
        tidyr::separate(line, c("label", "qid"), sep = ",")  %>% 
        dplyr::filter(str_detect(label, filter.str))
    
    
    v.questionLabels <- 
        dat.questionLabels %>% 
        tibble::deframe()
    
    v.questionLabels
}

# ----- Reformatting -----

get.number.ratio <- function(rrr = ., return.factor = TRUE) {
    
    rrr.str <- dplyr::case_when(
        # Whole numbers 
        rrr == as.integer(rrr) ~ paste0(rrr, ":1"),
        rrr == 3/2 ~ "3:2",
        rrr == 4/3 ~ "4:3",
        TRUE ~ NA_character_
    )
    
    
    if(return.factor){
        rrr.str <- factor(rrr.str)
    }
    
    rrr.str
    
}

separate.multiple.field.value.pairs <- function(dat = ., col = condition.name, field.sep = "\\.", key.sep = ":", replace.dot.with.colon = TRUE){
    
    col <- dplyr::enquo(col)
    
    if(replace.dot.with.colon){
        dat <- dat %>% 
            dplyr::mutate(!!col := str_replace_all(!!col, "\\.(\\d)", ":\\1"))
    }
    
    v.new.col.names <-  dat %>% 
        pull (!!col) %>% 
        str_split(field.sep, simplify = TRUE) %>% 
        as.data.frame  %>% 
        dplyr::mutate(across(everything (), ~ str_remove(.x, paste0(key.sep, ".*$")))) %>% 
        distinct %>% 
        as.list() %>% 
        unlist 
    
    dat %>%
        dplyr::mutate(!!col := purrr::map(!!col,
                                          ~ str_split(.x, field.sep, simplify = TRUE) %>% 
                                              as.data.frame  %>% 
                                              dplyr::mutate(across(everything (), ~ str_remove(.x, paste0("^.*", key.sep)))) %>% 
                                              set_names (v.new.col.names))) %>% 
        tidyr::unnest(!!col) 
    
    
    
    
}            

scenarios_to_long <- function(dat = .) {
    dat %>%
        dplyr::mutate(dplyr::across(dplyr::starts_with("scenario"),
                                    ~ gsub("^(\\d+).*", "\\1", .))) %>%
        dplyr::mutate(dplyr::across(dplyr::starts_with("scenario"),
                                    as.numeric)) %>%
        tidyr::pivot_longer(
            cols = dplyr::starts_with("scenario"),
            names_to = "condition.name",
            values_to = "rating",
            values_drop_na = TRUE
        )
}

calculate_number_conds <- function(dat = ., n_options = 6){
    dat %>% 
        dplyr::rename_with(~ str_replace(.x, "n_", "n."), starts_with("n_"),) %>% 
        dplyr::mutate(across(starts_with("n."), as.numeric)) %>% 
        dplyr::rename("rating.raw" = "rating") %>% 
        dplyr::mutate(rating.bin = 1 * (rating.raw > n_options / 2 ), .after = "rating.raw") %>% 
        dplyr::mutate(n.net.saved = n.saved - n.victims, 
                      ratio = n.saved / n.victims,
                      p.saved = n.saved / n.total,
                      .after = "n.victims") 
    
}

split_condition_name <- function(dat = ., col_names = c("scenarioID"), excluded_split_cols = c(1)) {
    
    # This is the column where the remaining information from condition.name goes
    col_names <- c(col_names, "condition.name")
    num_fields <- length(col_names) + length(excluded_split_cols)
    
    dat %>%
        dplyr::mutate(
            tmp = purrr::map(
                condition.name,
                ~ stringr::str_split_fixed(.x, "\\.", num_fields)[, -excluded_split_cols] %>%
                    t() %>%
                    as.data.frame(stringsAsFactors = FALSE) %>%
                    rlang::set_names(col_names)
            ),
            .keep = "unused"
        ) %>%
        tidyr::unnest(tmp)
}

convert.dates.and.calculate.duration <- function(dat = .) {
    dat %>%
        dplyr::mutate(across(dplyr::ends_with("Date"),
                             ~ as.POSIXct(.x, tz = "GMT", format = "%m/%d/%Y %H:%M"))) %>%
        dplyr::mutate(duration = difftime (endDate,
                                           startDate,
                                           units = "mins"))
}



# ----- Summarizing -----

summarize.first.by.subj.then.by.overall <- function(dat = .) {
    dat %>%
        group_by(ResponseId, n.saved, n.victims, number.range, number.ratio) %>%
        summarize_at (vars(contains("rating")),
                      mean) %>%
        group_by(n.saved, n.victims, number.range, number.ratio) %>%
        summarize_at (vars(contains("rating")),
                      list(~length(.), ~mean(.), ~se(.))) %>%
        dplyr::select(!matches ("rating"),
                      contains ("raw"),
                      contains ("bin")) %>%
        dplyr::select(-rating.bin_length) %>%
        dplyr::rename("N" = "rating.raw_length")
}


get.rating.mean.se <- function(dat = .){
    
    .df.m <- dat %>% 
        summarize_at(vars(contains("rating")), mean) %>% 
        gather(rating.type, rating.m, contains("rating"))
    
    .df.se <- dat %>% 
        summarize_at(vars(contains("rating")), se) %>% 
        gather(rating.type, rating.se, contains("rating"))
    
    dplyr::left_join(.df.m, .df.se)
}

# ----- Other -----

define_vivacity_col <- function(dat = .){
    
    dat %>% 
        dplyr::mutate(vivacity = dplyr::if_else(str_starts(condition.name, "vivid"),
                                                "vivid",
                                                "neutral"),
                      .after = "scenarioID") %>% 
        dplyr::mutate(condition.name = str_remove(condition.name, "^(vivid|neutral)\\."))
    
}

define_question_col <- function(dat = .) {
    
    dat %>% 
        dplyr::mutate(
            question = dplyr::case_when(
                str_ends(condition.name, "severity") ~ "severity",
                str_ends(condition.name, "acceptability") ~ "acceptability",
                TRUE ~ NA_character_
            )
        ) %>% 
        dplyr::mutate(
            condition.name = str_remove(condition.name, "\\.(acceptability|severity)$")
        )
    
}



make.Z.value.against.control <- function(dat = ., group = decisionType, controlCond = "moral", return.list = TRUE){
    
    group <- enquo(group)
    
    dat <- dat %>% 
        dplyr::ungroup()
    
    if(nrow(dat) != 2){
        stop("There are more than two conditions, double check your selection")
    }
    
    controlRow <- which((dat %>%
                             dplyr::pull(!!group)) %in% c(controlCond))
    
    
    # If the control condition is the second row, keep the order
    # If it's in the first row, switch the order
    row.order <- cyclic.shift(1:2, 2 - controlRow)
    
    dat <- dat[row.order,]
    
    dat.means <- dat %>% 
        dplyr::summarize(across(where(is.numeric), ~ -diff(.x))) %>% 
        dplyr::select(ends_with("_M")) %>% 
        dplyr::rename_all(~ str_remove(.x, "_M$")) 
    
    dat.SDs <- dat %>% 
        dplyr::filter(!!group == controlCond) %>% 
        dplyr::select(ends_with("_SD")) %>% 
        dplyr::rename_all(~ str_remove(.x, "_SD"))
    
    if(!identical(
        names(dat.means),
        names(dat.SDs))){
        stop("Columns for means and SD's don't match.")
    }
    
    if(return.list){
        
        (dat.means / dat.SDs) %>% 
            as.vector()
        
    } else {
        
        dat.means / dat.SDs
        
    }
}

add_z_relative_to_baseline <- function(dat = ., cond_col){
    
    cond_col <- rlang::enquo(cond_col)
    cond_col_name <- rlang::as_name(cond_col)
    
    
    
    dat_with_Z <- dat %>% 
        dplyr::select(experimentID, blocks, !!cond_col, dplyr::matches("_(M|SD)$")) %>% 
        tidyr::pivot_wider(
            names_from = !!cond_col,
            values_from = dplyr::matches("_(M|SD)$"),
            names_glue = paste0("{", cond_col_name, "}_{.value}")
        ) 
    
    if(cond_col_name == "vivacity"){
        dat_with_Z <- dat_with_Z %>% 
            dplyr::mutate(
                w_Z = (vivid_w_M - neutral_w_M) / neutral_w_SD,
                a_Z = (vivid_a_M - neutral_a_M) / neutral_a_SD
            ) %>% 
            dplyr::mutate(!!cond_col := "vivid")
    } else {
        dat_with_Z <- dat_with_Z %>% 
            dplyr::mutate(
                w_Z = (moral_w_M - economic_w_M) / economic_w_SD
            ) %>% 
            dplyr::mutate(!!cond_col := "moral")
    }
    
    dat_with_Z <- dat_with_Z %>% 
        dplyr::select(experimentID, blocks, !!cond_col, dplyr::ends_with("Z")) %>% 
        dplyr::mutate(dplyr::across(
            dplyr::ends_with("_Z"),
            list(p = ~ pnorm(.x, lower.tail=FALSE)*2)
        )
        )
    
    dplyr::left_join(
        dat,
        dat_with_Z,
        
        by = c("experimentID", "blocks", cond_col_name)
    ) %>% 
        dplyr::relocate(w_Z, w_Z_p, .after = "w_pi.upper") %>% 
        { if(cond_col_name == "vivacity") dplyr::relocate(., a_Z, a_Z_p, .after = "a_pi.upper") else . }
    
}        



rename.terms <- function(dat = ., col = Effect){
    
    col <- enquo(col)
    
    dat %>% 
        dplyr::mutate(!!col := map_chr(
            !!col,
            ~ mgsub::mgsub (
                .x,
                # c ("comparisonType" = "Experiment", 
                #    "decisionType" =  "Domain", 
                #    "first.scenario.type" = "First Domain", 
                #    "ratioorder" = "Ratio Order")))) %>% 
                pattern = c ("comparisonType",
                             "decisionType",
                             "first.scenario.type",
                             "ratioorder",
                             "block",
                             "scale\\(n\\.net\\.saved\\)",
                             "n\\.net\\.saved",
                             "n\\.victims\\.Z",
                             "n\\.victims",
                             ":"),
                replacement = c ("Experiment: ",
                                 "Domain: ",
                                 "First Domain: ",
                                 "Ratio Order: ",
                                 "Block: ",
                                 "utility",
                                 "utility",
                                 "harm",
                                 "harm",
                                 " $\\\\times$ "),
                ignore.case = TRUE))) %>% 
        dplyr::mutate(!!col := str_to_title(!!col)) %>% 
        dplyr::mutate(!!col := str_replace_all(!!col, fixed("\\Times"), fixed("\\times")))
}



# Combine a set of experiments into one combined ID.
# `pattern` is a regex matched against experimentID; the replacement is looked
# up from an internal table to ensure consistency.  Raises an error for unknown
# patterns so that new combinations are always registered here.
combine_exps <- function(dat = ., pattern) {
    replacements <- c(
        "exp2[ac]"     = "exp2ac",
        "^exp(8|10)$"  = "exp8+10",
        "^exp(10|11)$" = "exp10+11"
    )
    if (!pattern %in% names(replacements)) {
        stop("Unknown pattern '", pattern, "'. Add it to the lookup table in combine_exps().")
    }
    dat %>%
        dplyr::mutate(
            experimentID = stringr::str_replace(experimentID, pattern, replacements[[pattern]])
        )
}


get.original.experimentID <- function(experimentID = ., direction = c("paper2data", "data2paper")) {
    direction <- match.arg(direction)
    
    # Check direction and determine relevant columns for matching
    if (direction == "paper2data") {
        match_column <- "experimentID.paper"
        return_column <- "experimentID.data"
    } else {
        match_column <- "experimentID.data"
        return_column <- "experimentID.paper"
    }
    
    # Augment data frame for missing labels
    v_missing_labels <- experimentID[!experimentID %in% dat.moral.numbers.exp.correspondance[[match_column]]]
    
    if (length(v_missing_labels) > 0) {
        if (direction == "paper2data") {
            dat.correspondance.missing <- data.frame(
                experimentID.paper = v_missing_labels,
                experimentID.data = paste0(v_missing_labels, " (unchanged)"),
                stringsAsFactors = FALSE
            )
        } else {
            dat.correspondance.missing <- data.frame(
                experimentID.data = v_missing_labels,
                experimentID.paper = paste0(v_missing_labels, " (unchanged)"),
                stringsAsFactors = FALSE
            )
        }
        
        dat.correpondence <- dplyr::bind_rows(
            dat.moral.numbers.exp.correspondance,
            dat.correspondance.missing
        )
        
        warning("Labels ", paste0(v_missing_labels, collapse = "; "), " not found, keeping original labels.")
    } else {
        dat.correpondence <- dat.moral.numbers.exp.correspondance
    }
    
    
    purrr::map_chr(
        experimentID,
        ~ dat.correpondence %>%
            dplyr::filter(!!sym(match_column) == .x) %>%
            dplyr::pull(!!sym(return_column))
    )
}

#' Reformat experiment metadata columns for paper display.
#'
#' Accepts either a data frame or a character vector of internal experimentIDs.
#' @param exp_id_col Bare column name holding internal experiment IDs (default: `experimentID`).
#' - Always extracts any `.vivacityManipulationWorking` filtering suffix into a
#'   separate `filtering` column.
#' - Always formats `exp_id_col` via `get.original.experimentID()` + `str_replace("^exp", "Exp. ")`.
#' - Always normalises `comparisonType` (if present): underscores -> spaces,
#'   n.victims -> harm, n.net.saved -> utility.
#' - Optionally merges `filtering` and/or `comparisonType` back into
#'   `exp_id_col` via `str_c`. `wrap` controls the separator
#'   (" " or "\\n") used for all merge operations.
#'
#' When called with a character vector, merge flags are forced to FALSE and
#' a character vector is returned.
#' Refactored by Claude Code
format_exp_info <- function(dat = .,
                            exp_id_col = experimentID,
                            merge_filtering = TRUE,
                            merge_comparisonType = TRUE,
                            wrap = FALSE) {

    exp_id_col  <- rlang::enquo(exp_id_col)
    exp_id_name <- rlang::as_name(exp_id_col)

    # Helper functions

    # Convert internal experimentID string to paper-format (e.g. "exp8" -> "Exp. 4")
    format_experimentID_string <- function(x) {
        x %>%
            get.original.experimentID(direction = "data2paper") %>%
            stringr::str_replace("^exp", "Exp. ")
    }

    # Deal with comparison types in experiment 2 (ratios vs. harm or utility)
    normalise_comparisonType <- function(dat) {
        dat %>%
            dplyr::mutate(
                comparisonType = stringr::str_replace_all(comparisonType, "_", " ") %>%
                    stringr::str_replace_all("n\\.net\\.saved", "utility") %>%
                    stringr::str_replace_all("n\\.victims", "harm") %>%
                    stringr::str_replace_all("\\bvictims\\b", "harm") %>%
                    stringr::str_replace_all("\\bsaved\\b", "beneficiaries")
            )
    }

    # Extract .vivacityManipulationWorking suffix into a separate filtering column
    # Works even if the suffix is absent (filtering becomes NA)
    extract_filtering <- function(dat) {
        dat %>%
            tidyr::separate_wider_delim(
                !!exp_id_col,
                names = c(exp_id_name, "filtering"),
                delim = ".",
                too_few = "align_start"
            ) %>%
            dplyr::mutate(
                filtering = stringr::str_replace_all(filtering, "vivacityManipulationWorking", "(filtered)")
            )
    }

    # Allow character vector as input; return value is a character vector as well
    is_vector <- is.character(dat)
    if (is_vector) {
        dat <- tibble::tibble(!!exp_id_col := dat)
    }

    # Set separator for output
    sep <- if (wrap) "\n" else " "

    # Format experimentID string
    dat <- dat %>%
        extract_filtering() %>%
        dplyr::mutate(!!exp_id_col := format_experimentID_string(!!exp_id_col))

    # Normalise comparisonType if present
    has_comparisonType <- "comparisonType" %in% names(dat) && !all(is.na(dat$comparisonType))
    if (has_comparisonType) {
        dat <- dat %>% normalise_comparisonType()
    }

    # Optionally merge filtering back into experimentID
    if (merge_filtering) {
        dat <- dat %>%
            tidyr::unite(!!exp_id_col, !!exp_id_col, filtering, sep = sep, na.rm = TRUE)
    }

    # Optionally merge comparisonType into experimentID
    if (has_comparisonType && merge_comparisonType) {
        dat <- dat %>%
            dplyr::mutate(
                !!exp_id_col := dplyr::if_else(
                    !is.na(comparisonType),
                    stringr::str_c(!!exp_id_col, sep, "(", comparisonType, ")"),
                    !!exp_id_col
                )
            ) %>%
            dplyr::select(-comparisonType)
    }

    if (is_vector) return(dplyr::pull(dat, !!exp_id_col))
    dat
}


filter_vivacity_exps <- function(dat = ., l.exp = l_exp_with_single_severity_question, excluded.exps = c("exp9a"), match.type = c("start", "exact"), return.list = FALSE){
    
    match.type <- match.arg(match.type)
    
    excluded.exps <- as.character(excluded.exps)
    
    if (!is.null(excluded.exps)){
        l.exp <- setdiff(l.exp, excluded.exps)
    }
    
    if (is.null(l.exp)) return(dat)
    
    if(return.list) return(l.exp)
    
    if(match.type == "exact"){
        dat <- dat %>% 
            dplyr::filter(experimentID %in% l.exp) 
    } else {
        dat <- dat %>% 
            dplyr::filter(str_detect(experimentID, 
                                     str_c("^", l.exp, collapse="|")))
    }
    
    return(dat)
}


summarize.ratings <- function(dat = ., ...) {
    dat %>%
        dplyr::summarize(
            N = dplyr::n(),
            rating.raw.M = mean(rating.raw),
            rating.raw.SE = se(rating.raw),
            rating.raw.p = wilcox.p(rating.raw, mu = 3.5),
            rating.raw.d = (mean(rating.raw) - 3.5) / sd(rating.raw),
            rating.bin.M = mean(rating.bin),
            rating.bin.SE = se(rating.bin),
            rating.bin.p = binom.test(sum(rating.bin), length(rating.bin))$p.value,
            ...
        )
}
