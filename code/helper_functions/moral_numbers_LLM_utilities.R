classify_models <- function(dat = .) {
    
    dat %>% 
        dplyr::mutate(model_type = 
                          dplyr::case_when(
                              model %in% (dplyr::filter(LLM_LABELS_TIBBLE, model_type == "cloud") %>% pull(model)) ~ "cloud",
                              model %in% (dplyr::filter(LLM_LABELS_TIBBLE, model_type == "local") %>% pull(model)) ~ "local",
                              TRUE ~ NA_character_
                          ),
                      .before = 1)
}


relabel_models <- function(model) {
    # Apply the label mapping
    labeled <- LLM_LABELS[model]
    
    # Get all possible levels (excluding "Humans")
    all_levels <- setdiff(unname(LLM_LABELS), "Humans")

    # Models alphabetically; within a model, reasoning efforts from none to the highest
    # (labels end in "(<effort> reasoning)", or "(reasoning)" when reasoning is simply switched on)
    base_labels <- stringr::str_remove(all_levels, " \\([a-z]* ?reasoning\\)$")
    efforts <- stringr::str_match(all_levels, "\\(([a-z]*) ?reasoning\\)$")[, 2]
    effort_ranks <- match(efforts, c("no", "minimal", "low", "medium", "high", ""))

    # Put "Humans" first, then the sorted others
    level_order <- c("Humans", all_levels[order(base_labels, effort_ranks)])
    
    # Return as factor with specified levels
    factor(labeled, levels = level_order)
}


relabel_conds_llm <- function(cond) {
    
    cond <- str_to_title(as.character(cond))
    
    cond <- factor(cond, 
                   levels = c("Moral", "Economic",
                              "Neutral", "Vivid"))
    
    cond
    
}

relabel_model_type <- function(x = ., model_type_col = model_type){
    
    model_type_col <- enquo(model_type_col)
    
    if(is.data.frame(x)){
        dat <- x
    } else {
        dat <- dplyr::tibble(!!quo_name(model_type_col) := x)
    }
    
    dat <- dat %>% 
        dplyr::mutate(
            !!model_type_col := dplyr::case_when(
                is.na(!!model_type_col) ~ "Humans",
                !!model_type_col == "cloud" ~ "Frontier models",
                !!model_type_col == "local" ~ "Local models",
                TRUE ~ !!model_type_col
            ),
            !!model_type_col := factor(!!model_type_col ,
                                       levels = c("Humans", "Frontier models", "Local models"))
        )
    
    if(is.data.frame(x)){
        
        dat
        
    } else {
        
        dplyr::pull(dat, !!model_type_col)
        
    }
    
}
