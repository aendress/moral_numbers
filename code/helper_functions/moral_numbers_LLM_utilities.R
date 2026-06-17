mfv <- function(x = ., return_number = TRUE){
    
    # Get the most frequent value (as a character)
    val <- names(sort(table(x), decreasing = TRUE))[1]
    
    if(isTRUE(return_number)){
        # Try to convert to numeric if it looks numeric
        val_num <- suppressWarnings(as.numeric(val))
        
        # Return numeric if conversion succeeded
        if (!is.na(val_num)) {
            val <- val_num
        }
    }  
    
    return(val)
}

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
    all_levels <- setdiff(unname(LLM_LABELS), "Humans") %>% sort()
    
    # Put "Humans" first, then alphabetically sorted others
    level_order <- c("Humans", all_levels)
    
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
