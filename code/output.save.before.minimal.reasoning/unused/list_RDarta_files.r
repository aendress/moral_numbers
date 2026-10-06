library(tidyverse)

# 'tmp_stuff_for_llm.RData'
# Human results used for comparison

# unused:
# 'llm_test.RData'
#     'llm_test.ep10b.RData'

# overlapping data:
# 'llm_test.all_exps.gpt-4.1-mini.RData',
#  'llm_test.all_exps.cloud_models.RData',
# both contain only gpt-4.1.minia and identical experiments
# They are saved the same day, but the columns differ. Colums are identical in 
# * 'llm_test.all_exps.gemini-2.5-flash.RData' and 'llm_test.all_exps.gpt-4.1-mini.RData',
# * 'llm_test.all_exps.local_models.RData' and 'llm_test.all_exps.cloud_models.RData',
# I turns out that the data are identical, they are just reformatted for the global file

dat_rdata_content <- c(
    'llm_test.all_exps.cloud_models.RData',
    'llm_test.all_exps.gemini-2.5-flash.RData',
    'llm_test.all_exps.gpt-4.1-mini.RData',
    'llm_test.all_exps.local_models.RData'
) %>%
    purrr::map_dfr(function(file_name) {
        
        tmp_env <- new.env()
        load(file_name, envir = tmp_env)
        
        ls(envir = tmp_env) %>%
            tibble(object = .) %>%
            mutate(
                file = file_name,
                tmp = purrr::map(object, function(obj_name) {
                    obj <- get(obj_name, envir = tmp_env)
                    
                    if (is.data.frame(obj)) {
                        obj %>%
                            dplyr::ungroup() %>%
                            dplyr::count(model, experimentID, .drop = FALSE)
                    } else {
                        tibble(model = NA, experimentID = NA, n = NA)
                    }
                })
            ) %>%
            tidyr::unnest(tmp)
    }) %>%
    dplyr::relocate(model, experimentID, .before = 1) %>%
    dplyr::arrange(model, experimentID)

# Save tibble to CSV
readr::write_csv(dat_rdata_content, "dat_rdata_content.csv")


