get_model_url <- function(model = .){
    
    model <- str_to_lower(model)
    
    if(str_detect(model, "gpt")) return(LLM_URLS["gpt"] %>% unname)
    
    if(str_detect(model, "claude")) return(LLM_URLS["claude"] %>% unname)
    
    # We have different versions of gemini with different urls    
    if(str_detect(model, "gemini")) return(LLM_URLS[model] %>% unname)
    
    if(str_detect(model, "mistral")) return(LLM_URLS["mistral"] %>% unname)
    
    if(str_detect(model, "deepseek")) return(LLM_URLS["deepseek"] %>% unname)
    
    if(str_detect(model, "qwen")) return(LLM_URLS["qwen-plus"] %>% unname)
    
    return(LLM_URLS["ollama"] %>% unname)
}

make_llm_header <- function(model) {
    
    model_lower <- str_to_lower(model)
    
    # Detect model provider and return appropriate headers
    if (str_detect(model_lower, "gpt")) {
        return(c(Authorization = paste("Bearer", OPENAI_API_KEY)))
        
    } else if (str_detect(model_lower, "claude")) {
        return(c(
            `x-api-key` = ANTHROPIC_API_KEY, 
            `anthropic-version` = "2023-06-01"
        ))
        
    } else if (str_detect(model_lower, "gemini")) {
        return(c(`x-goog-api-key` = GEMINI_API_KEY))
        
    } else if (str_detect(model_lower, "mistral")) {
        return(c(Authorization = paste("Bearer", MISTRAL_API_KEY)))
        
    } else if (str_detect(model_lower, "deepseek")) {
        return(c(Authorization = paste("Bearer", DEEPSEEK_API_KEY)))
        
    } else if (str_detect(model_lower, "qwen")) {
        return(c(Authorization = paste("Bearer", QWEN_API_KEY)))
        
    } else {
        return(NULL)
    }
}

make_llm_body <- function(model, messages, system_prompt = NULL, system_instruction = NULL) {
    model_lower <- str_to_lower(model)
    
    # Build body depending on model
    if (str_detect(model_lower, "gpt-4")) {
        return(list(
            model = model,
            messages = messages,
            max_tokens = LLM_OPTIONS$max_output_tokens,        # max tokens to generate in response
            temperature = LLM_OPTIONS$temperature,        # controls randomness (0 = deterministic)
            #top_p = LLM_OPTIONS$top_p,              # nucleus sampling (1 = no restriction)
            n = LLM_OPTIONS$n                  # number of completions to generate
            # top_k is not a direct parameter in OpenAI API (used internally)
            #frequency_penalty = 0,  # optional, penalizes frequent tokens
            #presence_penalty = 0    # optional, penalizes new topic introduction
        ))
        
    } else if (str_detect(model_lower, "gpt-5")) {
        return(list(
            model = model,
            messages = messages,
            # reasoning effort: minimal|low|medium|high
            # default: medium
            reasoning_effort = "medium",
            
            #max_completion_tokens = LLM_OPTIONS$max_output_tokens,        # max tokens to generate in response
            # Not supported
            #temperature = LLM_OPTIONS$temperature,        # controls randomness (0 = deterministic)
            
            #top_p = LLM_OPTIONS$top_p,              # nucleus sampling (1 = no restriction)
            n = LLM_OPTIONS$n                  # number of completions to generate
            # top_k is not a direct parameter in OpenAI API (used internally)
            #frequency_penalty = 0,  # optional, penalizes frequent tokens
            #presence_penalty = 0    # optional, penalizes new topic introduction
        ))
        
    } else if (str_detect(model_lower, "claude")) {
        return(list(
            model = model,
            system = system_prompt,
            messages = messages,
            max_tokens = LLM_OPTIONS$max_output_tokens,        # max tokens to generate in response
            temperature = LLM_OPTIONS$temperature        # controls randomness (0 = deterministic)
            # can't set top_p togther with temperature
            #top_p = LLM_OPTIONS$top_p              # nucleus sampling (1 = no restriction)
            #top_k = LLM_OPTIONS$top_k, # claude supports top_k
        ))
        
    } else if (str_detect(model_lower, "gemini")) {
        return(list(
            #model = model, # spefified in url
            contents = messages, 
            systemInstruction = system_instruction,
            generationConfig = list(
                temperature = LLM_OPTIONS$temperature,
                topP = LLM_OPTIONS$top_p #,
                #topK = LLM_OPTIONS$top_k,
                #maxOutputTokens = LLM_OPTIONS$max_output_tokens
            )
        ))
        
    } else if (str_detect(model_lower, "mistral")) {
        #https://docs.mistral.ai/api/
        return(list(
            model = model,    
            messages = messages,
            temperature = LLM_OPTIONS$temperature,
            # Mistral recommends not setting top_p and temperature
            #top_p = LLM_OPTIONS$top_p,
            max_tokens = LLM_OPTIONS$max_output_tokens,
            #presence_penalty = 0,
            #frequency_penalty = 0,
            n = LLM_OPTIONS$n,
            stream = FALSE
        ))
        
    } else if (str_detect(model_lower, "deepseek")) {
        # https://api-docs.deepseek.com/
        return(list(
            model = model,
            messages = messages,
            temperature = LLM_OPTIONS$temperature,
            # Don't set together with temperature 
            #topP = LLM_OPTIONS$top_p,
            #frequencyPenalty = 0, # Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.
            #presencePenalty = 0, # Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.
            maxTokens = LLM_OPTIONS$max_output_tokens,
            stream = FALSE
            # stream = TRUE,
            # streamOptions = list(
            #     includeUsage = TRUE,
            #     continuousUsageStats = TRUE
            # )
        ))
        
    } else if (str_detect(model_lower, "qwen")) {
        # https://qwen.ai/apiplatform
        return(list(
            model = model,
            messages = messages,
            temperature = LLM_OPTIONS$temperature,
            # Don't set together with temperature
            #top_p = LLM_OPTIONS$top_p,
            top_k = LLM_OPTIONS$top_k,
            #frequency_penalty = 0, # Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.
            #presence_penalty = 0, # Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.
            max_tokens = LLM_OPTIONS$max_output_tokens,
            n = LLM_OPTIONS$n,
            stream = FALSE
            # stream = TRUE,
            # streamOptions = list(
            #     includeUsage = TRUE,
            #     continuousUsageStats = TRUE
            # )
        ))
        
    } else {
        # Ollama or default
        return(list(
            model = model,
            messages = messages,
            options = list(
                temperature = LLM_OPTIONS$temperature,
                # top_k = LLM_OPTIONS$top_k,
                # top_p = LLM_OPTIONS$top_p,
                num_ctx = LLM_OPTIONS$num_ctx  # 8k context window
            ),
            stream = FALSE
        ))
    }
}

parse_llm_response <- function(model, response) {
    model_lower <- str_to_lower(model)
    # Parse response depending on model
    if (str_detect(model_lower, "gpt")) {
        result <- content(response)$choices %>% 
            purrr::map_chr(purrr::pluck, "message", "content")
        
    } else if (str_detect(model_lower, "claude")) {
        result <- purrr::pluck(content(response, as = "parsed"), "content", 1, "text")
        
    } else if (str_detect(model_lower, "gemini")) {
        result <- purrr::pluck(content(response, as = "parsed"), "candidates", 1, "content", "parts", 1, "text")
        
    } else if (str_detect(model_lower, "mistral|deepseek|qwen")) {
        result <- purrr::pluck(content(response, as = "parsed"), "choices", 1, "message", "content") %>% 
            str_trim()
        
    } else {
        result <- purrr::pluck(content(response, as = "parsed"), "message", "content")
    }
    
    return(result)
}

send_llm_request <- function(model, url, messages, system_prompt = SYSTEM_PROMPTS$estimation, system_instruction = SYSTEM_INSTRUCTIONS$estimation, DEBUG = FALSE, JUST_COUNT_CHARACTERS = FALSE){
    
    model_lower <- str_to_lower(model)
    
    if(isTRUE(JUST_COUNT_CHARACTERS)){
        
        if(str_detect(model, "gemini")){
            stop("We cannot do the character count for gemini messages.")
        }
        n_characters <- return(purrr::map_chr(messages,
                                              ~ pluck(.x, "content")) %>%
                                   str_length() %>%
                                   sum %>%
                                   as.character())
        
    }
    
    # Build headers depending on model (API)
    headers <- make_llm_header(model_lower)
    
    # Build body depending on model
    body_json <- make_llm_body(model_lower, messages, system_prompt, system_instruction)
    
    if(isTRUE(DEBUG)){
        cat("\n=== API Request Debug ===\n")
        print(body_json)
        cat("==========================\n")
    }
    
    # Now make the request
    attempt <- 1
    repeat{
        response <- httr::RETRY(
            verb = "POST",
            url = url,
            httr::add_headers(.headers = headers),
            content_type_json(),
            encode = "json",
            body = toJSON(body_json, auto_unbox = TRUE),
            times = LLM_OPTIONS$max_attempts,                  # retry up to 5 times
            pause_base = 30,
            pause_min = 10,               # minimum wait between retries (seconds)
            pause_cap = 60,              # maximum wait
            terminate_on = c(400, 401)   # don't retry on these status codes
        )
        
        status <- httr::status_code(response)
        
        # If 429, handle retry with backoff
        if (status == 429) {
            retry_after <- httr::headers(response)[["retry-after"]]
            wait_time <- if (!is.null(retry_after)) {
                as.numeric(retry_after)
            } else {
                #min(2^(attempt - 1) * 5, 60)
                30
            }
            message(glue::glue("Rate limited (429). Sleeping for {wait_time} seconds (attempt {attempt}/{LLM_OPTIONS$max_attempts})..."))
            Sys.sleep(wait_time)
            attempt <- attempt + 1
            if (attempt > LLM_OPTIONS$max_attempts) {
                stop("Max retry attempts reached due to rate limiting (429). Aborting.")
            }
            next  # retry loop
        }
        
        # For permanent errors, stop immediately
        if (status %in% c(400, 401)) {
            
            print(glue::glue("API returned status {status}, aborting."))
            print("\n Full response \n")
            #print(response)
            print(content(response, as = "text"))
            stop("Exiting")
        }
        
        # Break on success or other statuses
        break
    }
    
    
    if(isTRUE(DEBUG)){
        # 🔍 DEBUGGING PRINT
        cat("\n=== API Response Debug ===\n")
        cat("\n** Messages **\n")
        print(messages)
        cat("\n** Response **\n")
        print(response)
        cat("\nStatus Code:", status_code(response), "\n")
        cat("\nRaw Content:\n")
        print(content(response, as = "text"))
        cat("\nSystem Prompt:\n")
        print(system_prompt)
        cat("\nSystem instruction:\n")
        print(system_instruction)
        
        cat("==========================\n")
    }
    
    
    result <- parse_llm_response(model_lower, response)
    
    result
    
}


compose_llm_messages <- function(dat = ., model){
    # dat is a data frane with columns role and message
    
    compose_llm_message_inner <- function(role, message, model){
        
        l_roles <- list(
            gemini = c(assistant = "model",
                       user = "user"),
            default = c(assistant = "assistant",
                        user = "user",
                        system = "system")
        )
        
        # In principle, the format is 
        # list(
        #     list(role = l_roles[["default"]][role] %>% unname,
        #          content = message
        #     )
        # )
        # However, as we call the function from within pmap, the outer list is already provided. 
        if(str_detect(model, "gemini")){
            #list( 
            list(role = l_roles[["gemini"]][role] %>% unname,
                 parts = list(list(text = message))
            )
            #)
        } else {
            #list(
            list(role = l_roles[["default"]][role] %>% unname,
                 content = message
            )
            #)
        }
        
    }
    
    dat %>% 
        purrr::pmap(~ compose_llm_message_inner(..1, ..2, model = model))
}

get_api_response_incremental <- function(dat = ., model, system_prompt, system_instruction, DEBUG = FALSE, JUST_COUNT_CHARACTERS = FALSE){
    
    
    url <- get_model_url(model)
    
    
    # Get first response. This depends on the model
    i <- 1
    if(str_detect(str_to_lower(model), "gpt")){
        # Don't override openai system prompt
        messages <- tibble::tribble(
            ~role, ~message,
            "user", glue::glue("{system_prompt} 
                               
                               {dat$prompt[i]}")
        ) %>% 
            compose_llm_messages(model = model)
        
    } else if(str_detect(str_to_lower(model), "gemini|claude")){
        
        # System prompt is included in API call
        
        messages <- tibble::tribble(
            ~role, ~message,
            "user", dat$prompt[i]
        ) %>% 
            compose_llm_messages(model = model)
        
    } else {
        messages <- tibble::tribble(
            ~role, ~message,
            "system", system_prompt,
            "user", dat$prompt[i]
        ) %>% 
            compose_llm_messages(model = model)
    } 
    
    if(nrow(dat) > 1){
        messages <- c(messages,
                      tibble::tribble(
                          ~role, ~message,
                          "assistant", dat$response[i]
                      ) %>% 
                          compose_llm_messages(model = model)
        )
    }
    
    if(nrow(dat) > 2){
        # Get subsequent responses except for the very last one
        messages <- c(messages,
                      tibble::tibble(
                          role = rep(c("user", "assistant"), times = nrow(dat) - 2),
                          message = list(dat$prompt[2:(nrow(dat) - 1)], 
                                         dat$response[2:(nrow(dat) - 1)]) %>% 
                              purrr::pmap(~ c(..1, ..2)) %>% 
                              unlist(use.names = FALSE)
                      ) %>% 
                          compose_llm_messages(model = model)
        )
    }
    
    
    # Last row for which we don't have a response yet
    if(nrow(dat) > 1){
        i <- nrow(dat)
        messages <- c(messages,
                      tibble::tribble(
                          ~role, ~message,
                          "user", dat$prompt[i]
                      ) %>% 
                          compose_llm_messages(model = model)
        )
    }
    
    # Send off the response
    result <- send_llm_request(model, url, messages, system_prompt = system_prompt, system_instruction = system_instruction, DEBUG = DEBUG, JUST_COUNT_CHARACTERS = JUST_COUNT_CHARACTERS) %>% 
        str_replace("^(\\d+)(\\D*)", "\\1")
    
    result
    
}



get_api_response_wrapper_df <- function(dat = ., dat_grp, verbose = FALSE, DEBUG = FALSE, JUST_COUNT_CHARACTERS = FALSE, ...){
    
    gc()
    
    
    model <- dat_grp$model %>% unique
    if(length(model) > 1) stop("More than one model detected")
    
    subj <- dat_grp$subj %>% unique
    if(length(subj) > 1) stop("More than one subject detected")
    
    experimentID <- dat_grp$experimentID %>% unique
    if(length(experimentID) > 1) stop("More than one experiment detected")
    
    if(str_detect(experimentID, "exp2")){
        system_prompt <- SYSTEM_PROMPTS$comparison
        system_instruction <- SYSTEM_INSTRUCTIONS$comparison
    } else {
        system_prompt <- SYSTEM_PROMPTS$estimation
        system_instruction <- SYSTEM_INSTRUCTIONS$estimation
    }
    
    if(isTRUE(verbose)) 
        print(glue::glue("Running model {model} for experiment {experimentID} and subject {subj}."))
    
    
    # Prepare return df
    dat <- dat %>% 
        dplyr::mutate(response = NA_character_)
    
    for(i in seq_len(nrow(dat))){
        dat[i, "response"] <- tryCatch({
            res <- get_api_response_incremental(
                dat[seq_len(i),],
                model = model,
                system_prompt = system_prompt,
                system_instruction = system_instruction,
                DEBUG = DEBUG,
                JUST_COUNT_CHARACTERS = JUST_COUNT_CHARACTERS
            )
            if (length(res) == 0) NA_character_ else res
        },
        
        error = function(e) NA_character_
        )  
        
    }
    
    
    
    return(dat)
}

make_prompt <- function(text, question, n_saved, n_victims, n_saved2, n_victims2, option1, option2){
    
    
    prompt <- dplyr::if_else(is.na(n_saved2),
                             glue::glue("{text} {question}"),
                             glue::glue("{text} {question} (1) {option1} (2) {option2}")
    )
    
    prompt <- dplyr::if_else(is.na(n_saved2),
                             prompt %>%
                                 stringr::str_replace_all("n_victims", as.character(n_victims)) %>%
                                 stringr::str_replace_all("n_saved", as.character(n_saved)),
                             
                             prompt %>%
                                 stringr::str_replace_all("n_victims2", as.character(n_victims2)) %>%
                                 stringr::str_replace_all("n_saved2", as.character(n_saved2)) %>%
                                 stringr::str_replace_all("n_victims1", as.character(n_victims)) %>%
                                 stringr::str_replace_all("n_saved1", as.character(n_saved))
    )
    
    prompt <- prompt %>% stringr::str_squish()
    
    
    prompt
    
}
