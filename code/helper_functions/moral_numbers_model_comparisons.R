
#' get.dv.from.model (mod = .)
#'
#' @Description
#' Extract the dependent variable from model `model`
#'
#' @param mod Fitted model of class lmerModLmerTest or glmerMod. Probably works for other classes as well
#'
#' @return Vector of the dependent variable
#'
#' @examples
#' # not run
#' # get.dv.from.model(myLmer)
get.dv.from.model <- function(mod = .){
    
    if(!(class(mod) %in% c("lmerModLmerTest", 
                           "glmerMod"))){
        stop("Model needs to be of classs lmerModLmerTest or glmerMod")
    }
    
    # From https://stackoverflow.com/questions/13217322/how-to-reliably-get-dependent-variable-name-from-formula-object
    dv <- all.vars(formula(mod)[[2]])
    
    getData(mod) %>% 
        dplyr::pull(dv)
}


# This is supposed to be a version of the j-test for GLMMs
# Don't use it; models are singular for some reason
j.test.glmer <- function (mod1, mod2){
    
    #mod1 <-  moral.numbers.ratio.vs.n.victims.bin.lmer1.ratio.only 
    #mod2 <- moral.numbers.ratio.vs.n.victims.bin.lmer1.n.only 
    
    if((class(mod1) != "glmerMod") || (class(mod2) != "glmerMod")){
        stop("Models need to be of classs lmerModLmerTest")
    }
    
    dat.mod1 <- getData(mod1)
    dat.mod2 <- getData(mod2)
    
    if(!identical(dat.mod1, dat.mod2)){
        
        stop("Models are not fitted to the same data.")
    }
    
    
    fit.mod1 <- fitted(mod1)
    fit.mod2 <- fitted(mod2)
    
    
    mod1.augmented <- stats::update(mod1, ~ . + fit.mod2)
    mod2.augmented <- stats::update(mod2, ~ . + fit.mod1)
    
    anova(mod1.augmented,
          mod1)
    
    
    anova(mod2.augmented,
          mod2)
    
}

generate_model_comp <- function(l_exps = ., rename_stuff_for_print = FALSE){
    
    
    dat_model_comp <- l_exps %>% 
        purrr::map(
            \(x) {
                tibble::tibble(
                    # 1. Calculate goodness of fit for target (i.e., one fixed factor) models
                    
                    # Calculate corrected AICs
                    AICc = MuMIn::AICc(x$mod.target),
                    # Calculate r.zheng.agreesti
                    r.zheng.agresti = r.zheng.agresti(x$mod.target),
                    
                    ####
                    # 2. Calculate model comparisons
                    model_comps = tibble::tibble(
                        added.predictor = x$added.predictor,
                        reduction.AICc = MuMIn::AICc(x$mod.full, 
                                                     x$mod.target) %>% 
                            dplyr::pull(AICc) %>% 
                            diff,
                        p.value = anova(x$mod.target,
                                        x$mod.full) %>% 
                            broom.mixed::tidy(.) %>% 
                            dplyr::filter(term == "x$mod.full") %>% 
                            dplyr::pull(p.value)
                    ) %>% 
                        tidyr::pivot_wider(
                            names_from = "added.predictor",
                            values_from = c("reduction.AICc", "p.value"),
                            names_vary = "slowest"
                        )
                ) %>% 
                    tidyr::unnest(model_comps)
            }
        ) %>% 
        purrr::list_rbind(names_to = "Predictor") %>% 
        # Join the two Ratio columns
        dplyr::group_by(Predictor) %>%
        dplyr::summarize(
            dplyr::across(
                dplyr::everything(), 
                ~ reduce(.x, coalesce)
            ), 
            .groups = "drop"
        )
    
    
    
    if(rename_stuff_for_print){    
        
        dat_model_comp <- dat_model_comp %>%
            dplyr::mutate(
                Predictor = dplyr::replace_values(
                    Predictor,
                    "Ratio" ~ "Ratio",
                    "n.net.saved" ~ "Utility",
                    "n.victims" ~ "Harm"
                ) 
            ) %>% 
            dplyr::rename_with(
                ~ stringr::str_replace_all(
                    .x,
                    c(
                        "n.net.saved" = "Utility",
                        "n.victims"   = "Harm",
                        "reduction."  = "$\\\\Delta$",
                        "p.value"     = "$p$"
                    )
                )
            )
    }
    
    dat_model_comp
}

