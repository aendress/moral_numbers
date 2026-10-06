DOES NOT WORK. ESTIMATES ARE ALL OVER THE PLACE

library(brms)
#library(tidyverse)

## Existing fits
# 
# dat.moral.numbers.fit.by.subj.1param <- dat.moral.numbers.for.bootstrap.fit %>% 
#     dplyr::group_by(experimentID, blocks, ResponseId, decisionType, vivacity) %>% 
#     tidyr::nest() %>% 
#     dplyr::mutate(tmp.fit = map(data, ~ get.weber.ratio(.x, fit.a = FALSE, fit.w = TRUE))) %>% 
#     tidyr::unnest(tmp.fit) %>% 
#     dplyr::select(-data)
# 
# dat.moral.numbers.fit.by.subj.2param <- dat.moral.numbers.for.bootstrap.fit %>% 
#     dplyr::group_by(experimentID, blocks, ResponseId, decisionType, vivacity) %>% 
#     tidyr::nest() %>% 
#     dplyr::mutate(tmp.fit = map(data, ~ get.weber.ratio(.x, fit.a = TRUE, fit.w = TRUE))) %>% 
#     tidyr::unnest(tmp.fit) %>% 
#     dplyr::select(-data)


fit_hierarchical_moral_model <- function(dat = ., fit_a = TRUE, return_df = TRUE, n_iter = 2000){
    
    
    # Just in case
    dat <- dat %>% 
        dplyr::mutate(ResponseId = as.factor(ResponseId))
    
    
    # Set formula
    if(fit_a){
        
        model_formula <- brms::bf(
            rating.bin ~ 1 - log2(1 - Phi((ratio - alpha) /
                                              (w * sqrt(ratio^2 + alpha^2)))),
            w + alpha ~ 1 + (1 | ResponseId),
            nl = TRUE
        )
    } else {    
        
        model_formula <- brms::bf(
            rating.bin ~ 1 - log2(1 - Phi((ratio - 1) /
                                              (w * sqrt(ratio^2 + 1)))),
            w ~ 1 + (1 | ResponseId),
            nl = TRUE
        )
        
    }
    
    # Set priors
    if(fit_a){   

        model_priors <- c(
            brms::prior(normal(0.5, 0.3), class = "b", nlpar = "w", lb = 0, ub = 2),
            brms::prior(normal(1.5, 0.5), class = "b", nlpar = "alpha", lb = 1, ub = 5)
        )
        
    } else {
        model_priors <- c(
            brms::prior(normal(0.5, 0.3), class = "b", nlpar = "w", lb = 0, ub = 2)
            #brms::prior(exponential(1), class = "sd", group = "ResponseId", nlpar = "w")
        )
    }
    
    result <- brm(
        formula = model_formula,
        data = dat,
        family = bernoulli(),
        prior = model_priors,
        chains = 4,
        iter = n_iter,
        cores = 4,
        control = list(adapt_delta = 0.99)
    )
    
    
    if(return_df){
        
        result <- map_dfr(dimnames(coef(result)$ResponseId)[[3]],
                     ~ {
                         coef(result)$ResponseId[,,.x] %>% 
                             as.data.frame() %>% 
                             tibble::rownames_to_column("ResponseId") %>% 
                             mutate(parameter = .x, .before = 1) 
                     }
            ) %>%
            dplyr::mutate(parameter = str_remove(parameter, "_Intercept"))
    }
    
    result
}


# fit <- brm(
#     bf(
#         rating.bin ~ 1 - log2(1 - Phi((ratio - 1) / 
#                                         (w * sqrt(ratio^2 + 1)))),
#         w  ~ 1 + (1 | ResponseId),
#         nl = TRUE
#     ),
#     data = dat.moral.numbers.for.bootstrap.fit %>% 
#         dplyr::filter(experimentID == "exp1", blocks == "all"),
#     family = bernoulli(),
#     prior = c(
#         prior(normal(0.5, 0.3), nlpar = "w", lb = 0)#,
#       #  prior(normal(1.5, 0.5), nlpar = "alpha", lb = 1)
#     ),
#     chains = 4,
#     iter = 2000,
#     cores = 4,
#     control = list(adapt_delta = 0.99)
# )
# 
# 
# map_dfr( dimnames(coef(fit)$ResponseId)[[3]],
#          ~ {
#              coef(fit)$ResponseId[,,.x] %>% 
#                  as.data.frame() %>% 
#                  tibble::rownames_to_column("subject") %>% 
#                  mutate(parameter = .x, .before = 1) 
#          }
# )


xxx <- fit_hierarchical_moral_model(dat.moral.numbers.for.bootstrap.fit %>% 
                                        dplyr::filter(experimentID == "exp1", blocks == "all"),
                                    n_iter = 100,
                                    fit_a = FALSE)



yyy<- left_join(xxx,
          dat.moral.numbers.fit.by.subj.1param %>% 
              dplyr::filter(experimentID == "exp1", blocks == "all"),
          by = c("ResponseId")) %>% 
    dplyr::select(Estimate,w)

yyy %>% 
    drop_na() %>% 
    ggplot(aes(x = w, y = Estimate)) +
    geom_point() 

sd(yyy$Estimate, na.rm = T )
sd(yyy$w, na.rm = T )

range(xxx$Estimate)
cor.test(yyy$Estimate, yyy$w)

zzz <- left_join(dat.moral.numbers.for.bootstrap.fit %>% 
                     dplyr::filter(experimentID == "exp1", blocks == "all"),
                 xxx,
                 by = c("ResponseId"))

cor.test(zzz$rating.raw, zzz$Estimate)

dat.moral.numbers.fit.by.subj.1param.with.ratings   %>% 
    cor_test(rating.raw, w)

zzz %>% 
    cor_test(rating.raw, Estimate)
