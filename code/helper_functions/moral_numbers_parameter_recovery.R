rnorm_mixture <- function(n, mu = NULL, sigma = NULL, dat_mu_sigma = NULL, w_prob = rep(1 / length(mu), length(mu))) {
    if (is.null(mu) & is.null(dat_mu_sigma)) stop("one of mu or dat_mu_sigma must be provided")

    # Input checks
    if (!is.null(mu)) {
        if (length(mu) != length(sigma)) stop("mu and sigma need to have the same length")
        if (length(mu) != length(w_prob)) stop("Length of w_prob must match mu and sigma")
    }

    if (!is.null(dat_mu_sigma)) {
        mu <- dat_mu_sigma %>% dplyr::pull(1)
        sigma <- dat_mu_sigma %>% dplyr::pull(2)
    }

    # Sample component counts (i.e., experiments)
    dat_components <- data.frame(
        component = sample(1:length(mu), size = n, replace = TRUE, prob = w_prob)
    ) %>%
        janitor::tabyl(component)

    # Generate and combine samples per component
    purrr::map2(
        dat_components$component, dat_components$n,
        ~ rnorm(.y, mean = mu[.x], sd = sigma[.x])
    ) %>%
        unlist() %>%
        sample() # Shuffle the result
}

fit_with_nlsLM <- function(formula, data, start, lower, upper, control = NULL) {
    tryCatch(
        minpack.lm::nlsLM(
            formula = formula,
            data = data,
            start = start,
            lower = lower,
            upper = upper,
            control = control
        ) %>%
            broom::tidy(),
        error = function(e) {
            message("nlsLM failed: ", e$message)
            data.frame(estimate = NA)
        }
    )
}
