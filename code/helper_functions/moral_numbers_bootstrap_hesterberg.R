
#' Compute Bootstrap t Confidence Interval
#'
#' Calculates a bootstrap t confidence interval for the mean parameter following
#' Hesterberg (2015). The interval is constructed using the percentile method
#' applied to the bootstrap t distribution.
#'
#' @param dat A data frame containing bootstrap sample results. Must include
#'   columns `theta_star_hat` (mean estimates from bootstrap samples) and
#'   `se_star_hat` (standard errors from bootstrap samples).
#' @param x_original A numeric vector containing the original sample data.
#' @param alpha Numeric value between 0 and 1 specifying the significance level.
#'   Default is 0.05 for a 95% confidence interval.
#'
#' @return A data frame with one row containing:
#'   \item{ti_lower}{Lower bound of the bootstrap t interval}
#'   \item{ti_upper}{Upper bound of the bootstrap t interval}
#'
#' @details
#' The bootstrap t interval is computed as:
#' \deqn{(\hat{\theta} - q_{1-\alpha/2}\hat{S}, \hat{\theta} - q_{\alpha/2}\hat{S})}
#' where \eqn{\hat{\theta}} and \eqn{\hat{S}} are the mean and standard error
#' from the original sample, and \eqn{q_x} are the quantiles of the bootstrap
#' t distribution defined by \eqn{t^* = (\hat{\theta}^* - \hat{\theta})/\hat{S}^*}.
#'
#' @references
#' Hesterberg, T. C. (2015). What teachers should know about the bootstrap:
#' Resampling in the undergraduate statistics curriculum. The American
#' Statistician, 69(4), 371-386.
#'
#' @examples
#' \dontrun{
#' # Generate bootstrap samples
#' original_data <- rnorm(100, mean = 5, sd = 2)
#' boot_samples <- data.frame(
#'   theta_star_hat = replicate(10000, mean(sample(original_data, replace = TRUE))),
#'   se_star_hat = replicate(10000, se(sample(original_data, replace = TRUE)))
#' )
#' 
#' # Compute bootstrap t interval
#' boot.ti(dat = boot_samples, x_original = original_data, alpha = 0.05)
#' }
#'
#' @export
boot.ti <- function(dat = ., x_original, alpha = 0.05) {
    
    # dat is a data frame. Each row is a bootstrap sample with the following columns
    # * theta_star_hat: mean parameter estimate in the bootstrap sample
    # * se_star_hat: standard error in the bootstrap sample
    # 
    # x is the vector of the original sample
    #
    # * refers to bootstrap sample
    
    # Input validation
    if (!is.data.frame(dat)) {
        stop("'dat' must be a data frame")
    }
    
    required_cols <- c("theta_star_hat", "se_star_hat")
    missing_cols <- setdiff(required_cols, names(dat))
    if (length(missing_cols) > 0) {
        stop(sprintf("'dat' must contain columns: %s. Missing: %s",
                     paste(required_cols, collapse = ", "),
                     paste(missing_cols, collapse = ", ")))
    }
    
    if (!is.numeric(x_original)) {
        stop("'x_original' must be a numeric vector")
    }
    
    if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
        stop("'alpha' must be a single numeric value between 0 and 1")
    }
    
    # Mean in the original sample
    theta_hat <- mean(x_original, na.rm = TRUE)
    # SE in the original sample
    se_hat <- se(x_original)
    
    dat %>% 
        # Compute t* in the bootstrap sample
        dplyr::mutate(
            t_star = (theta_star_hat - theta_hat) / se_star_hat
        ) %>% 
        # Now compute interval
        dplyr::summarize(
            ti_lower = theta_hat - quantile(t_star, 1 - alpha/2) * se_hat,
            ti_upper = theta_hat - quantile(t_star, alpha/2) * se_hat
        )
}

summarize_bootstrap_samples <- function(dat = .){
    
    
    dat_non_numeric_cols <- dat %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-dplyr::group_vars(dat)) %>% 
        dplyr::select(where(~ !is.numeric(.)))
    
    if (ncol(dat_non_numeric_cols) > 0) {
        stop(sprintf("All non-grouping columns must be numeric. Non-numeric columns: %s",
                     paste(names(dat_non_numeric_cols), collapse = ", ")))
    }    
    
    dat %>% 
        dplyr::summarize(
            across(everything(), 
                   list(
                       N = ~ sum(is.finite(.x)),
                       M = ~ mean(.x, na.rm = TRUE), 
                       SD = ~ sd(.x, na.rm = TRUE),
                       # Monte Carlo Error/ or Simulation Error
                       mc_error = ~ sd(.x, na.rm = TRUE) / sqrt(sum(is.finite(.x)) - 1),
                       # percentile interval
                       pi.lower = ~ quantile(.x, .025, na.rm = TRUE),
                       pi.upper = ~ quantile(.x, .975, na.rm = TRUE)
                   )
            ),
            .groups = "drop"
        )
    
}

