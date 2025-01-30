#' Calculate First Estimate
#' 
#' @description Calculates the first estimate as deviation from true value using a Cauchy distribution
#' @param pk Numeric vector. Prior knowledge values between 0 and 1
#' @return Numeric vector. First estimates as deviation from true value
calculate_first_estimate <- function(pk) {
  if (any(pk < 0) || any(pk > 1)) stop("Prior knowledge must be between 0 and 1")
  # Higher prior knowledge leads to smaller sigma (less spread)
  sigma <- 5.5 - 5 * pk  # sigma ranges from 0.5 (pk=1) to 5.5 (pk=0)
  rcauchy(length(pk), location = 0, scale = sigma)
}

#' Calculate Base Confidence
#' 
#' @description Calculates base confidence using a log-normal distribution
#' @param pk Numeric vector. Prior knowledge values between 0 and 1
#' @return Numeric vector. Base confidence values
calculate_base_confidence <- function(pk) {
  if (any(pk < 0) || any(pk > 1)) stop("Prior knowledge must be between 0 and 1")
  rlnorm(length(pk), meanlog = -1 + 2*pk, sdlog = 0.5)
}

#' Calculate Alpha Modification
#' 
#' @description Calculates alpha modification based on deviation from group estimate
#'              following specific anchor points for different deviation levels
#' @param D Numeric vector. Deviation values
#' @return Numeric vector. Alpha modification values between -0.5 and 1
calculate_alpha <- function(D) {
  D <- abs(D)
  # anchor points:
  # D = 0   -> alpha = 1    (no change to base confidence)
  # D = 0.5 -> alpha = 0.5  (moderate reduction)
  # D = 1   -> alpha = 1    (reaffirmation)
  # D = 1.5 -> alpha = 0    (complete uncertainty)
  # D = 2   -> alpha = -0.5 (overreacting)
  
  ifelse(D < 0.5, 
         1 - D,  # Linear decrease from 1 to 0.5
         ifelse(D < 1, 
                0.5 + (D - 0.5) * 1,  # Linear increase from 0.5 to 1
                ifelse(D < 1.5,
                       1 - 2 * (D - 1),  # Linear decrease from 1 to 0
                       ifelse(D < 2,
                              -2 * (D - 1.5),  # Linear decrease from 0 to -0.5
                              -0.5))))  # Constant -0.5 for D >= 2
}


#' Calculate Confidence
#' 
#' @description Calculates final confidence based on base confidence and alpha modification
#' @param alpha Numeric vector. Alpha modification values
#' @param conf_base Numeric vector. Base confidence values
#' @return Numeric vector. Final confidence values
calculate_confidence <- function(alpha, conf_base) {
  alpha * conf_base
}

#' Calculate Deviation
#' 
#' @description Calculates deviation between first estimate and social information
#' @param fe Numeric vector. First estimates
#' @param si Numeric vector. Social information
#' @return Numeric vector. Deviation values
calculate_deviation <- function(fe, si) {
  abs(si - fe) / fe
}

#' Calculate Social Weight
#' 
#' @description Calculates social weight based on confidence
#' @param conf Numeric vector. Confidence values
#' @return Numeric vector. Social weight values
calculate_social_weight <- function(conf) {
  1 - conf
}

#' Calculate Second Estimate
#' 
#' @description Calculates second estimate based on first estimate, social information, and social weight
#' @param fe Numeric vector. First estimates
#' @param si Numeric vector. Social information
#' @param sw Numeric vector. Social weights
#' @return Numeric vector. Second estimate values
calculate_second_estimate <- function(fe, si, sw) {
  (1 - sw) * fe + sw * si
}

#' Calculate Wisdom of Crowd
#' 
#' @description Determines if wisdom of crowd effect is present
#' @param agg_ge Numeric. Single aggregated group estimate
#' @param se_values Numeric vector. Individual second estimates
#' @return Integer. 1 if wisdom of crowd effect is present, 0 otherwise
calculate_woc <- function(agg_ge, se_values) {
  f <- function(x) x^2
  n <- length(se_values)
  
  agg_error <- f(agg_ge)
  individual_errors <- mean(sapply(se_values, f))
  
  as.integer(agg_error <= individual_errors)
}

#' Aggregate Estimates
#' 
#' @description Aggregates multiple estimates using specified method
#' @param estimates Numeric vector. Individual estimates to aggregate
#' @param method Character. "mean", "median", or "geometric"
#' @return Numeric. Single aggregated estimate
aggregate_estimates <- function(estimates, method = c("mean", "median", "geometric")) {
  method <- match.arg(method)
  
  switch(method,
         "mean" = mean(estimates),
         "median" = median(estimates),
         "geometric" = exp(mean(log(estimates))))
}


#' Simulate Complete Estimation Process
#' 
#' @description Simulates a complete estimation process for one subject, including
#'              first estimate, confidence calculations, and second estimate
#' @param pk Numeric. Prior knowledge value between 0 1
#' @param si Numeric. Social information
#' @return Named numeric vector containing:
#'         - fe: First estimate
#'         - conf_base: Base confidence
#'         - conf: Final confidence (after alpha modification)
#'         - se: Second estimate
simulate_estimation_process <- function(pk, si) {
  # Generate first estimate
  fe <- calculate_first_estimate(pk)
  
  # Calculate base confidence
  conf_base <- calculate_base_confidence(pk)
  
  # Calculate deviation and alpha
  dev <- calculate_deviation(fe, si)
  alpha <- calculate_alpha(dev)
  
  # Calculate final confidence
  conf <- calculate_confidence(alpha, conf_base)
  
  # Calculate social weight and second estimate
  sw <- calculate_social_weight(conf)
  se <- calculate_second_estimate(fe, si, sw)
  
  # Return results as named vector
  return(c(
    fe = fe,
    conf_base = conf_base,
    conf = conf,
    se = se
  ))
}

simulate_estimation_process(0.1, 1)

simulate_estimation_process(0.99, 1)
