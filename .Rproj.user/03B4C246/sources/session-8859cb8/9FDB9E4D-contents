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
         "geometric" = sign(prod(estimates)) * exp(mean(log(abs(estimates)))))
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


############# WORKING IN PROGRESS ####################

#' Simulate Group Estimation Process
#' 
#' @description Simulates estimation process for group of individuals
#' @param n_subjects Integer. Number of subjects in group (default 10)
#' @param pk_range Numeric vector of length 2. Range for prior knowledge sampling (default c(0.2, 0.8))
#' @param seed Integer. Random seed for reproducibility
#' @return df containing:
#'         - subject_id: Subject identifier
#'         - prior_knowledge: Individual prior knowledge values
#'         - fe: First estimates
#'         - conf_base: Base confidence
#'         - conf: Final confidence after alpha modification
#'         - se: Second estimates
simulate_group <- function(n_subjects = 10, 
                           pk_range = c(0.2, 0.8), 
                           seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Create base data frame for all subjects
  df <- data.frame(
    subject_id = 1:n_subjects,
    prior_knowledge = runif(n_subjects, min = pk_range[1], max = pk_range[2])
  )
  
  # Simulate first estimates
  df$fe <- sapply(df$prior_knowledge, calculate_first_estimate)
  
  # Calculate base confidence
  df$conf_base <- sapply(df$prior_knowledge, calculate_base_confidence)
  
  # Simulate estimation process sequentially
  for(i in 1:n_subjects) {
    if(i == 1) {
      si <- 0  # First VP gets true value as social information
    } else {
      # Use first estimates for social information
      si <- aggregate_estimates(df$fe[1:(i-1)], method = "mean")
    }
    
    # Calculate deviation
    dev <- calculate_deviation(df$fe[i], si)
    
    # Calculate alpha and confidence
    alpha <- calculate_alpha(dev)
    df$conf[i] <- calculate_confidence(alpha, df$conf_base[i])
    
    # Calculate social weight and second estimate
    sw <- calculate_social_weight(df$conf[i])
    df$se[i] <- calculate_second_estimate(df$fe[i], si, sw)
  }
  
  # Aggregate
  group_se <- aggregate_estimates(df$se, method = "mean")
  df$group_se <- group_se  # Add group estimate to dataframe
  
  return(df)
}

simulate_woc_experiment <- function(
    n_sims = 10,        
    n_subjects = 10,
    pk_range = c(0.0001, 0.001)
) {
  crowd_sq_errors <- numeric(n_sims)
  individual_sq_errors <- numeric(n_sims)
  
  for (i in seq_len(n_sims)) {

    sim_data <- simulate_group(n_subjects, pk_range = pk_range, )
    
    crowd_est <- aggregate_estimates(sim_data$se, method = "mean")
    
    # squared errors
    crowd_sq_errors[i] <- crowd_est^2
    individual_sq_errors[i] <- mean(sim_data$se^2)  
  }
  
  # mean squared errors
  mse_crowd <- mean(crowd_sq_errors)
  mse_indiv <- mean(individual_sq_errors)
  
  # RMSE
  rmse_crowd <- sqrt(mse_crowd)
  rmse_indiv <- sqrt(mse_indiv)
  
  list(
    MSE_crowd = mse_crowd,
    MSE_indiv = mse_indiv,
    RMSE_crowd = rmse_crowd,
    RMSE_indiv = rmse_indiv
  )
}

set.seed(123)
res <- simulate_woc_experiment(n_sims = 10, n_subjects = 10)
res



