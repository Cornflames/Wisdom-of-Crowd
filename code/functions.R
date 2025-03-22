# Set seed for reproducibility
set.seed(40)

# Distribution of Prior Knowledge
dPriorKnowledge <- function(n_individuals, distribution_type = "uniform", params = list()) {
  switch(distribution_type,
         "uniform" = {
           # Default uniform distribution between 0 and 1
           min_val <- if(is.null(params$min)) 0 else params$min
           max_val <- if(is.null(params$max)) 1 else params$max
           return(runif(n_individuals, min = min_val, max = max_val))
         },
         "normal" = {
           # Normal distribution with specified mean and sd
           mean_val <- if(is.null(params$mean)) 0.5 else params$mean
           sd_val <- if(is.null(params$sd)) 0.15 else params$sd
           # Clip values to be between 0 and 1
           knowledge <- pmin(pmax(rnorm(n_individuals, mean = mean_val, sd = sd_val), 0), 1)
           return(knowledge)
         },
         "truncnorm" = {
           # Truncated normal distribution between between 0 and 1
           # with specified mean and sd
           mean_val <- if(is.null(params$mean)) 0.5 else param$mean
           sd_val <- if(is.null(params$sd)) 0.15 else param$sd
           truncnorm::rtruncnorm(n_individuals, a = 0, b = 1, mean = mean_val, sd = sd_val)
         },
         "beta" = {
           # Beta distribution for more flexible shapes
           alpha <- if(is.null(params$alpha)) 2 else params$alpha
           beta <- if(is.null(params$beta)) 2 else params$beta
           return(rbeta(n_individuals, shape1 = alpha, shape2 = beta))
         },
         "bimodal" = {
           # Simple bimodal distribution (mix of two normals)
           prop1 <- if(is.null(params$prop1)) 0.5 else params$prop1
           mean1 <- if(is.null(params$mean1)) 0.25 else params$mean1
           mean2 <- if(is.null(params$mean2)) 0.75 else params$mean2
           sd1 <- if(is.null(params$sd1)) 0.1 else params$sd1
           sd2 <- if(is.null(params$sd2)) 0.1 else params$sd2
           
           n1 <- round(n_individuals * prop1)
           n2 <- n_individuals - n1
           
           group1 <- rnorm(n1, mean = mean1, sd = sd1)
           group2 <- rnorm(n2, mean = mean2, sd = sd2)
           
           # Clip values to be between 0 and 1
           knowledge <- pmin(pmax(c(group1, group2), 0), 1)
           return(knowledge)
         },
         # Default to uniform if distribution type not recognized
         runif(n_individuals, min = 0, max = 1)
  )
}

# Function to calculate confidence based on prior knowledge
calculateConfidence <- function(prior_knowledge) {
  # Simple linear relationship between knowledge and confidence
  return(prior_knowledge)
}

# Distribution of Individual First Estimates
dIndividualFirstEstimate <- function(prior_knowledge, true_value) {
  # The higher prior knowledge, the smaller the distribution of individual estimates
  # For maximum prior knowledge = 1, sd_of_log = 0.05
  # For minimum prior knowledge = 0, sd_of_log = 1
  sd_of_log = -0.95 * prior_knowledge + 1
  mean_of_log = log(true_value) - sd_of_log^2/2
 
  # Generate a sample from this distribution
  estimate <- rlnorm(1, meanlog = mean_of_log, sdlog = sd_of_log)
  
  return(estimate)
}

# Distribution of Group First Estimates
dGroupFirstEstimate <- function(prior_knowledge_vector, true_value) {
  # Generate first estimates for all individuals
  first_estimates <- sapply(prior_knowledge_vector, dIndividualFirstEstimate, true_value = true_value)
  return(first_estimates)
}

# Determine the weight a person puts on advice
getWeightOnAdvice <- function(confidence, first_estimate, social_information) {
  # Calculate log-based distance
  log_ratio <- log(social_information / first_estimate)
  distance <- abs(log_ratio)
  
  # tanh of large distances approaches 1
  weight_of_advice <- (1 - confidence) * (1 + confidence * tanh(distance))
  weight_of_advice <- max(0, min(1, weight_of_advice))

  return(weight_of_advice)
}


# Psi function for integrating social information
psi <- function(first_estimate, social_info, confidence) {
  # Weight on advice from confidence and the distance between first estimate and social info
  weight_on_advice <- getWeightOnAdvice(confidence, first_estimate, social_info)
  # Weight on the inividual first estimate
  self_weight <- 1 - weight_on_advice

  # The expected value for the second estimate based on weight on advice and self weight
  # Interpreted as a persons tendency to a second estimate value, it can be seen
  # as the central tendency of her second estimate distribution
  expected_second_estimate <- weight_on_advice * social_info + self_weight * first_estimate
  return(expected_second_estimate)
}

# Distribution of Individual Second Estimates
dIndividualSecondEstimate <- function(first_estimate, social_info, confidence) {
  # Returns the second estimate after applying the Psi function and random noise
  expected_second_estimate <- psi(first_estimate, social_info, confidence)
  second_estimate <- rnorm(1, mean = expected_second_estimate, sd = 1)
  return(second_estimate)
}

# Distribution of Group Second Estimates
dGroupSecondEstimates <- function(first_estimate_vector, social_info, confidence_vector) {
  # Generate second estimates for all individuals
  second_estimates <- mapply(dIndividualSecondEstimate,
                             first_estimate = first_estimate_vector,
                             confidence = confidence_vector,
                             MoreArgs = list(social_info = social_info))
  return(second_estimates)
}