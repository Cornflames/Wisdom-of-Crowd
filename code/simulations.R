# Set seed for reproducibility
set.seed(40)

# Function to determine WOC with social influence
determineWOCSocialInfluence <- function(first_estimate_vector, second_estimate_vector, true_value) {
  # Calculate mean estimates
  mean_first <- mean(first_estimate_vector)
  mean_second <- mean(second_estimate_vector)
  
  # Calculate individual means (average of individual estimates)
  mean_individual_first <- mean(first_estimate_vector)
  mean_individual_second <- mean(second_estimate_vector)
  
  # Calculate individual average distances (average error of individuals)
  mean_individual_distance_first <- mean(abs(first_estimate_vector - true_value))
  mean_individual_distance_second <- mean(abs(second_estimate_vector - true_value))
  
  # Calculate group distances (error of the aggregated estimate)
  group_distance_first <- abs(mean_first - true_value)
  group_distance_second <- abs(mean_second - true_value)
  
  # Calculate WOC benefit in absolute units (how many units closer the group is vs avg individual)
  woc_benefit_first <- mean_individual_distance_first - group_distance_first
  woc_benefit_second <- mean_individual_distance_second - group_distance_second
  
  # Change in WOC benefit
  woc_benefit_change <- woc_benefit_second - woc_benefit_first
  
  # Percentage WOC benefit (what % of individual error is eliminated by aggregation)
  woc_percent_benefit_first <- (woc_benefit_first / mean_individual_distance_first) * 100
  woc_percent_benefit_second <- (woc_benefit_second / mean_individual_distance_second) * 100
  woc_percent_benefit_change <- woc_percent_benefit_second - woc_percent_benefit_first
  
  # Also calculate the squared error metrics for completeness
  error_first <- (mean_first - true_value)^2
  error_second <- (mean_second - true_value)^2
  individual_error_first <- mean((first_estimate_vector - true_value)^2)
  individual_error_second <- mean((second_estimate_vector - true_value)^2)
  
  return(list(
    # Group estimates
    group_mean_first = mean_first,
    group_mean_second = mean_second,
    group_distance_first = group_distance_first,
    group_distance_second = group_distance_second,
    
    # Individual averages
    individual_mean_distance_first = mean_individual_distance_first,
    individual_mean_distance_second = mean_individual_distance_second,
    
    # WOC benefits in original units
    woc_benefit_first = woc_benefit_first,
    woc_benefit_second = woc_benefit_second,
    woc_benefit_change = woc_benefit_change,
    
    # WOC benefits as percentages
    woc_percent_benefit_first = woc_percent_benefit_first,
    woc_percent_benefit_second = woc_percent_benefit_second,
    woc_percent_benefit_change = woc_percent_benefit_change,
    
    # Original squared error metrics
    error_first = error_first,
    error_second = error_second,
    individual_error_first = individual_error_first,
    individual_error_second = individual_error_second,
    improvement = error_first - error_second,
    individual_improvement = individual_error_first - individual_error_second
  ))
}

# Function to run a simulation
runSimulation <- function(n_individuals, true_value, knowledge_distribution = "uniform", 
                          knowledge_params = list(), social_info_type = "mean", 
                          manipulated_social_info = NULL, n_trials = 1) {
  
  results <- list()
  
  for(i in 1:n_trials) {
    # Generate prior knowledge
    prior_knowledge <- dPriorKnowledge(n_individuals, knowledge_distribution, knowledge_params)
    
    # Calculate confidence
    confidence <- sapply(prior_knowledge, calculateConfidence)
    
    # Generate first estimates
    first_estimates <- dGroupFirstEstimate(prior_knowledge, true_value)
    
    # Calculate personalized social information and second estimates
    if(!is.null(manipulated_social_info)) {
      # If social_info is manipulated, use the same value for everyone
      social_info <- manipulated_social_info
      second_estimates <- dGroupSecondEstimates(first_estimates, social_info, confidence)
    } else {
      # Create vectors to store personalized social info and second estimates
      social_info_vector <- numeric(n_individuals)
      second_estimates <- numeric(n_individuals)
      
      # Calculate personalized social info for each individual (excluding their own estimate)
      for(j in 1:n_individuals) {
        # Get all estimates except this person's
        other_estimates <- first_estimates[-j]
        
        # Calculate social information based on specified type
        switch(social_info_type,
               "mean" = {
                 social_info_vector[j] <- mean(other_estimates)
               },
               "median" = {
                 social_info_vector[j] <- median(other_estimates)
               },
               "trimmed_mean" = {
                 # Remove the top and bottom 10%
                 social_info_vector[j] <- mean(other_estimates, trim = 0.1)
               },
               # Default to mean
               {
                 social_info_vector[j] <- mean(other_estimates)
               }
        )
        
        # Calculate second estimate for this person using their personalized social info
        second_estimates[j] <- dIndividualSecondEstimate(
          first_estimate = first_estimates[j],
          social_info = social_info_vector[j],
          confidence = confidence[j]
        )
      }
      
      # For compatibility with the rest of the code, store the average social info
      social_info <- mean(social_info_vector)
    }
    
    # Determine WOC effects
    woc_effects <- determineWOCSocialInfluence(first_estimates, second_estimates, true_value)
    
    # Store results for this trial
    results[[i]] <- list(
      prior_knowledge = prior_knowledge,
      confidence = confidence,
      first_estimates = first_estimates,
      social_info = social_info,
      second_estimates = second_estimates,
      woc_effects = woc_effects
    )
  }
  
  if(n_trials == 1) {
    return(results[[1]])
  } else {
    aggregate_results <- list(
      # Group estimates
      mean_group_first = mean(sapply(results, function(x) x$woc_effects$group_mean_first)),
      mean_group_second = mean(sapply(results, function(x) x$woc_effects$group_mean_second)),
      mean_group_distance_first = mean(sapply(results, function(x) x$woc_effects$group_distance_first)),
      mean_group_distance_second = mean(sapply(results, function(x) x$woc_effects$group_distance_second)),
      
      # Individual averages
      mean_individual_distance_first = mean(sapply(results, function(x) x$woc_effects$individual_mean_distance_first)),
      mean_individual_distance_second = mean(sapply(results, function(x) x$woc_effects$individual_mean_distance_second)),
      
      # WOC benefits in original units
      mean_woc_benefit_first = mean(sapply(results, function(x) x$woc_effects$woc_benefit_first)),
      mean_woc_benefit_second = mean(sapply(results, function(x) x$woc_effects$woc_benefit_second)),
      mean_woc_benefit_change = mean(sapply(results, function(x) x$woc_effects$woc_benefit_change)),
      
      # WOC benefits as percentages
      mean_woc_percent_benefit_first = mean(sapply(results, function(x) x$woc_effects$woc_percent_benefit_first)),
      mean_woc_percent_benefit_second = mean(sapply(results, function(x) x$woc_effects$woc_percent_benefit_second)),
      mean_woc_percent_benefit_change = mean(sapply(results, function(x) x$woc_effects$woc_percent_benefit_change)),
      
      # Proportion of trials showing improvement
      proportion_group_improved = mean(sapply(results, function(x) x$woc_effects$group_distance_first > x$woc_effects$group_distance_second)),
      proportion_individual_improved = mean(sapply(results, function(x) x$woc_effects$individual_mean_distance_first > x$woc_effects$individual_mean_distance_second)),
      proportion_woc_strengthened = mean(sapply(results, function(x) x$woc_effects$woc_benefit_change > 0))
    )
    
    return(list(
      trial_results = results,
      aggregate_results = aggregate_results
    ))
  }
}



normal_group_results <- runSimulation(
  n_individuals = 10,              
  true_value = 100,              
  social_info_type = "mean",
  knowledge_distribution = "uniform", 
  knowledge_params = list(
    min = 0.1,                   
    max = 0.9                     
  ),
  n_trials = 100
)

print(normal_group_results$aggregate_results)