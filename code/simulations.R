# Set seed for reproducibility
set.seed(60)

# Import necessarily libraries
source("code/functions.R")


# Function to determine WOC with social influence
determineSocialInfluenceEffects <- function(first_estimate_vector, second_estimate_vector, true_value) {
  # Calculate mean estimates
  mean_first <- mean(first_estimate_vector)
  mean_second <- mean(second_estimate_vector)
  
  # Calculate group distances (error of the aggregated estimate)
  collective_performance_first <- abs(mean_first - true_value)
  collective_performance_second <- abs(mean_second - true_value)
  
  # Calculate individual average distances (average error of individuals)
  # NOTE: Higher values correspond to lower accuracy
  collective_accuracy_first <- mean(abs(first_estimate_vector - true_value))
  collective_accuracy_second <- mean(abs(second_estimate_vector - true_value))
  
  # Collective accuracy change (change in aggregated individual errors)
  # NOTE: Positive values correspond to an accuracy loss after social influence
  #       as the average individual error has increased,
  #       negative values correspond to an accuracy benefit after social influence
  #       as the average individual error has decreased.
  collective_accuracy_change <- collective_accuracy_second - collective_accuracy_first
  
  # Collective accuracy change in percent (what % of average individual error has been eliminated)
  # NOTE: Positive values correspond to an accuracy loss after social influence
  #       as the average individual error has increased,
  #       negative values correspond to an accuracy benefit after social influence
  #       as the average individual error has decreased.
  collective_accuracy_percent_change <- ((collective_accuracy_second / collective_accuracy_first) - 1) * 100
  
  # Calculate WOC benefit in absolute units (how many units closer the group is vs avg individual)
  woc_benefit_first <- collective_accuracy_first - collective_performance_first
  woc_benefit_second <- collective_accuracy_second - collective_performance_second
  
  # Change in WOC benefit
  woc_benefit_change <- woc_benefit_second - woc_benefit_first
  
  # Percentage WOC benefit (what % of individual error is eliminated by aggregation)
  woc_percent_benefit_first <- (woc_benefit_first / collective_accuracy_first) * 100
  woc_percent_benefit_second <- (woc_benefit_second / collective_accuracy_second) * 100
  woc_percent_benefit_change <- woc_percent_benefit_second - woc_percent_benefit_first
  
  ## Also calculate the squared error metrics for completeness
  #error_first <- (mean_first - true_value)^2
  #error_second <- (mean_second - true_value)^2
  #individual_error_first <- mean((first_estimate_vector - true_value)^2)
  #individual_error_second <- mean((second_estimate_vector - true_value)^2)
  
  return(list(
    # Group estimates
    group_mean_first = mean_first,
    group_mean_second = mean_second,
    
    # Collective performance
    collective_performance_first = collective_performance_first,
    collective_performance_second = collective_performance_second,
    
    # Collective accuracy
    collective_accuracy_first = collective_accuracy_first,
    collective_accuracy_second = collective_accuracy_second,
    
    # Collective accuracy change
    collective_accuracy_change = collective_accuracy_change,
    collective_accuracy_percent_change = collective_accuracy_percent_change,
    
    # WOC benefits in original units
    woc_benefit_first = woc_benefit_first,
    woc_benefit_second = woc_benefit_second,
    woc_benefit_change = woc_benefit_change,
    
    # WOC benefits as percentages
    woc_percent_benefit_first = woc_percent_benefit_first,
    woc_percent_benefit_second = woc_percent_benefit_second,
    woc_percent_benefit_change = woc_percent_benefit_change
    
    ## Original squared error metrics
    #error_first = error_first,
    #error_second = error_second,
    #individual_error_first = individual_error_first,
    #individual_error_second = individual_error_second,
    #improvement = error_first - error_second,
    #individual_improvement = individual_error_first - individual_error_second
  ))
}

# Function to run a simulation
runSimulation <- function(n_individuals, true_value, knowledge_distribution = "uniform", 
                          knowledge_params = list(), social_info_type = "mean", n_trials = 1, extension=FALSE) {
  
  results <- list()
  
  for(i in 1:n_trials) {
    # Generate prior knowledge
    prior_knowledge <- dPriorKnowledge(n_individuals, knowledge_distribution, knowledge_params)
    
    # Calculate confidence
    confidence <- sapply(prior_knowledge, calculateConfidence)
    
    # Generate first estimates
    first_estimates <- dGroupFirstEstimate(prior_knowledge, true_value)
    
    # Calculate personalized social information and second estimates
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
    
    # Determine effects of social influence
    social_influence_effects <- determineSocialInfluenceEffects(first_estimates, second_estimates, true_value)
    
    # Calculate the revision-coefficient (Becker et al., 2017) for this trial
    # We calculate the correlation between the observed individual accuracy and
    # the observed adoption to advice 
    # (these measures can deviate from the expected accuracy in term of a persons prior knowledge and 
    # from the expected adoption in terms of a persons `psychological` weight on advice)
    # We calculate the factual weight on advice with the common formula identified by (Bailey et al., 2023):
    # [(final estimate—initial estimate) / (advice – initial estimate)]
    factual_weight_on_advice <- abs((second_estimates - first_estimates) / (social_info_vector - first_estimates))
    initial_individual_errors <- abs(first_estimates - true_value)
    revision_coefficient <- cor(initial_individual_errors, factual_weight_on_advice)

    
    # Store results for this trial
    results[[i]] <- list(
      prior_knowledge = prior_knowledge,
      confidence = confidence,
      first_estimates = first_estimates,
      social_info = social_info,
      social_info_accuracy = mean(abs(social_info_vector - true_value)),
      second_estimates = second_estimates,
      social_influence_effects = social_influence_effects,
      revision_coefficient = revision_coefficient,
      avg_deviations = mean(abs(first_estimates - social_info_vector))
    )
  }
  
  if(n_trials == 1) {
    return(results[[1]])
  } else {
    aggregate_results <- list(
      
      # Average collective accuracy
      avg_collective_accuracy_first = mean(sapply(results, function(x) x$social_influence_effects$collective_accuracy_first)),
      avg_collective_accuracy_second = mean(sapply(results, function(x) x$social_influence_effects$collective_accuracy_second)),
      
      # Average collective accuracy changes
      avg_collective_accuracy_change = mean(sapply(results, function(x) x$social_influence_effects$collective_accuracy_change)),
      avg_collective_accuracy_percent_change = mean(sapply(results, function(x) x$social_influence_effects$collective_accuracy_percent_change)),
      
      # Average social information accuracy
      avg_social_info_accuracy = mean(sapply(results, function(x) x$social_info_accuracy)),
      
      # Average average deviations of first estimates from social information
      avg_deviations = mean(sapply(results, function(x) x$avg_deviations)),
      
      # Average revision-coefficients
      avg_revision_coefficients = mean(sapply(results, function(x) x$revision_coefficient))
      
      ## Group estimates
      #mean_group_first = mean(sapply(results, function(x) x$woc_effects$group_mean_first)),
      #mean_group_second = mean(sapply(results, function(x) x$woc_effects$group_mean_second)),
      #mean_group_distance_first = mean(sapply(results, function(x) x$woc_effects$group_distance_first)),
      #mean_group_distance_second = mean(sapply(results, function(x) x$woc_effects$group_distance_second)),
      #
      ## Individual averages
      #mean_individual_distance_first = mean(sapply(results, function(x) x$woc_effects$individual_mean_distance_first)),
      #mean_individual_distance_second = mean(sapply(results, function(x) x$woc_effects$individual_mean_distance_second)),
      #
      ## WOC benefits in original units
      #mean_woc_benefit_first = mean(sapply(results, function(x) x$woc_effects$woc_benefit_first)),
      #mean_woc_benefit_second = mean(sapply(results, function(x) x$woc_effects$woc_benefit_second)),
      #mean_woc_benefit_change = mean(sapply(results, function(x) x$woc_effects$woc_benefit_change)),
      #
      ## WOC benefits as percentages
      #mean_woc_percent_benefit_first = mean(sapply(results, function(x) x$woc_effects$woc_percent_benefit_first)),
      #mean_woc_percent_benefit_second = mean(sapply(results, function(x) x$woc_effects$woc_percent_benefit_second)),
      #mean_woc_percent_benefit_change = mean(sapply(results, function(x) x$woc_effects$woc_percent_benefit_change)),
      #
      ## Proportion of trials showing improvement
      #proportion_group_improved = mean(sapply(results, function(x) x$woc_effects$group_distance_first > x$woc_effects$group_distance_second)),
      #proportion_individual_improved = mean(sapply(results, function(x) x$woc_effects$individual_mean_distance_first > x$woc_effects$individual_mean_distance_second)),
      #proportion_woc_strengthened = mean(sapply(results, function(x) x$woc_effects$woc_benefit_change > 0))
    )
    
    return(list(
      trial_results = results,
      aggregate_results = aggregate_results
    ))
  }
}

#=================#
# SIMULATION RUNS #
#=================#

# number of trials for all simulations
n_trials <- 100
# true value for all simulations
# value is taken from intuition, not to low and not too high
# because we take the log of deviations, the order of magnitude
# of the true_value is respected, and different values for the truth
# should have no significant impact on the test results
true_value <- 1000
# value is taken from intuition, we dont expect an impact
# of other values for n on the results.
# As the IFE distributions are not identially distributed,
# we dont expect a convergence of the mean of the first estimates
# to the true value (central limit theorem).
n_group_members <- 100

#------------#
# BASE MODEL #
#------------#

# C1: low knowledge condition
b_c1_results <- runSimulation(
  n_individuals = n_group_members,              
  true_value = true_value,              
  social_info_type = "mean",
  knowledge_distribution = "truncnorm", 
  knowledge_params = list(
    mean = 0.2,                   
    sd = 0.4                     
  ),
  n_trials = n_trials
)

# C2: moderate knowledge condition
b_c2_results <- runSimulation(
  n_individuals = n_group_members,              
  true_value = true_value,              
  social_info_type = "mean",
  knowledge_distribution = "truncnorm", 
  knowledge_params = list(
    mean = 0.5,                   
    sd = 0.4                     
  ),
  n_trials = n_trials
)

# C3: high knowledge condition
b_c3_results <- runSimulation(
  n_individuals = n_group_members,              
  true_value = true_value,              
  social_info_type = "mean",
  knowledge_distribution = "truncnorm", 
  knowledge_params = list(
    mean = 0.8,                   
    sd = 0.8                     
  ),
  n_trials = n_trials
)

# Group results aggregated over trials
summary_base_model <- data.frame(
  acc_first = c(
    b_c1_results$aggregate_results$avg_collective_accuracy_first,
    b_c2_results$aggregate_results$avg_collective_accuracy_first,
    b_c3_results$aggregate_results$avg_collective_accuracy_first
  ),
  acc_si = c(
    b_c1_results$aggregate_results$avg_social_info_accuracy,
    b_c2_results$aggregate_results$avg_social_info_accuracy,
    b_c3_results$aggregate_results$avg_social_info_accuracy
  ),
  avg_dev = c(
    b_c1_results$aggregate_results$avg_deviations,
    b_c2_results$aggregate_results$avg_deviations,
    b_c3_results$aggregate_results$avg_deviations
  ),
  acc_second = c(
    b_c1_results$aggregate_results$avg_collective_accuracy_second,
    b_c2_results$aggregate_results$avg_collective_accuracy_second,
    b_c3_results$aggregate_results$avg_collective_accuracy_second
  ),
  acc_change = c(
    b_c1_results$aggregate_results$avg_collective_accuracy_change,
    b_c2_results$aggregate_results$avg_collective_accuracy_change,
    b_c3_results$aggregate_results$avg_collective_accuracy_change
  ),
  acc_perc_change = c(
    b_c1_results$aggregate_results$avg_collective_accuracy_percent_change,
    b_c2_results$aggregate_results$avg_collective_accuracy_percent_change,
    b_c3_results$aggregate_results$avg_collective_accuracy_percent_change
  ),
  revision_coefficients = c(
    b_c1_results$aggregate_results$avg_revision_coefficients,
    b_c2_results$aggregate_results$avg_revision_coefficients,
    b_c3_results$aggregate_results$avg_revision_coefficients
  )
)
rownames(df_collective_accuracy_change) <- c("C1_LPK", "C2_MPK", "C3_HPK")

#-----------#
# EXTENSION #
#-----------#

# C1: low knowledge condition
b_c1_results <- runSimulation(
  n_individuals = n_group_members,              
  true_value = true_value,              
  social_info_type = "mean",
  knowledge_distribution = "truncnorm", 
  knowledge_params = list(
    mean = 0.2,                   
    sd = 0.4                     
  ),
  n_trials = n_trials,
  extension = TRUE
)

# C2: moderate knowledge condition
b_c2_results <- runSimulation(
  n_individuals = n_group_members,              
  true_value = true_value,              
  social_info_type = "mean",
  knowledge_distribution = "truncnorm", 
  knowledge_params = list(
    mean = 0.5,                   
    sd = 0.4                     
  ),
  n_trials = n_trials,
  extension = TRUE
)

# C3: high knowledge condition
b_c3_results <- runSimulation(
  n_individuals = n_group_members,              
  true_value = true_value,              
  social_info_type = "mean",
  knowledge_distribution = "truncnorm",
  knowledge_params = list(
    mean = 0.8,                   
    sd = 0.8                     
  ),
  n_trials = n_trials,
  extension = TRUE
)

# Group results aggregated over trials
summary_extension <- data.frame(
  acc_first = c(
    b_c1_results$aggregate_results$avg_collective_accuracy_first,
    b_c2_results$aggregate_results$avg_collective_accuracy_first,
    b_c3_results$aggregate_results$avg_collective_accuracy_first
  ),
  acc_si = c(
    b_c1_results$aggregate_results$avg_social_info_accuracy,
    b_c2_results$aggregate_results$avg_social_info_accuracy,
    b_c3_results$aggregate_results$avg_social_info_accuracy
  ),
  avg_dev = c(
    b_c1_results$aggregate_results$avg_deviations,
    b_c2_results$aggregate_results$avg_deviations,
    b_c3_results$aggregate_results$avg_deviations
  ),
  acc_second = c(
    b_c1_results$aggregate_results$avg_collective_accuracy_second,
    b_c2_results$aggregate_results$avg_collective_accuracy_second,
    b_c3_results$aggregate_results$avg_collective_accuracy_second
  ),
  acc_change = c(
    b_c1_results$aggregate_results$avg_collective_accuracy_change,
    b_c2_results$aggregate_results$avg_collective_accuracy_change,
    b_c3_results$aggregate_results$avg_collective_accuracy_change
  ),
  acc_perc_change = c(
    b_c1_results$aggregate_results$avg_collective_accuracy_percent_change,
    b_c2_results$aggregate_results$avg_collective_accuracy_percent_change,
    b_c3_results$aggregate_results$avg_collective_accuracy_percent_change
  ),
  revision_coefficients = c(
    b_c1_results$aggregate_results$avg_revision_coefficients,
    b_c2_results$aggregate_results$avg_revision_coefficients,
    b_c3_results$aggregate_results$avg_revision_coefficients
  )
)
rownames(df_collective_accuracy_change) <- c("C1_LPK", "C2_MPK", "C3_HPK")