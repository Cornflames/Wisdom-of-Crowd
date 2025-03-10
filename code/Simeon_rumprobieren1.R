# Berechnung der ersten Schätzung (First Estimate)
calculate_first_estimate <- function(true_value) {
  # Zufällige Abweichung um den wahren Wert basierend auf einer Cauchy-Verteilung
  sigma <- 2  # moderate Streuung
  fe <- true_value + rcauchy(1, location = 0, scale = sigma)
  return(fe)
}

# Berechnung der Abweichung (Deviation)
calculate_deviation <- function(fe, si) {
  abs(si - fe) / fe  # Absolute Abweichung zwischen First Estimate und Social Information
}

# Berechnung des sozialen Gewichts (Social Weight)
calculate_social_weight <- function(deviation, advice_quality) {
  discounting <- 0.3  # Egocentric Discounting (30 %)
  quality_adjustment <- ifelse(advice_quality == "high", 0.48,
                               ifelse(advice_quality == "average", 0.37, 0.32))
  sw <- deviation - (discounting * quality_adjustment)
  return(pmax(0, pmin(1, sw)))  # Begrenzung zwischen 0 und 1
}

# Berechnung der zweiten Schätzung (Second Estimate)
calculate_second_estimate <- function(fe, si, sw) {
  (1 - sw) * fe + sw * si
}

# Simulationsfunktion
simulate_wisdom_of_crowds <- function(true_value, si, advice_quality, group_size = 12, simulations = 10) {
  all_results <- list()  # Liste für alle Simulationen
  
  for (sim in 1:simulations) {
    fe_values <- numeric(group_size)
    se_values <- numeric(group_size)
    
    # Generiere First Estimates und Second Estimates für jede Person in der Gruppe
    for (i in 1:group_size) {
      fe <- calculate_first_estimate(true_value)
      deviation <- calculate_deviation(fe, si)
      sw <- calculate_social_weight(deviation, advice_quality)
      se <- calculate_second_estimate(fe, si, sw)
      
      fe_values[i] <- fe
      se_values[i] <- se
    }
    
    # Berechne aggregierte Schätzung und individuelle Abweichungen
    agg_se <- mean(se_values)
    avg_indiv_se <- mean(abs(se_values - true_value))
    woc <- ifelse(abs(agg_se - true_value) <= avg_indiv_se, 1, 0)
    
    # Speichere die Ergebnisse der Simulation
    all_results[[sim]] <- list(
      Simulation = sim,
      Aggregated_Second_Estimate = agg_se,
      Avg_Individual_Deviation = avg_indiv_se,
      Wisdom_of_Crowds = woc,
      First_Estimates = fe_values,
      Second_Estimates = se_values
    )
  }
  
  return(all_results)
}

# Beispiel-Simulation ohne set.seed und mit Gruppengröße 12
sim_results <- simulate_wisdom_of_crowds(true_value = 100, si = 130, advice_quality = "average", group_size = 12, simulations = 10)

# Zeige Ergebnisse für jede Simulation
print(sim_results)
