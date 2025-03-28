# Set seed for reproducibility
set.seed(40)

# Import necessarily libraries
source("code/functions.R")
library(ggplot2)
library(dplyr)
library(patchwork)

#=============================================#
# Plots for demonstrating model relationships #
#=============================================#

#------------#
# BASE MODEL #
#------------#

# Plot of weight on advice against different confidence levels
# and distances between first estimate and social info
woa_conf_si_data <- expand.grid(
  conf = seq(from = 0, to = 1, by = 0.1),
  distance = seq(from = 0, to = 4, by = 0.1)
)
  
woa_conf_si_data <- woa_conf_si_data %>% rowwise() %>% mutate(woa = (1 - conf) * (1 + conf * tanh(distance)))

ggplot(data = woa_conf_si_data, mapping = aes(x = distance, y = woa, colour = factor(conf))) +
  scale_x_continuous(name = "Distance |log(SI/FE)|", breaks = seq(0, 4, by = 0.5)) +
  ylab("Weight on Advice (WOA)") +
  guides(
    colour = guide_legend("Confidence")
  ) +
  geom_line()

# Plot the distribution of individual estimates for
# different levels of prior knowledge
true_value = 20
pk_breaks <- seq(from = 0, to = 1, by = 0.2)
pk_fe_data <- data.frame(
  pk = rep(pk_breaks, times = 100)
)
pk_fe_data <- pk_fe_data %>% rowwise() %>% mutate(sd_log = -0.95 * pk + 1, fe = dIndividualFirstEstimate(pk, true_value))

true_value_line <- data.frame(yintercept = true_value, Lines = "True Value")

violin <- ggplot(data = pk_fe_data, aes(x = pk, y = fe, fill = factor(sd_log))) +
  scale_x_continuous(name = "Prior Knowledge", breaks = seq(0, 1, by = 0.2)) +
  ylab("First Estimate") +
  geom_violin() +
  guides(
    fill = guide_legend("SD of log")
  ) +
  geom_hline(aes(yintercept = yintercept, linetype = Lines), true_value_line)

boxplot <- ggplot(data = pk_fe_data, aes(x = pk, y = fe, fill = factor(sd_log))) +
  scale_x_continuous(name = "Prior Knowledge", breaks = seq(0, 1, by = 0.2)) +
  ylab("First Estimate") +
  geom_boxplot() +
  guides(
    fill = guide_legend("SD of log")
  ) +
  geom_hline(aes(yintercept = yintercept, linetype = Lines), true_value_line)

violin + boxplot

#-----------#
# EXTENSION #
#-----------#

# Plot reaction strength against distance for different
# reactivity levels
dist_rea_data <- expand.grid(
  distance = seq(from = 0, to = 1, by = 0.1),
  reactivity = seq(from = 0, to = 1, by = 0.2)
)
dist_rea_data <- dist_rea_data %>% rowwise() %>% mutate(react_factor = reactivity * (distance^2) / 4)

ggplot(data = dist_rea_data, mapping = aes(x = distance, y = react_factor, colour = factor(reactivity))) +
  scale_x_continuous(name = "Distance |log(SI/FE)|", breaks = seq(0, 4, by = 0.5)) +
  ylab("reaction strength") +
  guides(
    colour = guide_legend("Reactivity")
  ) +
  geom_line()


# Plot the distance-woa-confidence-relationship for different
# levels of reactivity
dist_conf_rea_data <- expand.grid(
  distance = seq(from = 0, to = 4, by = 0.5),
  conf = seq(from = 0, to = 1, by = 0.1),
  reactivity = seq(from = 0, to = 1, by = 0.2)
)
dist_conf_rea_data <- dist_conf_rea_data %>% rowwise() %>% mutate(woa = getWeightOnAdviceExtended(conf, distance + 1, 1, reactivity))

ggplot(data = dist_conf_rea_data, mapping = aes(x = distance, y = woa, colour = factor(conf))) +
  scale_x_continuous(name = "Distance |log(SI/FE)|", breaks = seq(0, 4, by = 1)) +
  ylab("Weight on Advice (WOA)") +
  guides(
    colour = guide_legend("Confidence")
  ) +
  geom_line() +
  facet_wrap(~reactivity)


#=============================================#
# Plots for results of the actual simulations #
#=============================================#

source("code/simulations.R")

#------------#
# BASE MODEL #
#------------#

# Plot the relationship between revision-coefficients and collective accuracy
# within the trials of the different simulation conditions
rev_acc_data <- data.frame(
  condition = rep(c("C1", "C2", "C3"), each = n_trials),
  pk = rep(c("low", "moderate", "high"), each = n_trials),
  rev_coef = c(
    sapply(b_c1_results$trial_results, function (x) x$revision_coefficient),
    sapply(b_c2_results$trial_results, function (x) x$revision_coefficient),
    sapply(b_c3_results$trial_results, function (x) x$revision_coefficient)
  ),
  acc_change_perc = c(
    sapply(b_c1_results$trial_results, function (x) x$social_influence_effects$collective_accuracy_percent_change),
    sapply(b_c2_results$trial_results, function (x) x$social_influence_effects$collective_accuracy_percent_change),
    sapply(b_c3_results$trial_results, function (x) x$social_influence_effects$collective_accuracy_percent_change)
  )
)
ggplot(rev_acc_data, aes(x = rev_coef, y = acc_change_perc, colour = factor(pk))) +
  xlab("revision-coefficient") +
  ylab("change in collective accuracy (%)") +
  geom_point() +
  geom_smooth() +
  guides(
    colour = guide_legend("Prior Knowledge")
  )