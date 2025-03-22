source("code/functions.R")
library(ggplot2)
library(dplyr)
library(patchwork)

set.seed(40)

# Plot of weight on advice against different confidence levels
# and distances between first estimate and social info
woa_conf_si_data <- expand.grid(
    conf = seq(from = 0, to = 1, by = 0.2),
    distance = seq(from = 0, to = 1, by = 0.2)
)
woa_conf_si_data <- woa_conf_si_data %>% rowwise() %>% mutate(woa = getWeightOnAdvice(conf, distance, 1))

ggplot(data = woa_conf_si_data, mapping = aes(x = conf, y = woa, colour = factor(distance))) +
  scale_x_continuous(name = "Confidence", breaks = seq(0, 1, by = 0.2)) +
  ylab("Weight on Advice (WOA)") +
  guides(
    colour = guide_legend("Distance")
  ) +
  geom_point() +
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