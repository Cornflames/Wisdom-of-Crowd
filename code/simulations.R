source("code/functions.R")
library(ggplot2)
library(dplyr)

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