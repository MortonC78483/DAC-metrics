## ---------------------------
##
## Script name: 10_simulation_for_hypergeom.R
##
## Purpose of script:
##    This script runs a simulation demonstrating the efficacy of the hypergeometric
##    estimator compared to the naive estimator, and creates a figure of estimator comparison
##
## Author: Claire Morton
##
## Date Created: 2025-12-27
##
## Email: mortonc@berkeley.edu
##
## ---------------------------
##
##
## ---------------------------

library(BiasedUrn)
library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(78483)

# Function to solve for MoM weight in Wallenius's hypergeometric distribution
solve_wallenius <- function(m1, m2, mu, n){
  num <- log(1 - (mu / m1))
  denom <- log(1 - ((n - mu) / m2))
  return(num / denom)
}
# mu is number of red balls drawn, so if p is the proportion of
# red balls that were drawn, it's log(1-p)
# over log(1- number white balls drawn/number of white balls)

# Simulation parameters
ms <- c(250, 500, 1000) # different values of m
props <- seq(0.1, 0.9, 0.01) # proportion sampled
odds_ratios <- c(1, 3, 5, 7) # different odds ratios
nran <- 50 # number of Wallenius samples per setting

# Data frame to store all draws
all_draws <- data.frame(
  m = numeric(),
  prop_sampled = numeric(),
  draw = numeric(),
  est_odds = numeric(),
  naive_odds = numeric(),
  odds_ratio = numeric()
)

# Run simulations
for (odds_ratio in odds_ratios) {
  for (m in ms) {
    for (prop in props) {
      
      m1 <- m
      m2 <- m
      n <- prop * 2 * m
      
      draws <- rWNCHypergeo(
        nran = nran,
        m1 = m1, m2 = m2,
        n = n,
        odds = odds_ratio
      )
      
      for (x in draws) {
        red_prop <- x / n
        est_weight <- solve_wallenius(m1, m2, x, n)
        naive_odds_est <- ifelse(red_prop == 0 | red_prop == 1, NA, red_prop / (1 - red_prop))
        
        all_draws <- rbind(all_draws, data.frame(
          m = m,
          prop_sampled = prop,
          draw = x,
          est_odds = est_weight,
          naive_odds = naive_odds_est,
          odds_ratio = odds_ratio
        ))
      }
    }
  }
}

# summarize results
results_summary <- all_draws %>%
  group_by(odds_ratio, m, prop_sampled) %>%
  summarise(
    mean_est_odds = mean(est_odds, na.rm = TRUE),
    se_est_odds = sd(est_odds, na.rm = TRUE) / sqrt(sum(!is.na(est_odds))),
    mean_naive_odds = mean(naive_odds, na.rm = TRUE),
    se_naive_odds = sd(naive_odds, na.rm = TRUE) / sqrt(sum(!is.na(naive_odds))),
    .groups = "drop"
  ) 
plot_df <- results_summary %>%
  pivot_longer(
    cols = c(mean_est_odds, mean_naive_odds),
    names_to = "estimator",
    values_to = "odds"
  ) %>%
  mutate(
    estimator = recode(
      estimator,
      mean_est_odds = "Wallenius",
      mean_naive_odds = "Naive"
    )
  )

color_breaks <- c(
  "250.Naive",
  "250.Wallenius", 
  "500.Naive", 
  "500.Wallenius", 
  "1000.Naive",
  "1000.Wallenius"
)

color_labels <- c(
  "Naive (m = 250)",
  "Wallenius (m = 250)",
  "Naive (m = 500)",
  "Wallenius (m = 500)",
  "Naive (m = 1000)",
  "Wallenius (m = 1000)"
)

# Plot with different m values as lines
ggplot(plot_df, aes(x = prop_sampled)) +
  geom_ribbon(
    data = results_summary,
    aes(
      ymin = mean_est_odds - 1.96*se_est_odds,
      ymax = mean_est_odds + 1.96*se_est_odds,
      fill = factor(m)
    ),
    alpha = 0.2,
    color = NA
  ) +
  geom_ribbon(
    data = results_summary,
    aes(
      ymin = mean_naive_odds - 1.96* se_naive_odds,
      ymax = mean_naive_odds + 1.96*se_naive_odds,
      fill = factor(m)
    ),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(
    aes(
      y = odds,
      color = interaction(m, estimator),
      group = interaction(m, estimator)
    ),
    linewidth = 1.2
  ) +
  # True odds ratio
  geom_hline(
    aes(yintercept = odds_ratio),
    linetype = "dashed",
    linewidth = 0.6,
    color = "gray30"
  ) +
  facet_wrap(~ odds_ratio, scales = "free_y") +
  scale_color_manual(
    values = c(
      "250.Naive"      = "#66c2a5",
      "250.Wallenius"  = "#1b9e77",
      "500.Naive"      = "#fdb863",
      "500.Wallenius"  = "#d95f02",
      "1000.Naive"     = "#b2abd2",
      "1000.Wallenius" = "#7570b3"
    ),
    labels = c(
      "Naive (m = 250)",
      "Wallenius (m = 250)",
      "Naive (m = 500)",
      "Wallenius (m = 500)",
      "Naive (m = 1000)",
      "Wallenius (m = 1000)"
    ),
    breaks = color_breaks,
    name = "Estimator"
  )+
  scale_fill_brewer(
    palette = "Dark2",
    name = "m value"
  ) +
  guides(fill = "none")+
  labs(
    x = "Proportion of Population Sampled",
    y = "Estimated Odds"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave("outputs/figures/fig_wallenius.png", width = 7, height = 7)
