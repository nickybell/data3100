# File name: Week2_Synch_Session.R
# Description: This script contains code for the Week 2 synchronous session with a focus on the normal distribution and z-scores
# Date created: 2024-09-01
# Author: Nicholas Bell (belln@sas.upenn.edu)

# Load the tidyverse
library(tidyverse)

# In the video lecture, we were told that the best way to evaluate whether we have a normal distribution is to visualize it. Is the distribution centered around the mean? Is it symmetric? Does it have the characteristic bell shape?
# There is another rule of thumb we can use to evaluate whether a distribution is normal: the Central Limit Theorem (CLT).
# The CLT states that the sampling distribution of the sample mean will be approximately normal if the sample size is sufficiently large, regardless of the shape of the population distribution.
# The rule of thumb is that a sample size of 30 or more is sufficient for the CLT to hold. Why?
# One story is that n=30 was a good number for fitting Z-tables (https://math.arizona.edu/~rsims/ma464/standardnormaltable.pdf) on a textbook page, and after n=30 the difference in the tails of the distribution becomes negligible.
# Let's see if we can prove the CLT with a simple simulation.

set.seed(20016)
clt_sim <- tibble::tibble()
for (i in seq(5, 100, 5)) {
  # or 1:30
  clt_sim <- tibble::tibble(
    rep = i,
    samples = rnorm(i, mean = 0, sd = .25)
  ) |>
    bind_rows(clt_sim)
}

ggplot(clt_sim, aes(x = samples)) +
  geom_histogram() +
  facet_wrap(~rep, ncol = 5) +
  labs(
    title = "Central Limit Theorem Simulation",
    x = "Sample Values",
    y = "N"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

df <- data.frame()
for (i in 1:20) {
  # i is going to represent the magnitude of the deviation from the mean
  vals <- c()
  for (j in 1:100) {
    # j is going to represent the length of the vector
    start <- ifelse(length(vals) > 0, max(vals), 0) # Start at 0 for the first iteration, otherwise start at the max value of the previous iteration
    vals <- c(vals, start + i, -(start + i))
  }
  df <- rbind(
    df,
    data.frame(n = i, variance = sum((vals - 0)^2) / length(vals))
  )
}

# Now let's visualize the variance of these values as we increase the magnitude of the deviation from the mean.
ggplot(df, aes(x = n, y = variance)) +
  geom_line() +
  labs(
    title = "Variance of Values Over Iterations",
    x = "Iteration Number (n)",
    y = "Variance"
  ) +
  theme_minimal()

# What do you notice? The variance increases *expontentially* as the magnitude of the deviation from the mean increases. This is because squaring the differences gives more weight to larger deviations.

# Now let's talk about how we might use variance in a political science context.
# For this session, we will be using voteview_house_ideology.csv, which contains DW-NOMINATE scores for members of the U.S. House of Representatives from the 102nd Congress (1991-1992) to the 118th Congress (2023-2024).
df <- read_csv("data/voteview_house_ideology.csv")

# View the data
glimpse(df)

# This data represents a population because it contains all members of the U.S. House of Representatives from the 102nd to the 118th Congress.

# Let's start by calculating the central tendency and dispersion of the DW-NOMINATE scores (nominate_dim1) for all members in the dataset.
# Central Tendency
mean_dim1 <- mean(df$nominate_dim1, na.rm = TRUE)
mean_dim1

# Dispersion
var_dim1 <- sum((df$nominate_dim1 - mean_dim1)^2, na.rm = TRUE) /
  length(df$nominate_dim1)
var_dim1

sd_var1 <- sqrt(var_dim1)
sd_var1

# Coefficient of Variation
sd_var1 / mean_dim1 # This is not a meaningful measure in this context because the mean is close to zero.

# Our research question is whether Democrats and Republicans have become more ideologically homogenous (less dispersed) over time.
# Let's calculate the standard deviation of nominate_dim1 for each party in each Congress.
dispersion_by_party <- df |>
  group_by(congress, party) |>
  mutate(
    mean_dim1 = mean(nominate_dim1, na.rm = TRUE),
    diff_from_mean = nominate_dim1 - mean_dim1,
    squared_diff = diff_from_mean^2
  ) |>
  summarize(
    mean_dim1 = mean(nominate_dim1, na.rm = TRUE),
    sum_of_squared_diff = sum(squared_diff, na.rm = TRUE),
    variance = sum_of_squared_diff / n(),
    sd = sqrt(variance)
  )

# Now we can visualize the dispersion of DW-NOMINATE scores by party over time.
ggplot(dispersion_by_party) +
  geom_line(aes(x = congress, y = sd, color = factor(party))) +
  scale_color_manual(
    values = c("Democrat" = "#002B47", "Republican" = "#E71F1F")
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Dispersion of DW-NOMINATE Scores by Party Over Time",
    x = "Congress",
    y = "Standard Deviation",
    color = "Party Code"
  ) +
  theme_minimal()

# We might want to represent both the central tendency and dispersion of DW-NOMINATE scores in a single plot. Let's just focus on the Democratic party for now.
# To do this, we can use a geom layer called geom_errorbar.
dispersion_by_party |>
  filter(party == "Democrat") |>
  ggplot() +
  geom_col(aes(x = factor(congress), y = mean_dim1), fill = "#002B47") +
  geom_errorbar(
    aes(x = factor(congress), ymin = mean_dim1 - sd, ymax = mean_dim1 + sd),
    color = "gray50",
    width = 0.2
  ) +
  scale_y_continuous(limits = c(-1, NA)) +
  labs(
    title = "Central Tendency and Dispersion of DW-NOMINATE Scores for Democrats Over Time",
    x = "Congress",
    y = "DW-NOMINATE Score"
  ) +
  theme_minimal()
