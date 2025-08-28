# File name: Week1_Synch_Session.R
# Description: This script contains code for the Week 1 synchronous session with a focus on measures of dispersion
# Date created: 2024-09-02
# Author: Nicholas Bell (belln@sas.upenn.edu)

# Load the tidyverse
library(tidyverse)

# In the video lecture, we were told that the reason we square the differences between the values and the mean when calculating variance is to avoid negative values canceling out positive values.
# There is actually a second reason: squaring the differences gives more weight to larger deviations from the mean, which is useful in many statistical contexts.
# Let's illustrate this with a simple example. I am going to generate a sequence of values with the same mean but increasing absolute distance from the mean at the extremes.

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

# Dispersion

# Coefficient of Variation

# Our research question is whether Democrats and Republicans have become more ideologically homogenous (less dispersed) over time.
# Let's calculate the standard deviation of nominate_dim1 for each party in each Congress.

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
  scale_y_continuous(limits = c(-1, NA)) +
  labs(
    title = "Central Tendency and Dispersion of DW-NOMINATE Scores for Democrats Over Time",
    x = "Congress",
    y = "DW-NOMINATE Score"
  ) +
  theme_minimal()
