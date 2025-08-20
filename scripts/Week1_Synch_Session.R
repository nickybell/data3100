# File name: Week1_Synch_Session.R
# Description: This script contains code for the Week 1 synchronous session with a focus on measures of dispersion
# Date created: 2024-09-02
# Author: Nicholas Bell (belln@sas.upenn.edu)

# Load the tidyverse
library(tidyverse)

# Load the data
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
