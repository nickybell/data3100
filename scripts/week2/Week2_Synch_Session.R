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

# Let's go back to the data on congressional ideology from Voteview that we used in Week 1.
df <- read_csv("data/week1/voteview_house_ideology.csv")

# Let's start by visualizing the distribution of economic ideology scores for each party. Are the distributions approximately normal?

ggplot(df) +
  geom_histogram(aes(x = nominate_dim1, fill = party), bins = 30) +
  scale_fill_manual(
    values = c("Democrat" = "#002B47", "Republican" = "#E71F1F")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Our research question is whether the extreme wings of each party have become more ideologically extreme over time. Put another way, has the ideology score of the 90th percentile of the distribution moved further toward the extremes over time?

# Let's start by calculating the 90th percentile of the ideology scores for each party in each Congress.
extremes <- df |>
  group_by(congress, party) |>
  summarize(
    mean_dim1 = mean(nominate_dim1, na.rm = TRUE),
    sd_dim1 = sd(nominate_dim1, na.rm = TRUE)
  ) |>
  mutate(
    p90 = case_when(
      party == "Republican" ~ qnorm(0.9, mean_dim1, sd_dim1),
      party == "Democrat" ~ qnorm(0.1, mean_dim1, sd_dim1)
    )
  )

# Let's visualize the 90th percentile for each party over time.
extremes |>
  ggplot() +
  geom_line(aes(x = congress, y = abs(p90), color = party)) +
  scale_color_manual(
    values = c("Democrat" = "#002B47", "Republican" = "#E71F1F")
  ) +
  labs(
    title = "90th Percentile of DW-NOMINATE Scores by Party Over Time",
    x = "Congress",
    y = "90th Percentile (absolute value)",
    color = "Party"
  ) +
  theme_minimal()

# Now we might wonder about the *least* extreme members of each party - has bipartisanship declined? We can calculate the percentage of each party's distribution that falls beyond the 95% percentile of the opposite party's distribution.

# First, we need to calculate the 95th percentile of each party's distribution in each Congress.
extremes <- extremes |>
  mutate(
    p05 = case_when(
      party == "Republican" ~ qnorm(0.05, mean_dim1, sd_dim1),
      party == "Democrat" ~ qnorm(.95, mean_dim1, sd_dim1)
    )
  )

# Now we can calculate the percentage of each party's distribution that falls beyond the 95th percentile of the opposite party's distribution.
bipartisanship <- extremes |>
  group_by(congress) |>
  mutate(
    prop_bipartisan = case_when(
      party == "Democrat" ~
        pnorm(
          p05[party == "Republican"],
          mean_dim1,
          sd_dim1,
          lower.tail = FALSE
        ),
      party == "Republican" ~
        1 -
          pnorm(
            p05[party == "Democrat"],
            mean_dim1,
            sd_dim1,
            lower.tail = FALSE
          )
    )
  )
