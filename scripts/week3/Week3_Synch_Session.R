# File name: Week3_Synch_Session.R
# Description: This script contains code for the Week 3 synchronous session with a focus on the sampling distribution
# Date created: 2024-09-06
# Author: Nicholas Bell (belln@sas.upenn.edu)

# Load the tidyverse
library(tidyverse)

# In the video lectures, we learned that we can decrease the dispersion (increase the precision) of our sample means by increasing our sample size. Yet, pollsters often use small sample sizes of around 1,000 respondents. Why don't pollsters use larger sample sizes to increase the precision of their estimates?

# Let's say we are measuring a variable with a mean of 50 and a standard deviation of 10. (This meanas that How much does the standard deviation of our sample means decrease as we increase our sample size?

population <- rnorm(100000, mean = 50, sd = 10)

sample_size_df <- tibble()
for (sample_size in seq(200, 3000, 200)) {
  sample_means <- replicate(1000, {
    mean(
      sample(population, sample_size, replace = TRUE)
    )
  })
  sample_means_df <- tibble(
    sample_size = sample_size,
    sample_means = sample_means
  )
  sample_size_df <- bind_rows(sample_size_df, sample_means_df)
}

ggplot(sample_size_df, aes(x = sample_means)) +
  geom_histogram() +
  facet_wrap(~sample_size, ncol = 5) +
  labs(
    title = "Sampling Distribution of the Sample Mean",
    x = "Sample Means",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Now let's work with some real data, specifically from NOAA's National Buoy Data Center (https://www.ndbc.noaa.gov/). The data contains hourly measurements of wind speed, wind direction, air temperature, and water temperature from over a thousand buoys around the world. I've already downloaded the data for you and generated the mean water surface temperature for each buoy for each month.
buoy <- read_csv("data/week3/buoydata.csv")

# Given this data, how certain can we be that global temperatures have been rising since 1990? It depends on the sampling distribution! When the sampling distribution is tight, we can be more certain that the observed trend is real. When the sampling distribution is wide, we can't be as certain.

# For each year since 1990, I am going to sample 30 buoys (with replacement) and calculate the mean water temperature. I will repeat this process 100 times for each year.
set.seed(20912)
sampling_dist <- tibble()
for (year in 1990:2023) {
  cat("Processing year:", year, "\n")
  for (i in 1:100) {
    sampled_buoys <- buoy |>
      filter(YEAR == year) |>
      sample_frac(.1, replace = TRUE)
    mean_temp <- mean(sampled_buoys$WTMP, na.rm = TRUE)
    sampling_dist <- bind_rows(
      sampling_dist,
      tibble(
        YEAR = year,
        sample_mean = mean_temp,
        iteration = i
      )
    )
  }
}

# We can also calculate our estimate of the mean from our samples.
estimate_means <- sampling_dist |>
  group_by(date = make_datetime(YEAR)) |>
  summarize(estimate_mean = mean(sample_mean, na.rm = TRUE))

# Now, I'm going to plot the timeseries of sampled ocean surface temperatures, along with the mean.

sampling_dist |>
  mutate(date = make_datetime(YEAR)) |>
  ggplot() +
  geom_line(aes(x = date, y = sample_mean, group = iteration), alpha = 0.1) +
  geom_line(
    data = estimate_means,
    aes(x = date, y = estimate_mean),
    color = "red",
    size = 1
  ) +
  theme_minimal() +
  labs(
    title = "Sampling Distribution of Ocean Surface Temperatures",
    x = "Year",
    y = "Mean Water Temperature (°C)"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
