# File name: Week4_Synch_Session.R
# Description: This script contains code for the Week 4 synchronous session with a focus on one-sample hypothesis testing
# Date created: 2024-09-06
# Author: Nicholas Bell (belln@sas.upenn.edu)

# Today, we are going to work with data from the General Social Survey (GSS), which is a nationally representative survey of adults in the United States. The GSS has been conducted since 1972 and contains a wealth of information on social, political, and economic issues.

# Scholar Kieran Healy has compiled the GSS data into a package format that is easy to work with. You can find more information here: https://kjhealy.github.io/gssr/

# In the meantime, here is the code to install the two packages we will need for today's session.
# Install 'gssr' from 'ropensci' universe
install.packages(
  'gssr',
  repos = c('https://kjhealy.r-universe.dev', 'https://cloud.r-project.org')
)

# Also recommended: install 'gssrdoc' as well
install.packages(
  'gssrdoc',
  repos = c('https://kjhealy.r-universe.dev', 'https://cloud.r-project.org')
)

library(tidyverse)
library(gssr)
library(gssrdoc)

# And load the GSS data and documentation
data(gss_all) # This may take a moment
data(gss_dict)

# Let's look at the "rank" variable.
?rank

# Let's also limit the data to just the 2024 survey and remove any missing values.
rank_2024 <- gss_all |>
  filter(year == 2024 & !is.na(rank))


# Now, let's conduct a one-sample t-test to see if the mean rank in 2024 is significantly different from 5 (the midpoint of the scale).
rank_t_test <- t.test(rank_2024$rank, mu = 5)
rank_t_test

# For the sake of exploration, let's generate a bootstrap sample of this variable. A bootstrap sample is a random sample with replacement from the original data. This closely approximates the sampling distribution.
set.seed(20912)
samp_size <- length(rank_2024$rank)
boot_samples <- c()
for (i in seq_len(10000)) {
  boot_samp <- sample(rank_2024$rank, size = samp_size, replace = TRUE)
  boot_mean <- mean(boot_samp)
  boot_samples <- c(boot_samples, boot_mean)
}

# Let's visualize the bootstrap distribution of the mean along with the 95% confidence interval.
ggplot(tibble(means = boot_samples)) +
  geom_density(aes(x = means)) +
  geom_vline(
    xintercept = mean(boot_samples),
    linetype = "dashed",
    color = "orangered"
  ) +
  annotate(
    "text",
    x = mean(boot_samples) + 0.05,
    y = 1,
    label = paste0("Mean = ", round(mean(boot_samples), 2)),
    color = "orangered",
    size = 3
  ) +
  geom_vline(
    xintercept = quantile(boot_samples, c(0.025, 0.975)),
    linetype = "dashed",
    color = "sienna3"
  ) +
  labs(
    title = "Bootstrap Distribution of Rank Var",
    x = "Mean Rank",
    y = "Frequency"
  ) +
  theme_minimal()

# Why is this important? The standard error and the sampling distribution are closely related, but we can estimate the standard error empirically rather than drawing 10,000 bootstrap samples.
sd(boot_samples)
sd(rank_2024$rank) / sqrt(samp_size)
rank_t_test$conf.int[1]
mean(boot_samples) - 1.96 * sd(boot_samples)
rank_t_test$conf.int[2]
mean(boot_samples) + 1.96 * sd(boot_samples)

# Now let's get the mean and 95% confidence interval for the rank variable for each year in the GSS. This is very easy using the t.test() function.
rank_years <- gss_which_years(gss_all, rank) |>
  filter(rank == TRUE) |>
  pull(year)
rank_results <- tibble()
for (i in rank_years) {
  df <- filter(gss_all, year == i)
  t_test_result <- t.test(df$rank, mu = 5) # mu doesn't matter for this exercise
  year_summary <- tibble(
    year = i,
    mean = t_test_result$estimate,
    ci_lower = t_test_result$conf.int[1],
    ci_upper = t_test_result$conf.int[2]
  )
  rank_results <- bind_rows(rank_results, year_summary)
}

ggplot(data = rank_results, aes(x = year, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "sienna3") +
  labs(
    title = "Mean Rank by Year with 95% Confidence Intervals",
    x = "Year",
    y = "Mean Rank"
  ) +
  theme_minimal()

# We can also do t-tests using proportions, which is often useful when working with polling data since most responses are categorical. (You will need to convert your categorical variable to a binary variable first.)

# Let's look at the "conpress" question.
?conpress

# Are a majority of Americans in 2024 confident in the press (i.e., do they respond "a great deal" or "only some")?

gss_2024 <- gss_all |>
  filter(year == 2024) |>
  mutate(
    conpress_binary = case_when(conpress %in% c(1, 2) ~ 1, conpress == 3 ~ 0)
  )

ttest_result <- t.test(gss_2024$conpress_binary, mu = 0.5)
ttest_result

# However, there is no way to account for survey *weights* in a t.test() function, so we need to use some external packages to do so. The `survey` and `srvyr` packages are very useful for this purpose.
install.packages(c("survey", "srvyr"))
library(survey)
library(srvyr)

# The first thing we need to do is create a survey design object. This object tells R how to account for the survey weights when calculating statistics.
gss_design <- as_survey_design(
  filter(gss_2024, !is.na(conpress_binary), !is.na(wtssnrps)),
  weights = wtssnrps
)
svyttest_result <- svyttest(conpress_binary - 0.5 ~ 0, design = gss_design)
svyttest_result$estimate + 0.5
ttest_result$estimate
