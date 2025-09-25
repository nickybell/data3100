# File name: Week5_Synch_Session.R
# Description: This script contains code for the Week 5 synchronous session with a focus on two-sample hypothesis testing
# Date created: 2024-09-25
# Author: Nicholas Bell (belln@sas.upenn.edu)

# I want to start by discussing the role of confidence intervals and statistical significance. I think it is intuitive that if a 95% confidence interval does not include the value we are comparing against (which is a two-sample t-test, is often that the difference in means is zero), then we can reject the null hypothesis and accept the alternative hypothesis of difference. There is a very small chance (<=5%) that the true difference is zero, and we committed a Type I error (false positive) due to random sampling error.

# But what if we are comparing two differences in means?
set.seed(20912)
var1 <- rnorm(100, mean = 100, sd = 10)
var2 <- rnorm(100, mean = 103, sd = 10)

# Now calculate the confidence intervals of the mean. We could do this by hand or get it quite easily from the t.test() function.
var1_ci <- t.test(var1)$conf.int
var2_ci <- t.test(var2)$conf.int

ggplot() +
  geom_point(
    data = tibble(var = c("var1", "var2"), mean = c(mean(var1), mean(var2))),
    aes(x = var, y = mean)
  ) +
  geom_errorbar(
    data = tibble(
      var = c("var1", "var2"),
      ci_lower = c(var1_ci[1], var2_ci[1]),
      ci_upper = c(var1_ci[2], var2_ci[2])
    ),
    aes(x = var, ymin = ci_lower, ymax = ci_upper),
    width = 0.2
  )

# These confidence intervals overlap. But are the means significantly different from one another?
t.test(var1, var2)

# Here is the rule of thumb: if the confidence intervals do not overlap, then the means are significantly different from one another. If they do overlap, then you cannot say for sure that they are not significantly different from one another.

# Let's continue working with data from the General Social Survey (GSS), which as you'll recall, is a nationally representative survey of adults in the United States that has been conducted since 1972.

library(tidyverse)
library(gssr)
library(gssrdoc)
data(gss_all) # This may take a moment
data(gss_dict)

# One of the defining features of the current era of electoral politics is that the traditional defining lines between the parties -- such as income -- have either disappeared or reversed. Let's explore one potential source of contemporary polarization, which is occupational prestige. Occupational prestige is a measure of the social status of different occupations, and is also a measure of where one stands in the American economy and social hierarchy.

prestige_years <- gss_all |>
  filter(!is.na(prestg10)) |>
  distinct(year) |>
  pull(year)

ttest_results <- tibble()
for (i in prestige_years) {
  print(i)
  df <- filter(gss_all, year == i & !is.na(prestg10)) |>
    mutate(
      pid = case_when(
        partyid %in% c(0, 1, 2) ~ "Democrat",
        partyid %in% c(4, 5, 6) ~ "Republican",
        TRUE ~ NA
      )
    )

  res <- t.test(prestg10 ~ pid, data = df)

  res_tibble <- tibble(
    year = i,
    mean_dem = res$estimate[1],
    mean_rep = res$estimate[2],
    mean_diff = mean_dem - mean_rep,
    se_diff = res$stderr,
    ci_lower = res$conf.int[1],
    ci_upper = res$conf.int[2]
  )
  ttest_results <- bind_rows(
    ttest_results,
    res_tibble
  )
}

ggplot(data = ttest_results, aes(x = year, y = mean_diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "sienna3") +
  labs(
    title = "Difference in Mean Occupational Prestige by Party ID",
    x = "Year",
    y = "Mean Difference (Democrat - Republican)"
  ) +
  theme_minimal()

# It looks like there is a significant change in polarization by occupational prestige between 2022 and 2012 (the last election prior to the Trump presidency). But is this difference statistically significant?

# I'm now going to show you the single most important lesson I've learned about applied statistics in my career - how to compare two normally-distributed differences in means. We will apply this again when we work with linear regression.

# We can do the same procedure we did above with our simulated data, but instead of simulating with whatever mean and standard deviation we want, we can use the means and standard errors from our t-tests above.

# Why can we use the standard errors from our t-tests? Because the standard error of the difference in means is the dispersion of the sampling distribution of that difference. It is like standard deviation, but for an estimate rather than a random variable.

set.seed(20912)
diff_2012 <- rnorm(
  10000,
  mean = ttest_results$mean_diff[ttest_results$year == 2012],
  sd = ttest_results$se_diff[ttest_results$year == 2012]
)
diff_2022 <- rnorm(
  10000,
  mean = ttest_results$mean_diff[ttest_results$year == 2022],
  sd = ttest_results$se_diff[ttest_results$year == 2022]
)
diff_in_diff <- diff_2022 - diff_2012
quantile(diff_in_diff, c(0.025, 0.975))

# This confidence interval does not include zero, so we can conclude that the difference in occupational prestige between Democrats and Republicans changed significantly between 2012 and 2022.

# Lastly, I want to return to weighted t-tests using the survey package.

library(survey)
library(srvyr)

# The first thing we need to do is create a survey design object. This object tells R how to account for the survey weights when calculating statistics.
gss_2024 <- gss_all |>
  mutate(
    fepol_binary = case_when(fepol == 1 ~ 1, fepol == 2 ~ 0),
    pid = case_when(
      partyid %in% c(0, 1, 2) ~ "Democrat",
      partyid %in% c(4, 5, 6) ~ "Republican",
      TRUE ~ NA
    )
  ) |>
  filter(year == 2024, !is.na(fepol_binary), !is.na(pid), !is.na(wtssnrps))

gss_design <- gss_2024 |>
  as_survey_design(
    weights = wtssnrps
  )

svyttest(fepol_binary ~ pid, design = gss_design)
t.test(fepol_binary ~ pid, data = gss_2024)
