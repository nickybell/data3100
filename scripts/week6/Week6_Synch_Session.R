# File name: Week6_Synch_Session.R
# Description: This script contains code for the Week 6 synchronous session with a focus on ordinary least squares regression
# Date created: 2024-09-30
# Author: Nicholas Bell (belln@sas.upenn.edu)

# Today, we will focus on how to conduct an OLS regression in R and report the results using the {stargazer} package. We will also review an example of the linear probability model, which is a special case of OLS regression when we have a binary dependent variable.

# To start, let's load the necessary packages and data.
library(tidyverse)
library(stargazer)
options(scipen = 999) # Turn off scientific notation for numbers

# The data we will be working with includes (almost) all U.S. hurricane landfalls since 1996 and the associated human and economic impacts of natural disasters. The hurricane data comes from the National Hurricane Center's HURDAT2 dataset, which is publicly available online. The impact data comes from the New York Fed's website and is also publicly available.
hurr <- read_csv(here::here("data/week6/hurricane_landfalls.csv"))

# As a quick first cut at the data, let's look at how the category of the hurricane relates to the total property damage caused by the hurricane.
reg1 <- lm(total_property_damage_millions ~ category, data = hurr)
summary(reg1)

# Whoa! If the category of a hurricane goes up by 1 (category ranges from 1-5 in increasing intensity), then the predicted total property damage goes up by 1.6 billion dollars! This is a very large effect, but it makes sense that stronger hurricanes cause more damage. However, we should be cautious about interpreting this result as causal. There are many other factors that could influence the total property damage caused by a hurricane, such as the location of landfall, population density, and preparedness measures in place.

# We don't have a direct measure of preparedness, but we do have the date of landfall. Over time, our ability to predict hurricane landfall timing, location, and intensity has increased, so presumably people have more time to prepare. Let's create a new variable that measures the number of years since 1996 that the hurricane made landfall.

# Why not just use the year of landfall? Think about our linear regression model:
# Y = β0 + β1X1 + β2X2 + ε
# If we use year of landfall, then the intercept (β0) would be the predicted total property damage in year 0 (i.e., 0 AD), which is not very meaningful. By using years since 1980, the intercept becomes the predicted total property damage in 1996, which is more interpretable.

library(lubridate)
hurr2 <- hurr |>
  mutate(years_since_1996 = year(date) - 1996)

reg2 <- lm(
  total_property_damage_millions ~ category + years_since_1996,
  data = hurr2
)
summary(reg2)

# The coefficient on years_since_1996 is negative, which suggests that over time, the total property damage caused by hurricanes has decreased, holding the category of the hurricane constant. However, the coefficient is not statistically significant at the 5% level, so we cannot reject the null hypothesis that there is no relationship between time and total property damage.

# Maybe there's not much we can do to prevent property damage (lots of houses were built before 1980, for example), but over time we should be able to do more to prevent fatalities. Let's look at how the category of the hurricane relates to the total number of fatalities caused by the hurricane.
reg3 <- lm(total_fatalities ~ category + years_since_1996, data = hurr2)
summary(reg3)

# Once again, our coefficient on years since 1996 is not statistically significant, so we cannot reject the null hypothesis that there is no relationship between advances in preparedness and total fatalities.

# What if we try a *linear probability model* instead? A linear probability model is just an OLS regression where the dependent variable is binary (0 or 1). In this case, let's create a new variable that indicates whether or not the hurricane caused any fatalities. Let's also substitute property damage for category as a measure of intensity, since property damage is a continuous variable and may be a more precise measure of intensity.
hurr3 <- hurr2 |>
  mutate(fatalities_binary = if_else(total_fatalities > 0, 1, 0))
reg4 <- lm(
  fatalities_binary ~ total_property_damage_millions + years_since_1996,
  data = hurr3
)
summary(reg4)

# Interesting! The coefficient on years since 1996 is still not statistically significant, which is a troubling finding. Even though we are better at predicting hurricanes and have more advanced warning systems, we are not seeing a statistically significant reduction in the probability of fatalities from hurricanes over time. Perhaps we need to control for more confounders, such as population density or socioeconomic status of the affected areas, but we don't have that data available in this dataset.

# But let's also look at the coefficient on total property damage. A one million dollar increase in property damage is associated with a 0.006 percentage point (not percent) increase in the probability of fatalities, holding year constant. This is a small effect, but it might matter a LOT if we compare the least damaging hurricane to the most damaging hurricane.
(max(hurr3$total_property_damage_millions) -
  min(hurr3$total_property_damage_millions)) *
  reg4$coefficients[2]

# Between the least and most damaging hurricanes, the predicted probability of fatalities increases by 0.7, or 70 percentage points!

# But of course, we need to control for our confounders in this analysis as well. We can do this with the predict() function. The predict() function allows us to create a new data frame with predicted values from our regression model, holding certain variables constant.
predict(
  reg4,
  newdata = data.frame(
    total_property_damage_millions = c(
      min(hurr3$total_property_damage_millions),
      max(hurr3$total_property_damage_millions)
    ),
    years_since_1996 = mean(hurr3$years_since_1996)
  )
)

# There's the 70 percentage point increase again! But something a bit odd is happening here. The predicted probability of fatalities for the most damaging hurricane is greater than 1, meaning greater than 100%. This is a limitation of the linear probability model - it can produce predicted probabilities that are less than 0 or greater than 1. This is one reason why logistic regression is often preferred for binary dependent variables, but we won't cover that in this class.

# Finally, let's use the {stargazer} package to create a nice table of our regression results.
library(stargazer)
stargazer(
  reg1,
  reg2,
  reg3,
  reg4,
  type = "text",
  title = "Regression Results",
  dep.var.labels = c(
    "Property Damage (Millions)",
    "Total Fatalities",
    "Probability of Fatalities"
  ),
  covariate.labels = c(
    "Category",
    "Property Damage (Millions)",
    "Years Since 1996"
  ),
  keep.stat = c("n", "adj.rsq")
)

# When doing this in an RMarkdown file, we need to specify type = "html" or type = "latex" for HTML and PDF outputs, respectively, to get the table to display in the knitted document. We also need to provide a special output option to the code chunk.

# ```{r results = "asis"}
# stargazer(
#   reg1,
#   reg2,
#   reg3,
#   reg4,
#   type = "text",
#   title = "Regression Results",
#   dep.var.labels = c(
#     "Property Damage (Millions)",
#     "Total Fatalities",
#     "Probability of Fatalities"
#   ),
#   covariate.labels = c(
#     "Category",
#     "Property Damage (Millions)",
#     "Years Since 1996"
#   ),
#   keep.stat = c("n", "adj.rsq")
# )
# ```

# Also, please note that stargazer can be a bit finicky, especially with the order of dependent variable and covariate labels. Always confirm your output table looks correct before including it in a report or paper. (My honest opinion is that there is no good way to report regression results in R, but stargazer is probably the least bad option.)
