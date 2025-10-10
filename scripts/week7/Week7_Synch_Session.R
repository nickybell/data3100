# File name: Week7_Synch_Session.R
# Description: This script contains code for the Week 7 synchronous session with a focus on ordinary least squares regression
# Date created: 2024-10-09
# Author: Nicholas Bell (belln@sas.upenn.edu)

# Today, we are going to continue our discussion of ordinary least squares regression by extending our analysis to include two new elements: (1) categorical independent variables with a reference category, and (2) interaction terms.

# We are going to be working with data from an ABC News/Washington Post poll conducted in 2019. You may recall that from December 2018 - January 2019, the U.S. federal government was partially shut down for a record 35 days. The poll asked respondents about their opinions on the shutdown and its effects on them personally. We will use this data to get an idea of how the politics of the current government shutdown might play out (the politics being secondary to the human impacts, of course, but I'm a political scientist).

# To start, let's load the necessary packages and data.
library(tidyverse)
library(stargazer)
options(scipen = 999) # Turn off scientific notation for numbers

# And load our data
poll <- read_csv(here::here("data/week7/shutdown_poll.csv"))

# Let's estimate the effect of political party on blaming "Trump and the Republicans in Congress" for the shutdown, controlling for our other demographic variables. For this linear *probability* model, if we want to ensure that our dependent variable is measuring our proportion of interest, it is best practice to recode the variable to be 0/1 or TRUE/FALSE. Let's do that now.
poll <- poll |>
  mutate(
    shutdown_blame_binary = if_else(
      shutdown_blame == "Trump and the Republicans in Congress",
      1,
      0
    )
  )

# Now we can estimate our model.
reg1 <- lm(
  shutdown_blame_binary ~ pid + region + gender + age + educ + income,
  data = poll,
  weights = weight
)
summary(reg1)

# Our regression only contains estimates of the coefficient (slope) for Independents and Republicans. What happened to Democrats?
# Democrats are what is known as the reference category. There are two things you should know about regression to understand reference categories:
# 1. All of our variables must be numeric. Therefore, R automatically converts categorical variables into a series of binary (0/1) dummy/dicohotomous variables. (This is called one-hot encoding in machine learning, for some reason.)
# 2. One of the assumptions of regression is that there is no perfect correlation between independent variables. If we included a dummy variable for every category of a categorical variable, then the sum of those dummy variables would equal 1 for every observation, which is perfect correlation. Therefore, R automatically omits one category from the regression. This omitted category is the reference category.

# The regression coefficients for a categorical variable are interpreted a little bit differently than a continuous variable. The coefficient for a categorical variable represents the difference in the predicted value of the dependent variable between that category and the reference category, holding all other independent variables constant.

# If we want to change the reference level, we can use the relevel() function. Let's change the reference level for pid to "Independent" instead of "Democrat".
reg2 <- lm(
  shutdown_blame_binary ~
    relevel(factor(pid), ref = "Independent") +
      region +
      gender +
      age +
      educ +
      income,
  data = poll,
  weights = weight
)
summary(reg2)

# Notice that the regression coefficient for Democrats is the inverse of the coefficient for Independents in the previous regression. Why?
# But the regression coefficient for Republicans is not the same as before. Why not?

# If we are using {stargazer} to report our regression results, it is good practice to indicate the reference category in the table.
stargazer(
  reg2,
  type = "text",
  dep.var.labels = "Blame Republicans for Shutdown",
  covariate.labels = c(
    "Democrat (ref: Independent)",
    "Republican",
    "Northeast (ref: Midwest)",
    "South",
    "West",
    "Male",
    "Age",
    "HS or less (ref: College graduate)",
    "Post-Grad",
    "Some College"
  ),
  keep.stat = c("n", "adj.rsq")
)

# Now, we might think that the effect of political party might be moderated (changed) by another variable. For example, maybe the effect of political party is different depending on whether the respondent has been personally inconvenienced by the shutdown. We can test this hypothesis by including an interaction term in our regression.

# x -----> y
#     ^
#     |
#     |
#     z

# In this diagram, x is our independent variable (political party), y is our dependent variable (blaming Trump and the Republicans), and z is our moderator (personal inconvenience). The arrow from z to the arrow from x to y indicates that the effect of x on y is moderated (changed) by z.

# To estimate this in a regression equation, we simply multiple the independent variable by the moderator. For simplicity, I'm going to remove our other control variables for now.
reg3 <- lm(
  shutdown_blame_binary ~ pid * shutdown_inconv,
  data = poll,
  weights = weight
)
summary(reg3)

stargazer(
  reg3,
  type = "text",
  dep.var.labels = "Blame Republicans for Shutdown",
  covariate.labels = c(
    "Independent (ref: Democrat)",
    "Republican",
    "Personally Inconvenienced by Shutdown",
    "Independent x Personally Inconvenienced",
    "Republican x Personally Inconvenienced"
  ),
  keep.stat = c("n", "adj.rsq")
)

# The interpretation of the coefficients in a regression with an interaction term is a bit more complicated.

# Y = β0 + β1X1 + β2X2 + β3(X1*X2) + ε
# Y = β0 + β1(pid) + β2(shutdown_inconv) + β3(pid*shutdown_inconv) + ε
# So for a Republican who is inconvenienced by the shutdown:
# Y = 0.94336 + -0.76004*(1) + -0.01138*(1) + 0.14294*(1*1) + ε
# Y =

# And for a Democrat who is inconvenienced by the shutdown:
# Y = 0.94336 + -0.76004*(0) + -0.01138*(1) + 0.14294*(0*1) + ε
# Y =

# Importantly, the coefficient on the main terms (pid and shutdown_inconv) are no longer interpretable on their own. The effect of pid on the dependent variable depends on the value of shutdown_inconv, and vice versa. To understand the effect of pid on the dependent variable, we need to consider both the coefficient for pid and the coefficient for the interaction term.

# So what if we still want to know the independent effect pid, regardless of whether or not the respondent was personally inconvenienced by the shutdown? We can use the predict() function to estimate the predicted values of the dependent variable for different combinations of the independent variable and moderator, holding all other variables constant.

predict(
  reg3,
  newdata = data.frame(
    pid = c("Democrat", "Independent", "Republican"),
    shutdown_inconv = c("No", "No", "No")
  )
)

predict(
  reg3,
  newdata = data.frame(
    pid = c("Democrat", "Independent", "Republican"),
    shutdown_inconv = c("Yes", "Yes", "Yes")
  )
)

# There is a phenomenal package called {marginaleffects} that makes this process much easier (e.g., you can get confidence intervals around the marginal effects), but we won't cover it in this class. If you are interested, I highly recommend checking it out on your own.
