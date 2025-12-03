## GLM Example: Logistic regression
## Goal: Model the relationship between inbreeding and survival (Teenagers/Pregnancies)
##       using a binomial GLM (logistic regression).

# load libraries ----------------------------------------------------------

library(ggplot2)   # plotting
library(broom)     # tidying model output (not used below, but often handy)
library(emmeans)   # estimated marginal means and trends
library(readr)     # reading data (read_csv)
library(dplyr)     # data wrangling (mutate, pipes)
library(ggrepel)   # nicer non-overlapping text labels for points

# load data ---------------------------------------------------------------

palace_inbreeding <- read_csv(
  "https://raw.githubusercontent.com/ybrandvain/datasets/master/inbreeding.csv"
)
# Data contain: inbreeding coefficient, number of pregnancies, number of teenagers
# who survive, and the King_Queen label for each royal couple.

# transform data ----------------------------------------------------------

palace_inbreeding <- palace_inbreeding |>
  mutate(
    # proportion of pregnancies that survive to teenage years
    prop_survived = Teenagers / Pregnancies
  )

# data visualization ------------------------------------------------------
# Plot the relationship between inbreeding and survival,
# fitting a logistic regression curve with weights.

ggplot(
  palace_inbreeding, 
  aes(
    x      = inbreeding, 
    y      = prop_survived,
    weight = Pregnancies  # tells geom_smooth how many trials each point represents
  )
) +
  # Fit a binomial GLM (logistic regression) smooth through the data.
  # method.args = list("binomial") means: use family = "binomial".
  geom_smooth(method = "glm", method.args = list("binomial")) +
  
  # Label each point by King_Queen, with labels pushed apart to avoid overlap.
  geom_label_repel(aes(label = King_Queen), alpha = 0.7) +
  
  # Points sized by number of pregnancies (more pregnancies = bigger point).
  geom_point(aes(size = Pregnancies)) +
  
  # Light, minimalist theme.
  theme_light()

# model fitting -----------------------------------------------------------
# Fit a logistic regression model:
# response: proportion surviving (Teenagers / Pregnancies)
# predictor: inbreeding
# weights: number of trials (Pregnancies)
# family = "binomial": logistic regression with logit link by default.

this_glm <- glm(
  prop_survived ~ inbreeding, 
  data    = palace_inbreeding, 
  weights = Pregnancies,  # each row represents this many Bernoulli trials
  family  = "binomial"
)

# look at parameter estimates ---------------------------------------------
# Shows the estimated intercept and slope on the logit (log-odds) scale,
# plus standard errors, z-values, and p-values.

this_glm

# describe uncertainty in slope -------------------------------------------
# emtrends() estimates the trend (slope) of the response with respect to
# "inbreeding", along with its standard error and confidence interval.
# Here, ~1 means "overall slope" (no grouping factor).

emtrends(this_glm, ~ 1, var = "inbreeding")

# uncertainty in estimated means for different inbreeding levels ----------

# Get estimated mean response at a grid of inbreeding values from 0 to 0.25
# in steps of 0.05. By default, emmeans returns results on the model scale
# (here: logit scale).

emmeans(
  this_glm,
  ~ inbreeding,
  at = list(inbreeding = seq(0, 0.25, 0.05))
)  # on logit (linear predictor) scale

# Same thing, but type = "response" transforms predictions back to the
# original scale: proportion surviving (0 to 1), with CIs on that scale.

emmeans(
  this_glm,
  ~ inbreeding,
  at   = list(inbreeding = seq(0, 0.25, 0.05)),
  type = "response"
)  # on probability (response) scale

# NHST: test overall effect of inbreeding ---------------------------------
# anova(..., test = "LRT") compares this model to a reduced model
# (intercept-only) using a likelihood ratio test.
# Null hypothesis: inbreeding has no effect on survival (slope = 0).

anova(this_glm, test = "LRT")
