# ============================================================
# Assignment 3 – Part 2: STAR (High School Graduation)
# Applied Quantitative Methods II
# ============================================================


# ============================================================
# 0. Setup
# ============================================================

library(tidyverse)
library(broom)
library(marginaleffects)
library(modelsummary)


# ============================================================
# 2.1 Data Preparation
# ============================================================

rawdata <- read_csv("C:/Users/Usuario/Downloads/star.csv")

df <- rawdata %>%
  mutate(
    classtype = factor(classtype,
                       levels = c(1, 2, 3),
                       labels = c("Small", "Regular", "Regular+Aide")),
    
    race = factor(race,
                  levels = c(1, 2, 3, 4, 5, 6),
                  labels = c("White", "Black", "Asian",
                             "Hispanic", "Native American", "Other")),
    
    small = if_else(classtype == "Small", 1, 0)
  )

df <- df %>%
  filter(!is.na(hsgrad))

nrow(df)

mean(df$hsgrad)

df %>%
  group_by(classtype) %>%
  summarise(grad_rate = mean(hsgrad, na.rm = TRUE))

# Graduation rates barely differ on average between types of classes, but in any 
# case the graduation ratio in small classes isn't higher than in Regular+Aide 
# ones


# ============================================================
# 2.2 LPM and Logit (Bivariate Models)
# ============================================================

linear1 <- lm(hsgrad ~ small, data = df)

logit1 <- glm(hsgrad ~ small,
              family = binomial,
              data = df)

tidy(linear1)
tidy(logit1)

# Students in a small class have an increased probability of being graduated by
# about 0.338 percentage points when compared to students in regular classes

avg_slopes(logit1)

# The coefficient from the logit model is the same as the one from the linear
# model once the AME is extracted using avg_slopes


# ============================================================
# 2.3 Adding Controls
# ============================================================

linear2 <- lm(hsgrad ~ small + race + yearssmall, data = df)

logit2 <- glm(hsgrad ~ small + race + yearssmall,
              family = binomial,
              data = df)

tidy(linear2)
tidy(logit2)

# The coefficient is now negative and statistically significant, which indicates
# the lack of full success in the randomisation process

avg_slopes(logit2, variables = "yearssmall")

# This coefficient, which is statistically significant, means that one extra year
# of having been in a small class increases your probability of graduating by 
# 2.8 percentage points on average


# ============================================================
# 2.4 Predicted Probabilities
# ============================================================

# I have been trying to fix an error which I do not understand which does not 
# allow me to use predictions for some reason but I have been unable to fix it,
# I will inquire about this in class to see how it may be fixed. Here is the
# code that is giving me the error:

newdat <- data.frame(
  race = factor(c("White", "Black"), levels = levels(df$race)),
  small = c(1, 0),
  yearssmall = c(3, 0)
)

predictions(logit2, newdata = newdat)

# ============================================================
# 2.5 Interactions
# ============================================================

logit3 <- glm(hsgrad ~ small * race + yearssmall,
              family = binomial,
              data = df)

avg_slopes(logit3,
           variables = "small",
           by = "race")

# The effects are only statistically significant for white and black students, 
# for which the effect of being in a small class on the probability of graduating
# is reduced by 7 to 10 percentage points


# ============================================================
# 2.6 Presenting Results
# ============================================================

# Table comparing all four models
modelsummary(
  list(
    "LPM (bivariate)" = lpm1,
    "LPM (controlled)" = lpm2,
    "Logit (bivariate)" = logit1,
    "Logit (controlled)" = logit2
  ),
  vcov = list("robust", "robust", NULL, NULL),
  stars = TRUE
)

# Coefficient plot
coef_plot <- modelplot(
  list(
    "LPM (bivariate)" = lpm1,
    "LPM (controlled)" = lpm2,
    "Logit (bivariate)" = logit1,
    "Logit (controlled)" = logit2
  )
)

ggsave("coefficient_plot.png",
       plot = coef_plot,
       width = 7,
       height = 5)

# The experimental evidence is more credible than an observational study because
# it is closer to ideal randomisation given the randomisation of treatment, and 
# because the researchers are able to isolate the causal effects by controlling 
# for specific variables. The database suggests that the effect of small classes
# on the likelihood of graduating is not too significant, although the impact
# of one added year of presence in a small class is way higher, suggesting
# exactly the same dynamic as we explored in the last assignment. The LPM and 
# logit results tell mostly a similar story in terms of coefficients, although
# the implications of both models are always going to be different and we will
# have to choose depending on the outcomes we are interested in given the 
# different statistical structures of both models.