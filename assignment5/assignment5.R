# ==========================================
# Assignment 5 – Part 2: Teaching Evaluations
# ==========================================

# Libraries
library(haven)
library(dplyr)
library(ggplot2)
library(modelsummary)
library(fixest)
library(plm)

# ------------------------------------------
# 1. Load the data
# ------------------------------------------

df <- df <- read_dta("C:/Users/Usuario/Downloads/teaching_evals.dta")

# ------------------------------------------
# 2.1 Data exploration
# ------------------------------------------

# Number of unique instructors
n_instructors <- n_distinct(df$InstrID)

# Number of unique courses
n_courses <- n_distinct(df$CourseID)

n_instructors
n_courses

# Average number of observations per instructor
avg_obs_instr <- df %>%
  group_by(InstrID) %>%
  summarise(n = n()) %>%
  summarise(mean(n))

avg_obs_instr

# This looks like a short pannel given the number of instructors and the number
# of observations per instructor

# Scatter plot: evaluations vs % of A grades
scatter1 <- ggplot(df, aes(x = Apct, y = Eval)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(
    x = "Percent of A grades",
    y = "Course Evaluation",
    title = "Relationship between grading generosity and evaluations"
  )
ggsave("scatter1.png", plot = scatter1)

# ------------------------------------------
# 2.2 Pooled OLS baseline
# ------------------------------------------

m1 <- lm(Eval ~ Apct + Enrollment + Required, data = df)

summary(m1)

modelsummary(m1)

# A 1% increase in the share of A grades is associated with an increase of 0.36 
# points in teachers' evaluations.

# How able the teacher is to correctly explain the subject matter, or how
# friendly overall the teacher is might impact both things at the same time, 
# thus creating upwards bias.

# ------------------------------------------
# 2.3 Fixed Effects Models
# ------------------------------------------

# Instructor fixed effects
m_instr <- feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df)

# Two-way fixed effects (Instructor + Year)
m_twfe <- feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df)

# Compare models with clustered SEs
modelsummary(
  list(
    "Pooled OLS" = m1,
    "Instructor FE" = m_instr,
    "Two-Way FE" = m_twfe
  ),
  vcov = ~InstrID,
  stars = TRUE,
  gof_map = c("r.squared", "nobs")
)

# Within the same instructor, a one percentage point increase in the share of 
# A grades is correlateed with an increase of 0.3 points in evaluations. The 
# fixed effect controls for all the characteristics of each instructor by making
# the model compare courses taught by the same instructor over time. The coef 
# being lower than in the pooled OLS means that the pooled OLS coefficient was 
# probably biased upwards, indicating instructors who give more A grades also 
# have other characteristics that lead to higher evaluations. 

# ------------------------------------------
# 2.4 Random Effects and Hausman Test
# ------------------------------------------

# Convert to panel data
pdata <- pdata.frame(df, index = c("InstrID", "CourseID"))

# Random effects model
m_re <- plm(
  Eval ~ Apct + Enrollment + Required,
  data = pdata,
  model = "random"
)

summary(m_re)

# Fixed effects model (plm version)
m_fe_plm <- plm(
  Eval ~ Apct + Enrollment + Required,
  data = pdata,
  model = "within"
)

# Hausman test
hausman_test <- phtest(m_fe_plm, m_re)

hausman_test

# The null hypothesis of the hausman test is that the random effects estimator
# is consistent and the difference between both models is not systematic. In this
# case, the hausman test fails to reject the null hypothesis, which means that 
# for this dataset one is actually better off using the random effects model 
# since it is more efficient, given that the fixed effects model is actually not
# shown to fix any of the possible upward bias. 