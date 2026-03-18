# PART 1 

library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)

df <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/minwage.csv")

df = df %>% mutate(NJ = ifelse(location != "PA", 1, 0))
table(df$NJ)

df %>%
  group_by(NJ) %>%
  summarise(
    mean_wage_before = mean(wageBefore, na.rm = TRUE),
    mean_wage_after = mean(wageAfter, na.rm = TRUE))

means = df %>%
  group_by(NJ) %>%
  summarise(
    before = mean(fullBefore, na.rm = TRUE),
    after = mean(fullAfter, na.rm = TRUE),
    change = after - before)
means

nj_change = means$change[means$NJ == 1]
pa_change = means$change[means$NJ == 0]
did_est = nj_change - pa_change
cat("DiD estimate:", round(did_est, 3), "\n")

df_long = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(fullBefore, fullAfter),
    names_to = "period",
    values_to = "full_emp") %>%
  mutate(
    post = ifelse(period == "fullAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))
nrow(df_long)

nrow(df)

m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
modelsummary(m_did, stars = TRUE, gof_map = c("nobs", "r.squared"),
             output = "markdown")

m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
modelsummary(
  list("DiD" = m_did, "DiD + Chain FE" = m_did_fe),
  stars = TRUE, gof_map = c("nobs", "r.squared"),
  output = "markdown")

df_long_wage = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(wageBefore, wageAfter),
    names_to = "period",
    values_to = "wage") %>%
  mutate(
    post = ifelse(period == "wageAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))
m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)
modelsummary(m_wage, stars = TRUE, gof_map = c("nobs", "r.squared"),
             output = "markdown")

#-----------------------------------------------------------------------

############################################################
# Part 2: Staggered DiD
############################################################

# Load packages
library(did)
library(dplyr)
library(ggplot2)
library(fixest)

# Load data
data(mpdta)

############################################################
# 2.1 Data structure and visualization
############################################################

# (a) Number of counties
length(unique(mpdta$countyreal))

# Number of treatment cohorts
length(unique(mpdta$first.treat))

# Distribution of cohorts
table(mpdta$first.treat)

# COMMENT:
# Staggered treatment adoption means that different counties undergo treatment 
# at different points in time. Comparing treated and untreated counties might
# be problematic because countries that have already undergone treatment serve
# as control for those who are treated later, which is potentially problematic
# and can bias estimates.


# (b) Plot average lemp by cohort over time
mpdta_avg = mpdta %>%
  mutate(cohort = factor(first.treat,
                         levels = c(0, 2004, 2006, 2007),
                         labels = c("Never treated", "Adopted 2004",
                                    "Adopted 2006", "Adopted 2007"))) %>%
  group_by(year, cohort) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE), .groups = "drop")

ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")

# Save plot
ggsave("mpdta_cohort_trends.png", width = 7, height = 5)

# COMMENT:
# Pre-treatment trends seem to be relatively graphically similar in all cohorts,
# with the cohort that got treated in 2004 exhibiting a different path to the 
# rest, and the cohort that got treated in 2006 exhibiting a similar initial 
# response to that of the units treated earlier. 


############################################################
# 2.2 Naive TWFE vs Callaway-Sant'Anna
############################################################

# (a) TWFE model
mpdta = mpdta %>%
  mutate(treated_post = as.integer(first.treat > 0 & year >= first.treat))

m_twfe = feols(lemp ~ treated_post | countyreal + year,
               data = mpdta,
               cluster = ~countyreal)

summary(m_twfe)

# COMMENT:
# The coefficient on treated_post is statistically significant and suggests a 
# negative relationship between treatment and log teen employment. However, 
# the model inherently assumes, given its pooled nature, that the effect across
# time is homogeneous for all groups, which is problematic in a staggered 
# adoption setting for the reasons explained before. 


# (b) Callaway-Sant'Anna estimator
cs_out = att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~ lpop,
  data = mpdta,
  control_group = "nevertreated"
)

# Aggregate ATT
agg_simple = aggte(cs_out, type = "simple")
summary(agg_simple)

# COMMENT:
# The Callaway-Sant'Anna estimate is similar but a little larger, showing upwards
# bias of the pooled model, yet not a very significant one. 

# (c) Event-study (dynamic effects)
cs_dyn = aggte(cs_out, type = "dynamic")

ggdid(cs_dyn)

ggsave("mpdta_event_study_nevertreated.png", width = 7, height = 5)

# COMMENT:
# The fact that the pre-treatment estimates aren't statistically different from 
# zero shows that the parallel trends assumption is not violated. The post-treat
# ment estimates indicate a negative impact of treatment that seems to persist
# throughout time. 


############################################################
# 2.3 Pre-testing parallel trends
############################################################

cs_out_bt = att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~ lpop,
  data = mpdta,
  control_group = "nevertreated",
  bstrap = TRUE,
  cband = TRUE
)

summary(cs_out_bt)

# COMMENT:
# Given the large p-value, we fail to reject the null hypothesis, which in this
# case is that there are no systematic differences in trends between treated
# and control groups before treatment, thus not violating the parallel trends
#assumption.


# Plot all ATT(g,t)
ggdid(cs_out_bt)

ggsave("mpdta_att_gt.pdf", width = 10, height = 6)

# COMMENT:
# Pre-treatment values are indistinguishable from zero across all cohorts, yet 
# the value for 2004 of the group treated in 2007 is almost distinguishable from 
# zero. It is also important to note that only those values for post-treatment 
# effects that are distinguishable from zero are only those corresponding to the
# group treated in 2004. 


# COMMENT:
# Pre-trends testing tells us that trends were similar before treatment, but it 
# cannot rule out the possibility that trends would have differed regardless after
# treatment in the absence of it. As such, although it offers evidence for the 
# parallel trends assumption, it does not outright prove that it holds true.


############################################################
# 2.4 Different control groups
############################################################

# (a) Not-yet-treated control group
cs_out_nyt = att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~ lpop,
  data = mpdta,
  control_group = "notyettreated"
)

agg_nyt = aggte(cs_out_nyt, type = "simple")
summary(agg_nyt)

# COMMENT:
# Both estimates are very similar, suggesting results are robust to the choice
# of control group 


# (b) Event study
cs_dyn_nyt = aggte(cs_out_nyt, type = "dynamic")

ggdid(cs_dyn_nyt)

ggsave("mpdta_event_study_nyt.pdf", width = 7, height = 4)

# COMMENT:
# The conclusions do not change significantly, for the pre-treatment estimates
# are still statistically indistinguishable from zero, althought the trend 
#suggested by such estimates differs slightly. 


# (c) COMMENT:
# Using not-yet-treated units might be preferable when we necessitate a bigger
# control group or want higher precision, 
# but using never-treated units gets rid of the anticipation 
# assumption, thus being preferable when we suspect that units might change their
# behaviour in anticipation of receiving the treatment. 


############################################################
# 2.5 Why TWFE fails
############################################################

# COMMENT:
# As we have explained before, the naive TWFE can be problematic because it uses
# units that have been treated as control for units that are treated later, which
# is not ideal if the effects are heterogeneous across time and amongst groups,
# invalidating the control group.



# COMMENT:
# # The TWFE estimate and the Callaway-Sant’Anna one are similar in sign and 
# magnitude. However, based on the event-study results showing that
# pre-treatment trends are not significantly different from zero, the CS
# estimate is more credible. This is because it accounts for staggered treatment  
# and avoids the bias from using already-treated units as controls, making it more 
# reliable.