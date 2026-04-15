library(carData)
library(MASS)
library(nnet)
library(pscl)
library(AER)
library(marginaleffects)
library(ggplot2)
data(BEPS)

table(BEPS$economic.cond.national)

BEPS$econ_ord = factor(BEPS$economic.cond.national, ordered = TRUE)

# The distribution is concentrated in the middle categories (2, 3, and 4), 
# with category 3 (“stayed about the same”) being the modal response. Very few 
# respondents chose the extreme ends (1 = got much worse, 5 = got much better).
# OLStreats the numeric values 1–5 as equally spaced, implying that the difference
# between “got much worse” and “got a little worse” is identical to the difference
# between “stayed the same” and “got a little better.” This is almost certainly
# wrong for Likert-type survey items, where psychological distances between
# adjacent categories need not be equal. Ordered logit avoids this assumption
# by estimating threshold parameters that let the data determine the spacing of
# the latent scale.

m_ologit = polr(econ_ord ~ age + gender + Europe + political.knowledge,
                data = BEPS, Hess = TRUE)
summary(m_ologit)

# Note that polr() uses a reversed sign convention: the raw coefficient reported
# in the output is negated relative to the standard parameterization. A positive
# raw coefficient in the polr() output corresponds to a negative association
# with higher ordinal categories. The raw coefficient on Europe is negative, 
# which — after applying the sign reversal — implies a positive association: 
# respondents with stronger pro-EU attitudes tend to perceive national economic
# conditions as having improved. This is plausible: Blair’s government was
# broadly pro-European and pro-single-market, so EU supporters may have held 
# more favorable views of its economic stewardship. For reliable interpretation
# of magnitudes, we use average marginal effects below.

avg_slopes(m_ologit)

# The AMEs show the average change in the probability of each response category
# associated with a one-unit increase in each predictor. For Europe, the AMEs 
# on the lower categories (1 and 2) are negative, while the AMEs on the higher 
# categories (4 and 5) are positive, consistent with a positive association
# between pro-EU sentiment and more optimistic economic assessments. As a sanity 
# check, the AMEs for any given predictor must sum to zero across the five 
#categories because probabilities are constrained to sum to one.

predictions(m_ologit, newdata = datagrid(gender = c("female", "male")))

# The predicted probabilities for the most pessimistic category 
# (1 = got much worse) and the most optimistic category (5 = got much better)
# are shown separately for female and male respondents, with all other covariates
# held at their sample means. Any gender differences in these predicted 
# probabilities should be modest, given that gender does not appear to be a 
# strong driver of economic perceptions relative to the other covariates in
# the model. The overlapping confidence intervals for male and female respondents
# across most categories suggest that the gender gap in economic optimism is 
# not large in this dataset.


BEPS$vote = relevel(BEPS$vote, ref = "Conservative")
m_mlogit = multinom(vote ~ economic.cond.national + Blair + Hague +
                      Kennedy + Europe, data = BEPS, trace = FALSE)
summary(m_mlogit)

# The model produces two sets of log-odds coefficients: Labour vs. Conservative
# and Liberal Democrat vs. Conservative. The coefficient on Blair in the Labour 
# vs. Conservative equation is strongly positive: higher approval of Tony Blair 
# is associated with substantially greater log-odds of voting Labour rather than
# Conservative. This makes intuitive sense — Blair was the Labour leader, so 
# voters who rated him favorably were much more likely to have voted for his 
# party. By contrast, the Blair coefficient in the Liberal Democrat vs. 
# Conservative equation is expected to be smaller or near zero, since Blair 
# approval does not strongly differentiate Liberal Democrat voters from Conservatives.

avg_slopes(m_mlogit)

# The AME of Blair on the probability of voting Labour is positive and
# substantial. A one-unit increase in Blair approval (on the 1–5 scale)
# is associated with a meaningful increase in the average probability of
# voting Labour, holding all other variables constant. This reflects the
# strong personalization of vote choice in 1997: feelings toward the party
# leader were a major driver of vote intention, and Blair in particular
# was unusually popular relative to his Conservative counterpart.

# c) The multinomial logit assumes Independence of Irrelevant Alternatives (IIA):
# the odds ratio between any two alternatives (e.g., Labour vs. Conservative)
# is unaffected by the presence or characteristics of the third alternative
# (Liberal Democrats). In the red bus / blue bus analogy, IIA fails because
# two alternatives are near-perfect substitutes and removing one simply
# shifts its probability to the other rather than distributing it
# proportionally. For British party choice, IIA is a moderate concern:
# Labour and the Liberal Democrats are both centre-left parties, sharing
# some ideological space, so some voters may treat them as partial substitutes
# in a way IIA cannot accommodate. The Conservatives, however, occupy a
# clearly distinct ideological position (right-wing), so the three-party
# menu is not as degenerate as two buses of different colours. Overall,
# IIA is plausible for Conservative vs. the others but is a more legitimate
# worry for the Labour/Liberal Democrat distinction.

summary(bioChemists$art)

var(bioChemists$art)

ggplot(bioChemists, aes(x = art)) +
  geom_histogram(binwidth = 1, fill = "#294b66", color = "white") +
  theme_minimal() +
  labs(title = "Publications in last 3 years of PhD",
       x = "Number of articles", y = "Count")

# The distribution of art is right-skewed, with a mode at zero and a long
# upper tail. The mean is around 1.69 while the variance is approximately
# 3.71 — roughly twice the mean. Under the Poisson assumption, the variance
# should equal the mean; a ratio substantially above 1 indicates
# overdispersion. This pattern is a first signal that a standard Poisson
# model may underestimate uncertainty and produce anti-conservative
# standard errors.

m_pois = glm(art ~ fem + mar + kid5 + phd + ment,
             data = bioChemists, family = poisson)
summary(m_pois)

exp(coef(m_pois)["ment"])

# The incidence rate ratio (IRR) for ment is 1.026: each additional article
# published by the mentor is associated with a multiplicative increase in
# expected student articles by that factor, holding all else constant.
# The effect is modest but positive, suggesting that more productive mentors
# slightly boost student output. The residual deviance is substantially
# larger than the residual degrees of freedom (their ratio is well above 2),
# which is another clear diagnostic signal of overdispersion — the Poisson
# model does not adequately capture the variation in publication counts.

dispersiontest(m_pois)

# The dispersion test strongly rejects the null hypothesis of
# equidispersion (p < 0.001). The estimated dispersion parameter is well
# above 1, confirming that the variance in art substantially exceeds its
# mean. This means the Poisson standard errors are too small: the model
# underestimates uncertainty, inflates test statistics, and produces
# p-values that are misleadingly small. A model that explicitly accounts
# for overdispersion — such as the negative binomial — is needed.

m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment,
              data = bioChemists)
summary(m_nb)

# The coefficient on ment is similar to the Poisson estimate, indicating
# that the point estimate is reasonably stable. The key difference is in
# the standard errors: the negative binomial model produces larger, more
# honest uncertainty estimates. The estimated overdispersion parameter
# theta (shown in the summary) quantifies how much the variance exceeds
# the Poisson prediction; a smaller theta means more severe overdispersion.
# Here theta is moderate, indicating meaningful but not extreme
# extra-Poisson variation.

AIC(m_pois)

AIC(m_nb)

# The negative binomial AIC is substantially lower than the Poisson AIC,
# despite the NB model having one additional parameter (theta). Under AIC,
# the improvement in fit more than compensates for the added complexity.
# This confirms that overdispersion is a genuine feature of the data, not
# noise, and that the negative binomial is the more appropriate model for
# these publication counts.

predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))

# The predicted number of articles for men exceeds that for women,
# holding marital status, number of young children, PhD prestige, and
# mentor productivity constant at their sample means. The confidence
# intervals provide information on whether this gender gap is statistically
# distinguishable: if the intervals do not overlap, the difference is
# significant at conventional levels. The gap reflects a persistent
# within-group gender difference in publication productivity that is not
# simply an artefact of other observable characteristics.

# d) Summary of findings

# The Poisson model is not adequate for this dataset. The variance-to-mean
# ratio of art is roughly double, the residual deviance far exceeds the
# degrees of freedom, and the formal dispersiontest() rejects
# equidispersion with a p-value well below 0.001. The negative binomial
# model, which adds a dispersion parameter to accommodate this extra
# variation, achieves a substantially lower AIC and produces more reliable
# (wider) standard errors. On substantive findings: the mentor's
# productivity (ment) has a positive and statistically significant effect,
# with an IRR slightly above 1 — each additional mentor article is
# associated with a modest multiplicative increase in expected student
# articles, suggesting that working with a productive mentor confers a
# real, if small, boost. Gender (fem) and number of young children (kid5)
# are both negative and statistically significant: women publish fewer
# articles on average, and each additional child under age 5 is associated
# with reduced output. PhD program prestige (phd) and marital status (mar)
# are not statistically significant in the negative binomial model.
# Together, the results point to early-career productivity being shaped by
# mentor environment, gender, and family demands — patterns consistent
# with broader literature on PhD student outcomes in STEM fields.

#-----------------------------------------------------------------------------

# =========================
# Part 2: Survival Analysis
# =========================

# Load required packages
library(survival)
library(broom)
library(ggplot2)
library(marginaleffects)

# Load data
lung <- survival::lung

# Recode event variable (1=censored, 2=dead → 0=censored, 1=dead)
lung$dead <- lung$status - 1

# =========================
# 2.1 Kaplan-Meier Survival
# =========================

# a) Explore the data
n_total <- nrow(lung)
n_events <- sum(lung$dead == 1, na.rm = TRUE)
n_censored <- sum(lung$dead == 0, na.rm = TRUE)
prop_censored <- n_censored / n_total

n_total
n_events
n_censored
prop_censored

# There are 228 observations, 165 deaths, 63 alive patients, and thus 27% of the
# patients are censored. 

# b) Overall Kaplan-Meier curve
km_fit <- survfit(Surv(time, dead) ~ 1, data = lung)

summary(km_fit)
summary(km_fit)$table

# The median is 310 days, which means 310 days is the estimated time at which 
# 50% of the patients will have died

# c) KM curves by sex + plot + log-rank test
km_sex <- survfit(Surv(time, dead) ~ sex, data = lung)

km_tidy <- broom::tidy(km_sex)

p <- ggplot(km_tidy, aes(x = time, y = estimate, color = strata)) +
  geom_step() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = strata),
              alpha = 0.2, color = NA) +
  labs(title = "Kaplan-Meier Survival Curves by Sex",
       x = "Time (days)",
       y = "Survival Probability",
       color = "Sex",
       fill = "Sex") +
  theme_minimal()

ggsave("km_survival_by_sex.pdf", plot = p, width = 6, height = 4)

logrank <- survdiff(Surv(time, dead) ~ sex, data = lung)
logrank

p_value <- 1 - pchisq(logrank$chisq, df = 1)
p_value

# The p-value of the log-rank test indicates statistical significance, that is 
# to say, that the difference in survival time between the groups is unlikely
# to have occured purely by chance. The graph shows us that women have signifi
#cantly higher surival probability as days go by, although they fully converge
#at around 750 days, the cofidence intervals overlapping somewhat earlier than 
# that.

# =========================
# 2.2 Cox Model
# =========================

# a) Fit Cox proportional hazards model
cox_model <- coxph(Surv(time, dead) ~ age + sex + ph.ecog, data = lung)

summary(cox_model)

# HR for sex = 0.5754
# A HR of 0.5754 means that women have a 42.5% LOWER hazard of death
# compared to men, holding age and ph.ecog constant.
# This is statistically significant as we can see in the summary.
# This seems to indicate that women survive significantly longer.

exp(coef(cox_model))

# HR for ph.ecog = 1.5900
# A one-unit increase in ECOG score is associated with a 59% HIGHER hazard of
# death, holding age and sex constant.
# This is highly statistically significant as we can see in the summary.
# Worse ECOG performance is thus associated with shorter survival. 

# c) Test proportional hazards assumption
ph_test <- cox.zph(cox_model)
ph_test

plot(ph_test)

# None of the covariates have a significant p-value, and the global test is also
# non-significant. Therefore, we fail to reject the proportional hazards assumption —
# the assumption holds for the model and all of its variables. If the assumption
# were violated for, say, sex, it would mean that the proportional survival 
# advantage of women would reverse or fade over time, thus invalidating the 
# assumption and making the model unfit for use in such a case.

# =========================
# d) Final Summary
# =========================

# COMMENTS:
# The Kaplan-Meier analysis suggested significant differences between the sur-
# vival probabilities of each sex, women having a higher chance of survival 
# for up to 750 days. This difference between sexes remained significant when 
# accounting for age and ecog scores in posterior analysis. Although age did 
# not prove to be a significant factor, perhaps counterintuitively, the model, 
# which was fit for the data at hand for it did not violate the proportional 
# hazards assumption, also seemed to show that worse physical performance is 
# associated with lower chances of survival. 
