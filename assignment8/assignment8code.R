# Part 1

# Load libraries
library(sf)
library(spData)
library(spdep)
library(spatialreg)
library(ggplot2)

# Load data
data(world)

# 1. Setup and OLS baseline

# Filter data and create log GDP
world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)

# Check number of observations
nrow(world)

# After removing observations with missing gdpPercap or lifeExp and dropping 
# Antarctica, 160 countries remain. Welog-transform GDP per capita because the raw
# variable is strongly right-skewed: a handful of very rich countries have values 
# far above the bulk of the distribution. The log transformation compresses the 
# upper tail and makes the relationship between GDP and life expectancy more linear,
# which is an assumption of OLS.

# OLS regression
ols_fit = lm(lifeExp ~ log_gdp, data = world)
summary(ols_fit)

# The coefficient on log_gdp is positive and statistically significant (p < 0.001).
# It means that a one-unit increase in log GDP per capita — roughly a doubling of
# GDP per capita — is associated with higher life expectancy by approximately that
# many years on average. The model explains a substantial share of cross-country 
# variation in life expectancy, as reflected by the R².

# Save residuals and plot
world$ols_resid = residuals(ols_fit)

ggplot(world) +
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "white",
    high = "#d6604d",
    midpoint = 0,
    name = "OLS residual"
  ) +
  theme_void() +
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")

ggsave("ols_residuals_map.pdf", width = 10, height = 5)

# The residual map reveals clear geographic clustering. Sub-Saharan Africa shows
# a concentration of negative 2residuals — countries with lower life expectancy 
# than the model predicts given their income level, likely due to high HIV/AIDS 
# prevalence and disease burden. Western Europe and parts of East Asia display 
# positive residuals, indicating that these regions achieve higher life expectancy
# than income alone predicts. This non-random geographic pattern in the residuals
# is a visual signal of spatial autocorrelation.

# 2. Spatial weights matrix

# Create neighbors and weights
nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)

# Summary of neighbors
summary(nb)

# Some countries have zero neighbors in the contiguity matrix. These are island
# nations (e.g., New Zealand, Japan, Caribbean states, Pacific island states) 
# that share no land boundary or common border point with any other polygon in the
# dataset. Queen contiguity requires at least one shared point; islands surrounded
# by ocean have none, so they are isolated nodes in the weights graph. The 
# zero.policy = TRUE argument allows these units to remain in the analysis despite
# having no neighbors.

# Moran’s I test on OLS residuals
moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)

# The Moran’s I statistic is positive and the p-value is well below 0.05, 
# indicating statistically significant positive spatial autocorrelation in the OLS
# residuals. Countries close to each other tend to have similar residuals — either
# both overestimated or both underestimated — which violates the OLS assumption of
# independent errors. Ignoring this pattern yields inefficient estimates and
# invalid standard errors.

# 3. Lagrange Multiplier tests

lm_tests = lm.LMtests(
  ols_fit,
  listw = listw,
  test = c("LMerr", "LMlag", "RLMerr", "RLMlag"),
  zero.policy = TRUE
)

summary(lm_tests)

# LMerr tests whether there is spatial dependence in the error term
# (λ ̸ = 0 in the SEM). LMlag tests whether a spatially lagged dependent variabl
# e belongs in the model (ρ ̸ = 0 in the SLM). Both tests are significan
# t (p < 0.05), meaning both types of spatial dependence appear to be present
# in some form when tested individually. When both standard LM tests are
# significant, we turn to the robust versions to discriminate.

# The robust tests (RLMerr, RLMlag) each control for the presence of the other 
# type of spatial dependence. Comparing them: if RLMerr is more significant than
# RLMlag, the evidence favors the SEM; if RLMlag dominates, the SLM is preferred.
# Based on the decision rule from class — select the model whose robust test is
# more significant — the output above guides the choice between the two 
# spatial models for Part 2.

# 4. Spatial Error Model (SEM)

sem_fit = errorsarlm(
  lifeExp ~ log_gdp,
  data = world,
  listw = listw,
  zero.policy = TRUE
)

summary(sem_fit)

# The coefficient on log_gdp from the SEM and the OLS estimate are both
# reported above. The SEM coefficient mayshift somewhat from OLS because the 
# error-structure correction absorbs spatial confounding. The ˆ λ (lambda) 
# parameter captures spatial autocorrelation in the errors; if it is positive 
# and statistically significant, the SEM has identified genuine spatial 
# dependence in the residual variation.

# In the SEM, λ governs the spatial autoregressive process in the disturbances:
# u = λWu + ε. A positive and significant λ means that the unmeasured factors 
# driving life expectancy are spatially correlated — omitted variables such as
# regional disease environments, cultural practices around healthcare, or 
# cross-border health infrastructure are themselves geographically clustered. 
# The SEM filters this spatial correlation out of the residuals without positing 
# that life expectancy itself directly diffuses across borders.

# Moran’s I on SEM residuals
world$sem_resid = residuals(sem_fit)

moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)

# Comparing this Moran’s I to the one from question 2b, the SEM substantially
# reduces the spatial autocorrelation in the residuals. The test statistic is now
# much closer to zero and the p-value is no longer significant (or much less so),
# indicating that the spatial error correction has absorbed most of the geographic
# clustering that OLS left behind in its residuals.

#----------------------------------------------------------------------------

############################################
# PART 2: Spatial Lag Model & Comparison
############################################

############################################
# 2.1 Spatial Lag Model (SLM / SAR)
############################################

# Fit Spatial Lag Model
slm_fit = lagsarlm(
  lifeExp ~ log_gdp,
  data = world,
  listw = listw,
  zero.policy = TRUE
)

# View results
summary(slm_fit)

# Save residuals (optional diagnostic)
world$slm_resid = residuals(slm_fit)

# The estimated rho (ρ) parameter is -0.0043 with a p-value of 0.806, thus not
# being significant. The coefficient on log_gdp is 5.548 and statistically 
# significant. As such, there is no evidence of spatial dependence, and a 
# positive correlation between log gdp per capita and life expectancy.

# Rho captures spatial dependence of the dependent variable, thus whether life
# expectancy in one country is influenced by the same variable in bordering
# countries. In this case, there is no evidence of such dependence.

# The coefficient on log_gdp in the SLM is NOT the marginal effect of GDP on life expectancy,
# because the Spatial Lag Model includes a feedback process.
# The matrix (I - ρW)^(-1) captures how changes in one country propagate
# through the network of neighboring countries and feed back. soa change in
# log_gdp in one country affects not only its own life expectancy,
# but also spills over to other countries and feeds back again.

############################################
# 2.2 Direct and Indirect Effects
############################################

# Set seed for reproducibility
set.seed(123)

# Compute impacts (direct, indirect, total)
slm_impacts = impacts(
  slm_fit,
  listw = listw,
  R = 500
)

# View effects
summary(slm_impacts)

# The direct effect of log_gdp is approximately 5.54.
# The indirect effect is approximately -0.022.

# The direct effect is very similar to the raw log_gdp coefficient from the SLM (~5.55)
# and also very close to the OLS estimate, because the spatial parameter rho is 
# close to zero, meaning there is very little spatial feedback in the model.

# The indirect effect represents the spillover effect of GDP in one country
# on life expectancy in other countries through the spatial network.

# Here, because the indirect effect is very small and not statistically, significant,
# we can assume that an increase in GDP per capita in one country does not impact
# life expectancy in neighbouring countries. 

# In this case, the total effect is slightly smaller than the direct effect 
# because the indirect effect is negative. In general, in Spatial Lag Models, 
# the total effect is often larger than the direct effect when rho is positive 
# and there are strong spillovers. In this case, however, spillovers are 
# negligible, and thus the model behaves like an OLS model and the importance
# of indirect effect is decreased.

############################################
# 2.3 Model Comparison (OLS vs SEM vs SLM)
############################################

# Compare model fit using AIC
AIC(ols_fit, sem_fit, slm_fit)

# The SEM has the lowest AIC, indicating the best model fit after penalizing for
# complexity, which is consistent with LM tests from Part 1, which suggested it 
# was preferable to use the SEM over the SLM. 

# The OLS residuals showed strong and statistically significant spatial autocorrelation,
# indicating a violation of the independence assumption.

# Based on the LM tests, we selected the SEM model, for the LMerr tests was more
# significant that the LMlag results. The coefficient on log_gdp decreases
# slightly in the SEM model, which suggests part of the relationship was due to 
# spatially correlated ommited variables, while rho is not significant, thus 
# there being no diffusion. This suggests that similarities in life expectancy 
# across countries are driven more by shared regional characteristics
# (captured by the SEM) than by direct spillovers.

# A limitation of queen contiguity weights is that they exclude island nations
# and treat all neighboring countries as equally connected regardless of distance,
# which may oversimplify real-world geographic relationships.

############################################
# (Optional) Check residual spatial autocorrelation
############################################

moran.test(world$slm_resid, listw = listw, zero.policy = TRUE)


############################################
# 2.4 Extension: Spatial Durbin Model (SDM)
############################################

# Fit Spatial Durbin Model
sdm_fit = lagsarlm(
  lifeExp ~ log_gdp,
  data = world,
  listw = listw,
  Durbin = TRUE,
  zero.policy = TRUE
)

# View results
summary(sdm_fit)

# Compare all models including SDM
AIC(ols_fit, sem_fit, slm_fit, sdm_fit)