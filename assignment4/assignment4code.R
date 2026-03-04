library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(readstata13)
library(tidyverse)

df = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/infantmortality.dta")

summary(df)

nrow(df)

# There are 101 countries

p_hist_infant <- ggplot(df, aes(x = infant)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Infant Mortality Rate",
       x = "Infant Mortality (per 1,000 live births)", y = "Count") +
  theme_minimal()
ggsave("hist_infant.png", p_hist_infant, width = 6, height = 4)

p_hist_income <- ggplot(df, aes(x = income)) +
  geom_histogram(bins = 30, fill = "coral", color = "white") +
  labs(title = "Distribution of Income per Capita",
       x = "Income (USD)", y = "Count") +
  theme_minimal()
ggsave("hist_income.png", p_hist_income, width = 6, height = 4)

# Both histograms are right-skewed

p_scatter1 <- ggplot(df, aes(x = income, y = infant, color = region)) +
  geom_point(alpha = 0.7) +
  labs(title = "Infant Mortality vs. Income per Capita",
       x = "Income (USD)", y = "Infant Mortality (per 1,000 live births)",
       color = "Region") +
  theme_minimal()
ggsave("scatter_infant_income.png", p_scatter1, width = 7, height = 5)

p_scatter2 <- ggplot(df, aes(x = log(income), y = log(infant), color = region)) +
  geom_point(alpha = 0.7) +
  labs(title = "Log Infant Mortality vs. Log Income",
       x = "Log(Income)", y = "Log(Infant Mortality)",
       color = "Region") +
  theme_minimal()
ggsave("scatter_log_infant_log_income.png", p_scatter2, width = 7, height = 5)

# Most countries exhibit relatively low incomes, with no high mortality rates
# shown for the countries with higher income, but variation amongst the
# lower incomes countries. The log-log relationship does look more linear.

m1 <- lm(infant ~ income, data = df)
summary(m1)

m2 <- lm(log(infant) ~ log(income), data = df)
summary(m2)

cat("m1 coefficient on income (per $1,000 increase):",
    coef(m1)["income"] * 1000, "\n")
cat("m2 elasticity (log-log):", coef(m2)["log(income)"], "\n")
cat("Interpretation: A 10% increase in income is associated with a",
    round(coef(m2)["log(income)"] * 10, 2), "% change in infant mortality.\n")

# According to m1, a $1000 increase in income is associated with 20.9 fewer 
#deaths, but the right-skewedness and the low R2 mean this model fits poorly. 
# A 10% increase in income according to m2 is associated with a 5% decrease in 
# infant mortality, the coefficient being an elasticity.

aug_m1 <- broom::augment(m1)
p_resid_m1 <- ggplot(aug_m1, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, color = "blue") +
  labs(title = "Residuals vs. Fitted: Level-Level Model",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()
ggsave("resid_m1.png", p_resid_m1, width = 6, height = 4)

aug_m2 <- broom::augment(m2)
p_resid_m2 <- ggplot(aug_m2, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, color = "blue") +
  labs(title = "Residuals vs. Fitted: Log-Log Model",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()
ggsave("resid_m2.png", p_resid_m2, width = 6, height = 4)

# The residual pattern for the second model is way better, for the pattern is 
# more random around 0 and the spread is more constant

m3 = lm(log(infant) ~ log(income) + region + oil, data = df)
summary(m3)

cat("m2 income coefficient:", coef(m2)["log(income)"], "\n")
cat("m3 income coefficient:", coef(m3)["log(income)"], "\n")

# Controlling for oil and region does decrease the coefficient by about a third.
# The Africa region is taken as the baseline category, which means the log of 
# infant mortality is substantially higher in Africa on average than in other 
# regions at the same oil and income levels, given the coefficients. As such, 
# the Africa coefficient captures disadvantages not explained by the model.

avg_slopes(m3)

# A $1000 increase in income is associated with a fall of infant mortality to 
# approximately 20% of its previous value, this being statistically significant

m4 <- lm(log(infant) ~ log(income) * oil + region, data = df)
summary(m4)

slopes(m4, variables = "income", by = "oil")

# For non-oil countries, the relationship between income and mortality is also 
# strongly negative, but for oil countries, higher income means slightly higher
#infant mortality, which is interesting. A possible explanation is that income
# growth in oil-rich countries tends to be concentrated amongst the elites.

p_slopes_oil <- plot_slopes(m4, variables = "income", condition = "oil") +
  labs(title = "Marginal Effect of Income on Infant Mortality by Oil Status",
       x = "Oil Exporting Status", y = "Marginal Effect of Income") +
  theme_minimal()
ggsave("slopes_oil.png", p_slopes_oil, width = 6, height = 4)

preds <- predictions(m3,
                     newdata = datagrid(
                       income = c(1000, 20000, 10000),
                       region = c("Africa", "Europe", "Americas"),
                       oil = c("no", "no", "yes")
                     )
)
print(preds)

preds_df <- as.data.frame(preds)
preds_df$infant_predicted <- exp(preds_df$estimate)
preds_df$infant_lower <- exp(preds_df$conf.low)
preds_df$infant_upper <- exp(preds_df$conf.high)

cat("\nPredicted infant mortality (original scale):\n")
print(preds_df[, c("income", "region", "oil",
                   "infant_predicted", "infant_lower", "infant_upper")])

# The infant mortalities expected are 67 for the African country, 8.6 for the 
#European one, and around 33.5 for the American one. This seems broadly plausible,
# but the gap between Africa and Europe is large even when comparing at same 
# levels of income, which means there are important variables that aren't defined
# in the model.

p_pred <- plot_predictions(m3, condition = c("income", "region")) +
  labs(
    title = "Predicted Infant Mortality by Income and Region",
    subtitle = "Based on log-log model with controls for oil-exporting status",
    x = "Income per Capita (USD)",
    y = "Predicted Infant Mortality (per 1,000 live births)",
    color = "Region",
    fill = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))
ggsave("pred_plot_region.png", p_pred, width = 8, height = 5)

# Higher income is associated with lower infant mortality all around the world, 
# but the levels differ by region. The slopes are all the same because there are
# no interactions in the model, which is a limitation in itself. There can also 
# be concerns of reverse causality whereby infant mortality causes economic 
# stagnation, and the fact that the gap between regions exists in the first place
#indicates that there are other variables causing differences in infant mortality
# that might be region-specific beyond the ones included in the model, for 
# children do not die more often just based on their geographical location. 
# Additionally, this analysis is based on country-level data, so we cannot draw
# conclusions on an individual level.

aug_m3 <- broom::augment(m3)
p_resid_m3 <- ggplot(aug_m3, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, color = "blue") +
  labs(title = "Residuals vs. Fitted: Log-Log Model with Controls (m3)",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()
ggsave("resid_m3.png", p_resid_m3, width = 6, height = 4)

# There are reasons to be concerned about heteroskedasticity, the pattern is not
# linear and the spread is wider in medium than in lower levels

modelsummary(
  list("Level" = m1, "Log-Log" = m2, "Controls" = m3, "Interaction" = m4),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  title = "Regression Models: Wealth and Infant Mortality"
)

modelsummary(
  list("Level" = m1, "Log-Log" = m2, "Controls" = m3, "Interaction" = m4),
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  title = "Regression Models: Wealth and Infant Mortality"
)

# Using SEs is good because it controls for heteroskedasticity, but in this case
# the conclusions don't change drastically for most cases, apart from the
# relationship between having oil and higher infant mortality, which evaporates
# because it loses statistical significance.

