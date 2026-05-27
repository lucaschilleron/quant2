# 1. REQUIRED PACKAGES
# ------------------------------------------------------------------------------
library(dplyr)           
library(fixest)         
library(marginaleffects) 
library(ggplot2)         
library(modelsummary)   
library(stringr)

# 2.
# ------------------------------------------------------------------------------
#vdemfull1 <- "the dataset"

vars_final <- vdemfull1 %>%
  select(
    country_id = country_id,
    year = year,
    pol2_raw           = v2smpolsoc,
    regime_support_raw = v2regimpgroup, 
    pol_raw            = v2cacamps,               
    cap_raw            = v2stfisccap, 
    gdppc_raw          = e_gdppc,    
    conflict_raw       = v2caviol,   
    regime_raw         = v2x_polyarchy             
  ) %>%
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(
    support_collapsed = case_when(
      regime_support_raw %in% c(0, 1, 3, 8)       ~ "Elite",
      regime_support_raw %in% 5                   ~ "Military",
      regime_support_raw %in% c(6, 7)             ~ "Identity",
      regime_support_raw %in% c(2, 4)             ~ "Bureaucratic/Political",
      regime_support_raw %in% c(9, 10, 11, 12)    ~ "Popular/Mass",
      regime_support_raw %in% 13                  ~ "External",
      TRUE                                   ~ "Other"
    ),
    support_collapsed = factor(support_collapsed, 
                               levels = c("Elite", "Identity", "Military", 
                                          "Bureaucratic/Political", "Popular/Mass",
                                          "External", "Other")),
    
    next_state_collapsed = lead(support_collapsed, 1),
    
    transition_collapsed = if_else(support_collapsed != next_state_collapsed, 1, 0)
  ) %>%
  
  mutate(
    spell_id = cumsum(lag(transition_collapsed, default = 0))
  ) %>%
  group_by(country_id, spell_id) %>%
  mutate(
    years_in_power = row_number() - 1,
    log_years = log(years_in_power + 1)
  ) %>%
  ungroup() %>%
  
  group_by(country_id) %>%
  mutate(
    lag_pol      = lag(pol_raw, 1),
    lag_cap      = lag(cap_raw, 1),
    lag_gdppc    = log(lag(gdppc_raw, 1) + 1),
    lag_conflict = lag(conflict_raw, 1),
    lag_regime   = lag(regime_raw, 1),
    lag_pol2     = lag(pol2_raw, 1)
  ) %>%
  ungroup() %>%
  
  filter(!is.na(lag_pol), !is.na(lag_cap), !is.na(lag_gdppc), !is.na(lag_pol2),  !is.na(transition_collapsed))%>%
  mutate(across(c(lag_pol, lag_cap, lag_gdppc, lag_conflict, lag_pol2, lag_regime, log_years), 
                ~ as.numeric(scale(.))))

collapsed_types <- c("Identity", "Military", "Bureaucratic/Political", "Popular/Mass", "External", "Other")

coalition_breakdown <- vars_final %>%
  group_by(support_collapsed) %>%
  summarise(
    Observations = n(),
    Unique_Countries = n_distinct(country_id),
    Avg_Years_In_Power = mean(years_in_power, na.rm = TRUE)
  )
print(coalition_breakdown)

datasummary_balance(
  ~ support_collapsed,
  data = vars_final %>% select(support_collapsed, lag_pol, lag_gdppc, lag_cap, lag_conflict),
  title = "Table 1: Covariate Means and Profiles Across Regime Support Coalitions",
  dinm = FALSE
)

vars_final %>%
  filter(transition_collapsed == 1) %>%
  count(support_collapsed, next_state_collapsed) %>%
  arrange(desc(n))

# ==============================================================================
# 3. MODEL SPECIFICATIONS: TRANSITIONS "OUT OF" CURRENT COALITION
# ==============================================================================

lpm_full_conditional <- feols(
  transition_collapsed ~ support_collapsed : (lag_pol + lag_cap + lag_gdppc + 
                                                lag_conflict + lag_regime + log_years) 
  | country_id + year,
  data    = vars_final,
  cluster = ~country_id
)

lpm_full_conditional2 <- feols(
  transition_collapsed ~ support_collapsed : (lag_pol2 + lag_cap + lag_gdppc + 
                                                lag_conflict + lag_regime + log_years) 
  | country_id + year, # Fixed Effects cleanly absorb the standalone base levels
  data    = vars_final,
  cluster = ~country_id
)


# ==============================================================================
# 4. MODEL SPECIFICATIONS: TRANSITIONS "TO" SPECIFIC COALITIONS
# ==============================================================================

results_lpm_to <- lapply(collapsed_types, function(dest) {
  model_data <- vars_final %>%
    mutate(to_dest = if_else(transition_collapsed == 1 & next_state_collapsed == dest, 1, 0)) %>%
    filter(!is.na(next_state_collapsed))
  
  feols(to_dest ~ lag_pol + lag_cap + lag_gdppc + lag_conflict + 
          lag_regime + log_years | country_id + year, 
        data = model_data, cluster = ~country_id)
})
names(results_lpm_to) <- collapsed_types

results_lpm_to2 <- lapply(collapsed_types, function(dest) {
  model_data <- vars_final %>%
    mutate(to_dest = if_else(transition_collapsed == 1 & next_state_collapsed == dest, 1, 0)) %>%
    filter(!is.na(next_state_collapsed))
  
  feols(to_dest ~ lag_pol2 + lag_cap + lag_gdppc + lag_conflict + 
          lag_regime + log_years | country_id + year, 
        data = model_data, cluster = ~country_id)
})
names(results_lpm_to2) <- collapsed_types

results_logit_to <- lapply(collapsed_types, function(dest) {
  model_data <- vars_final %>%
    mutate(to_dest = if_else(transition_collapsed == 1 & next_state_collapsed == dest, 1, 0)) %>%
    filter(!is.na(next_state_collapsed)) %>%
    group_by(country_id) %>%
    filter(n_distinct(to_dest) > 1) %>% 
    ungroup()
  
  feglm(to_dest ~ lag_pol + lag_cap + lag_gdppc + lag_conflict + lag_regime | country_id,
        data = model_data, family = binomial("logit"), cluster = ~country_id)
})
names(results_logit_to) <- collapsed_types

results_logit_to2 <- lapply(collapsed_types, function(dest) {
  model_data <- vars_final %>%
    mutate(to_dest = if_else(transition_collapsed == 1 & next_state_collapsed == dest, 1, 0)) %>%
    filter(!is.na(next_state_collapsed)) %>%
    group_by(country_id) %>%
    filter(n_distinct(to_dest) > 1) %>% 
    ungroup()
  
  feglm(to_dest ~ lag_pol2 + lag_cap + lag_gdppc + lag_conflict + lag_regime | country_id,
        data = model_data, family = binomial("logit"), cluster = ~country_id)
})
names(results_logit_to2) <- collapsed_types

#5

lpm_lead_check <- feols(
  transition_collapsed ~ support_collapsed * lag_pol + 
    support_collapsed * lead(lag_pol, 1) +  
    lag_cap + lag_gdppc + lag_conflict + log_years | country_id + year,
  data = vars_final,
  cluster = ~country_id
)

lpm_lead_check2 <- feols(
  transition_collapsed ~ support_collapsed * lag_pol2 + 
    support_collapsed * lead(lag_pol2, 1) +  
    lag_cap + lag_gdppc + lag_conflict + log_years | country_id + year,
  data = vars_final,
  cluster = ~country_id
)

reverse_placebo_model <- feols(
  lead(lag_pol, 1) ~ transition_collapsed * support_collapsed + 
    lag_cap + lag_gdppc + lag_conflict + lag_regime | country_id + year,
  data = vars_final,
  cluster = ~country_id
)

reverse_placebo_model2 <- feols(
  lead(lag_pol2, 1) ~ transition_collapsed * support_collapsed + 
    lag_cap + lag_gdppc + lag_conflict + lag_regime | country_id + year,
  data = vars_final,
  cluster = ~country_id
)

reverse_future_model <- feols(
  lead(lag_pol, 2) ~ transition_collapsed * support_collapsed + 
    lag_cap + lag_gdppc + lag_conflict + lag_regime | country_id + year,
  data = vars_final,
  cluster = ~country_id
)

reverse_future_model2 <- feols(
  lead(lag_pol2, 2) ~ transition_collapsed * support_collapsed + 
    lag_cap + lag_gdppc + lag_conflict + lag_regime | country_id + year,
  data = vars_final,
  cluster = ~country_id
)

library(car)

vif_check_model <- lm(
  transition_collapsed ~ support_collapsed + lag_pol + lag_cap + 
    lag_gdppc + lag_conflict + lag_regime + log_years,
  data = vars_final
)

vif_results <- car::vif(vif_check_model)
print(vif_results)

#6

all_mfx_lpm <- avg_slopes(
  lpm_full_conditional,
  variables = c("lag_pol", "lag_cap", "lag_gdppc", "lag_conflict", "lag_regime"),
  by = "support_collapsed"
)

all_mfx_lpm2 <- avg_slopes(
  lpm_full_conditional2,
  variables = c("lag_pol2", "lag_cap", "lag_gdppc", "lag_conflict", "lag_regime"),
  by = "support_collapsed"
)

# --- Summary Output Tables ---
modelsummary(results_lpm_to, stars = TRUE, title = "LPM: Destination Analysis")
modelsummary(results_logit_to, exponentiate = TRUE, stars = TRUE, title = "Logit: Destination Odds Ratios")

#----------------------------------

logit_interacted_trimmed <- feglm(
  transition_collapsed ~ support_collapsed * (lag_pol + lag_cap + lag_gdppc + 
                                                lag_conflict + lag_regime) | country_id,
  data     = vars_final %>% filter(!support_collapsed %in% c("External", "Other")),
  family   = binomial("logit"),
  cluster  = ~country_id,
  glm.iter = 200
)

logit_interacted_trimmed2 <- feglm(
  transition_collapsed ~ support_collapsed * (lag_pol2 + lag_cap + lag_gdppc + 
                                                lag_conflict + lag_regime) | country_id,
  data     = vars_final %>% filter(!support_collapsed %in% c("External", "Other")),
  family   = binomial("logit"),
  cluster  = ~country_id,
  glm.iter = 200
)

coef_pvals <- tidy(logit_interacted_trimmed, conf.int = TRUE) %>%
  filter(str_detect(term, "lag_pol|lag_cap|lag_gdppc|lag_conflict|lag_regime")) %>%
  mutate(
    sig = case_when(
      p.value < 0.01 ~ "p < 0.01",
      p.value < 0.05 ~ "p < 0.05",
      p.value < 0.10 ~ "p < 0.10",
      TRUE           ~ "n.s."
    ),
    coalition = case_when(
      str_detect(term, "Identity")      ~ "Identity",
      str_detect(term, "Military")      ~ "Military",
      str_detect(term, "Bureaucratic")  ~ "Bureaucratic/Political",
      str_detect(term, "Popular")       ~ "Popular/Mass",
      TRUE                              ~ "Elite"
    ),
    predictor = case_when(
      str_detect(term, "lag_pol$")      ~ "lag_pol",
      str_detect(term, "lag_cap")       ~ "lag_cap",
      str_detect(term, "lag_gdppc")     ~ "lag_gdppc",
      str_detect(term, "lag_conflict")  ~ "lag_conflict",
      str_detect(term, "lag_regime")    ~ "lag_regime"
    )
  ) %>%
  filter(!is.na(predictor))

mfx_points <- avg_slopes(
  logit_interacted_trimmed,
  variables = c("lag_pol", "lag_cap", "lag_gdppc", "lag_conflict", "lag_regime"),
  by = "support_collapsed",
  vcov = FALSE
)

mfx_with_sig <- mfx_points %>%
  left_join(
    coef_pvals %>% select(coalition, predictor, sig, p.value),
    by = c("support_collapsed" = "coalition", "term" = "predictor")
  )

plot_logit <- ggplot(mfx_with_sig,
                     aes(x = estimate, y = support_collapsed,
                         color = support_collapsed, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_point(size = 3) +
  scale_shape_manual(values = c("p < 0.01" = 8,
                                "p < 0.05" = 16,
                                "p < 0.10" = 17,
                                "n.s."     = 1)) +
  facet_wrap(~term, scales = "free_x", nrow = 2) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Coalition-Specific Average Marginal Effects on Transition Risk",
    x = "Average Marginal Effect (Probability Scale)",
    y = "Current Support Coalition",
    shape = "Significance",
    caption = "AMEs computed at observed covariate values. Point shapes indicate significance from clustered SEs.\nFixed-effects uncertainty not propagated."
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray95", color = NA)
  )

print(plot_logit)

logit_interacted_year <- feglm(
  transition_collapsed ~ support_collapsed * (lag_pol + lag_cap + lag_gdppc + 
                                                lag_conflict + lag_regime) | country_id + year,
  data     = vars_final %>% filter(!support_collapsed %in% c("External", "Other")),
  family   = binomial("logit"),
  cluster  = ~country_id,
  glm.iter = 200
)

library(broom)
library(purrr)
library(dplyr)
library(ggplot2)


mfx_lpm_clean1 <- all_mfx_lpm %>% 
  as.data.frame() %>%
  mutate(
    z_stat  = estimate / std.error,
    p_val   = 2 * pnorm(-abs(z_stat)),
    sig     = case_when(
      p_val < 0.01 ~ "p < 0.01",
      p_val < 0.05 ~ "p < 0.05",
      p_val < 0.10 ~ "p < 0.10",
      TRUE         ~ "n.s."
    ),
    term_clean = case_when(
      term == "lag_pol" ~ "Polarization (Metric Comparison)",
      TRUE              ~ term
    ),
    specification = "Primary Measure (lag_pol)"
  )

mfx_lpm_clean2 <- all_mfx_lpm2 %>% 
  as.data.frame() %>%
  mutate(
    z_stat  = estimate / std.error,
    p_val   = 2 * pnorm(-abs(z_stat)),
    sig     = case_when(
      p_val < 0.01 ~ "p < 0.01",
      p_val < 0.05 ~ "p < 0.05",
      p_val < 0.10 ~ "p < 0.10",
      TRUE         ~ "n.s."
    ),
    term_clean = case_when(
      term == "lag_pol2" ~ "Polarization (Metric Comparison)",
      TRUE               ~ term
    ),
    specification = "Alternative Measure (lag_pol2)"
  )

all_mfx_lpm_combined <- bind_rows(mfx_lpm_clean1, mfx_lpm_clean2) %>%
  mutate(
    sig = factor(sig, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "n.s.")),
    is_significant = if_else(sig == "n.s.", "Insignificant", "Significant")
  )

plot_lpm_comparison <- ggplot(all_mfx_lpm_combined, 
                              aes(x = estimate, y = support_collapsed, 
                                  color = specification, group = specification)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high, shape = sig, alpha = is_significant), 
                  position = position_dodge(width = 0.6), size = 0.6) +
  
  facet_wrap(~term_clean, scales = "free_x", nrow = 2) + 
  
  scale_color_manual(values = c("Primary Measure (lag_pol)" = "royalblue4", 
                                "Alternative Measure (lag_pol2)" = "darkorange3")) +
  
  scale_shape_manual(values = c("p < 0.01" = 8, "p < 0.05" = 16, "p < 0.10" = 17, "n.s." = 1)) +
  
  scale_alpha_manual(values = c("Significant" = 1.0, "Insignificant" = 0.45), guide = "none") +
  
  theme_minimal(base_size = 11) +
  labs(
    title = "Linear Probability Model: Conditional Marginal Effects Comparison",
    subtitle = "Percentage point changes in transition risk across separate polarization metrics",
    x = "Marginal Effect (Percentage Point Change)",
    y = "Current Support Coalition Base",
    color = "Model Specification",
    shape = "Statistical Significance",
    caption = "Note: Solid shapes (points/triangles/asterisks) indicate statistical confidence. Hollow circles (n.s.) indicate the effect is indistinguishable from zero.\nError bars reflect 95% confidence intervals built via cluster-robust variance estimators."
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(plot_lpm_comparison)

#-------------------------------

library(broom)
library(dplyr)
library(marginaleffects)
library(ggplot2)
library(stringr)

trimmed_data <- vars_final %>% 
  filter(!support_collapsed %in% c("External", "Other"))

logit_trimmed_PRIMARY <- feglm(
  transition_collapsed ~ support_collapsed * (lag_pol + lag_cap + lag_gdppc + lag_conflict + lag_regime),
  data     = trimmed_data,
  family   = binomial("logit"),
  cluster  = ~country_id,
  glm.iter = 200
)

logit_trimmed_ROBUST <- feglm(
  transition_collapsed ~ support_collapsed * (lag_pol2 + lag_cap + lag_gdppc + lag_conflict + lag_regime),
  data     = trimmed_data,
  family   = binomial("logit"),
  cluster  = ~country_id,
  glm.iter = 200
)

pvals_primary <- tidy(logit_trimmed_PRIMARY) %>% 
  filter(str_detect(term, "lag_pol|lag_cap|lag_gdppc|lag_conflict|lag_regime")) %>%
  mutate(
    sig = case_when(
      p.value < 0.01 ~ "p < 0.01", p.value < 0.05 ~ "p < 0.05",
      p.value < 0.10 ~ "p < 0.10", TRUE           ~ "n.s."
    ),
    coalition = case_when(
      str_detect(term, "Identity")     ~ "Identity", 
      str_detect(term, "Military")     ~ "Military",
      str_detect(term, "Bureaucratic") ~ "Bureaucratic/Political",
      str_detect(term, "Popular")      ~ "Popular/Mass", 
      TRUE                             ~ "Elite" 
    ),
    predictor = case_when(
      str_detect(term, "lag_pol")      ~ "lag_pol", 
      str_detect(term, "lag_cap")      ~ "lag_cap",
      str_detect(term, "lag_gdppc")    ~ "lag_gdppc", 
      str_detect(term, "lag_conflict") ~ "lag_conflict",
      str_detect(term, "lag_regime")   ~ "lag_regime"
    )
  ) %>% 
  filter(!is.na(predictor)) %>%
  select(coalition, predictor, sig)  # <-- FIX: drop 'estimate' and other tidy() cols

mfx_primary <- avg_slopes(
  logit_trimmed_PRIMARY, 
  variables = c("lag_pol", "lag_cap", "lag_gdppc", "lag_conflict", "lag_regime"),
  by = "support_collapsed", vcov = FALSE
) %>% 
  as.data.frame()

if ("dydx" %in% colnames(mfx_primary)) {
  mfx_primary$estimate <- mfx_primary$dydx
}

mfx_primary_final <- mfx_primary %>%
  left_join(pvals_primary, by = c("support_collapsed" = "coalition", "term" = "predictor")) %>%
  mutate(
    term_clean = if_else(term == "lag_pol", "Polarization (Metric Comparison)", term),
    specification = "Trimmed Logit: Primary (lag_pol)"
  )

pvals_robust <- tidy(logit_trimmed_ROBUST) %>% 
  filter(str_detect(term, "lag_pol2|lag_cap|lag_gdppc|lag_conflict|lag_regime")) %>%
  mutate(
    sig = case_when(
      p.value < 0.01 ~ "p < 0.01", p.value < 0.05 ~ "p < 0.05",
      p.value < 0.10 ~ "p < 0.10", TRUE           ~ "n.s."
    ),
    coalition = case_when(
      str_detect(term, "Identity")     ~ "Identity", 
      str_detect(term, "Military")     ~ "Military",
      str_detect(term, "Bureaucratic") ~ "Bureaucratic/Political",
      str_detect(term, "Popular")      ~ "Popular/Mass", 
      TRUE                             ~ "Elite"
    ),
    predictor = case_when(
      str_detect(term, "lag_pol2")     ~ "lag_pol2", 
      str_detect(term, "lag_cap")      ~ "lag_cap",
      str_detect(term, "lag_gdppc")    ~ "lag_gdppc", 
      str_detect(term, "lag_conflict") ~ "lag_conflict",
      str_detect(term, "lag_regime")   ~ "lag_regime"
    )
  ) %>% 
  filter(!is.na(predictor)) %>%
  select(coalition, predictor, sig)  # <-- FIX: drop 'estimate' and other tidy() cols

mfx_robust <- avg_slopes(
  logit_trimmed_ROBUST, 
  variables = c("lag_pol2", "lag_cap", "lag_gdppc", "lag_conflict", "lag_regime"),
  by = "support_collapsed", vcov = FALSE
) %>% 
  as.data.frame()

if ("dydx" %in% colnames(mfx_robust)) {
  mfx_robust$estimate <- mfx_robust$dydx
}

mfx_robust_final <- mfx_robust %>%
  left_join(pvals_robust, by = c("support_collapsed" = "coalition", "term" = "predictor")) %>%
  mutate(
    term_clean = if_else(term == "lag_pol2", "Polarization (Metric Comparison)", term),
    specification = "Trimmed Logit: Robustness (lag_pol2)"
  )

logit_plots_combined <- bind_rows(mfx_primary_final, mfx_robust_final) %>%
  filter(!is.na(term_clean)) %>%
  mutate(
    sig = factor(sig, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "n.s.")),
    specification = factor(specification, levels = c("Trimmed Logit: Primary (lag_pol)", 
                                                     "Trimmed Logit: Robustness (lag_pol2)"))
  )

plot_trimmed_robust_final <- ggplot(logit_plots_combined,
                                    aes(x = estimate, y = support_collapsed, 
                                        color = specification, group = specification, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  
  geom_point(size = 3.2, position = position_dodge(width = 0.6)) +
  
  scale_shape_manual(values = c("p < 0.01" = 8, "p < 0.05" = 16, "p < 0.10" = 17, "n.s." = 1),
                     drop = FALSE) +
  
  scale_color_manual(values = c("Trimmed Logit: Primary (lag_pol)" = "purple4", 
                                "Trimmed Logit: Robustness (lag_pol2)" = "seagreen4")) +
  
  facet_wrap(~term_clean, scales = "free_x", nrow = 2) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Trimmed Fixed Effects Logit: Comparative Average Marginal Effects",
    subtitle = "Omitting 'External' and 'Other' coalitions; country-level fixed effects model structure",
    x = "Average Marginal Effect (Probability Scale)",
    y = "Operational Support Coalition Base",
    shape = "Significance Threshold",
    color = "Model Specification Target",
    caption = "Note: Solid shapes indicate statistical confidence. Hollow circles (n.s.) are statistically indistinguishable from zero.\nEstimates map direct percentage changes holding country fixed effects constant."
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )

print(plot_trimmed_robust_final)

library(survival)
library(cmprsk)
library(tidyr)
library(dplyr)
library(ggplot2)
library(survminer)

vars_with_true_spells <- vars_final %>%
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(
    # Triggers a new index number every time the regime type changes within a country
    regime_change_trigger = if_else(support_collapsed != lag(support_collapsed) | is.na(lag(support_collapsed)), 1, 0),
    true_spell_id = cumsum(regime_change_trigger)
  ) %>%
  ungroup()

spell_data <- vars_with_true_spells %>%
  group_by(country_id, true_spell_id) %>%
  summarise(
    duration         = n(),
    
    source_coalition = first(support_collapsed),
    destination      = last(next_state_collapsed),
    transitioned     = max(transition_collapsed, na.rm = TRUE),
    
    baseline_pol      = first(lag_pol),
    baseline_pol2     = first(lag_pol2),
    baseline_cap      = first(lag_cap),
    baseline_gdppc    = first(lag_gdppc),
    baseline_conflict = first(lag_conflict),
    baseline_regime   = first(lag_regime),
    
    .groups = "drop"
  ) %>%
  mutate(
    cr_status = case_when(
      transitioned == 0 | is.na(destination) ~ 0,  # Right-Censored
      destination == "Elite"                  ~ 1,
      destination == "Identity"               ~ 2,
      destination == "Military"               ~ 3,
      destination == "Bureaucratic/Political" ~ 4,
      destination == "Popular/Mass"           ~ 5,
      destination == "External"               ~ 6,
      destination == "Other"                  ~ 7,
      TRUE                                    ~ 0
    ),
    binary_status = if_else(transitioned == 1, 1, 0)
  )

print(table(spell_data$cr_status))

cov_matrix <- spell_data %>%
  select(baseline_pol, baseline_cap, baseline_gdppc, baseline_conflict, baseline_regime) %>%
  as.matrix()

cov_matrix2 <- spell_data %>%
  select(baseline_pol2, baseline_cap, baseline_gdppc, baseline_conflict, baseline_regime) %>%
  as.matrix()

print("Estimating Fine-Gray models across all competing risk vectors...")

destination_codes  <- c(1, 2, 3, 4, 5, 6, 7)
destination_labels <- c("Elite", "Identity", "Military", "Bureaucratic/Political", 
                        "Popular/Mass", "External", "Other")

cr_models <- lapply(seq_along(destination_codes), function(i) {
  crr(
    ftime    = spell_data$duration,
    fstatus  = spell_data$cr_status,
    cov1     = cov_matrix,
    failcode = destination_codes[i]
  )
})
names(cr_models) <- destination_labels

cr_models2 <- lapply(seq_along(destination_codes), function(i) {
  crr(
    ftime    = spell_data$duration,
    fstatus  = spell_data$cr_status,
    cov1     = cov_matrix2,
    failcode = destination_codes[i]
  )
})
names(cr_models2) <- destination_labels

cox_model <- coxph(
  Surv(duration, binary_status) ~ baseline_pol + baseline_cap + baseline_gdppc + 
    baseline_conflict + baseline_regime + strata(source_coalition),
  data    = spell_data,
  cluster = country_id
)

cox_model2 <- coxph(
  Surv(duration, binary_status) ~ baseline_pol2 + baseline_cap + baseline_gdppc + 
    baseline_conflict + baseline_regime + strata(source_coalition),
  data    = spell_data,
  cluster = country_id
)

tidy_cr <- bind_rows(
  lapply(destination_labels, function(dest) {
    m <- cr_models[[dest]]
    data.frame(
      destination = dest,
      term        = rownames(m$coef %>% as.matrix()),
      estimate    = as.numeric(m$coef),
      se          = sqrt(diag(m$var)),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        conf.low  = estimate - 1.96 * se,
        conf.high = estimate + 1.96 * se,
        p.value   = 2 * pnorm(-abs(estimate / se)),
        sig = case_when(
          p.value < 0.01 ~ "p < 0.01",
          p.value < 0.05 ~ "p < 0.05",
          p.value < 0.10 ~ "p < 0.10",
          TRUE           ~ "n.s."
        )
      )
  })
) %>%
  rename(predictor = term) %>%
  filter(predictor %in% c("baseline_pol", "baseline_cap", "baseline_gdppc", "baseline_conflict", "baseline_regime")) %>%
  mutate(
    shr      = exp(estimate),
    shr_low  = exp(conf.low),
    shr_high = exp(conf.high)
  )

tidy_cr2 <- bind_rows(
  lapply(destination_labels, function(dest) {
    m <- cr_models2[[dest]]
    data.frame(
      destination = dest,
      term        = rownames(m$coef %>% as.matrix()),
      estimate    = as.numeric(m$coef),
      se          = sqrt(diag(m$var)),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        conf.low  = estimate - 1.96 * se,
        conf.high = estimate + 1.96 * se,
        p.value   = 2 * pnorm(-abs(estimate / se)),
        sig = case_when(
          p.value < 0.01 ~ "p < 0.01",
          p.value < 0.05 ~ "p < 0.05",
          p.value < 0.10 ~ "p < 0.10",
          TRUE           ~ "n.s."
        )
      )
  })
) %>%
  rename(predictor = term) %>%
  filter(predictor %in% c("baseline_pol2", "baseline_cap", "baseline_gdppc", 
                          "baseline_conflict", "baseline_regime")) %>%
  mutate(
    shr      = exp(estimate),
    shr_low  = exp(conf.low),
    shr_high = exp(conf.high)
  )

cif <- cuminc(
  ftime   = spell_data$duration,
  fstatus = spell_data$cr_status
)

plot(cif, 
     xlab  = "Years in Coalition Spell",
     ylab  = "Cumulative Transition Probability",
     main  = "Cumulative Incidence by Destination Type",
     lty   = 1:7,
     col   = 1:7)
legend("topleft", 
       legend = destination_labels,
       lty    = 1:7, 
       col    = 1:7,
       cex    = 0.8)

#-----------------


fit_cox <- survfit(cox_model, data = spell_data)

plot_survival <- ggsurvplot(
  fit_cox,
  data = spell_data,
  
  palette = "Dark2",            
  linewidth = 1,               
  conf.int = TRUE,             
  conf.int.alpha = 0.1,      
  
  fun = "pct",                 
  break.time.by = 5,          
  xlim = c(0, 30),            
  
  ggtheme = theme_minimal(base_size = 12) + 
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold")
    ),
  
  title = "Adjusted Regime Survival Profiles by Source Coalition",
  xlab = "Years in Power (Spell Duration)",
  ylab = "Probability of Regime Survival (%)",
  legend.title = "Starting Coalition",
  legend.labs = c("Elite", "Identity", "Military", "Bureaucratic/Political", 
                  "Popular/Mass", "External", "Other"),
  
  risk.table = TRUE,            
  risk.table.y.text = FALSE,  
  risk.table.height = 0.25      
)
print(plot_survival)

#--------------------------------

spell_data_means <- vars_with_true_spells %>%
  group_by(country_id, true_spell_id) %>%
  summarise(
    duration         = n(),
    source_coalition = first(support_collapsed),
    destination      = last(next_state_collapsed),
    transitioned     = max(transition_collapsed, na.rm = TRUE),
    
    mean_pol      = mean(lag_pol, na.rm = TRUE),
    mean_pol2     = mean(lag_pol2, na.rm = TRUE), # Alternative operationalization variable
    mean_cap      = mean(lag_cap, na.rm = TRUE),
    mean_gdppc    = mean(lag_gdppc, na.rm = TRUE),
    mean_conflict = mean(lag_conflict, na.rm = TRUE),
    mean_regime   = mean(lag_regime, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    cr_status = case_when(
      transitioned == 0 | is.na(destination) ~ 0,  
      destination == "Elite"                  ~ 1,
      destination == "Identity"               ~ 2,
      destination == "Military"               ~ 3,
      destination == "Bureaucratic/Political" ~ 4,
      destination == "Popular/Mass"           ~ 5,
      destination == "External"               ~ 6,
      destination == "Other"                  ~ 7,
      TRUE                                    ~ 0
    ),
    binary_status = if_else(transitioned == 1, 1, 0)
  )

cov_matrix_means <- spell_data_means %>%
  select(mean_pol, mean_cap, mean_gdppc, mean_conflict, mean_regime) %>%
  as.matrix()

cov_matrix_means2 <- spell_data_means %>%
  select(mean_pol2, mean_cap, mean_gdppc, mean_conflict, mean_regime) %>%
  as.matrix()

cr_models_means <- lapply(seq_along(destination_codes), function(i) {
  crr(
    ftime    = spell_data_means$duration,
    fstatus  = spell_data_means$cr_status,
    cov1     = cov_matrix_means,
    failcode = destination_codes[i]
  )
})
names(cr_models_means) <- destination_labels

cr_models_means2 <- lapply(seq_along(destination_codes), function(i) {
  crr(
    ftime    = spell_data_means$duration,
    fstatus  = spell_data_means$cr_status,
    cov1     = cov_matrix_means2,
    failcode = destination_codes[i]
  )
})
names(cr_models_means2) <- destination_labels

tidy_cr_means <- bind_rows(
  lapply(destination_labels, function(dest) {
    m <- cr_models_means[[dest]]
    data.frame(
      destination = dest,
      term        = rownames(m$coef %>% as.matrix()),
      estimate    = as.numeric(m$coef),
      se          = sqrt(diag(m$var)),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        conf.low  = estimate - 1.96 * se,
        conf.high = estimate + 1.96 * se,
        p.value   = 2 * pnorm(-abs(estimate / se)),
        sig = case_when(
          p.value < 0.01 ~ "p < 0.01",
          p.value < 0.05 ~ "p < 0.05",
          p.value < 0.10 ~ "p < 0.10",
          TRUE           ~ "n.s."
        )
      )
  })
) %>%
  rename(predictor = term) %>%
  mutate(
    shr      = exp(estimate),
    shr_low  = exp(conf.low),
    shr_high = exp(conf.high),
    predictor_clean = case_when(
      predictor == "mean_pol"      ~ "Over-Time Mean Polarization (Primary)",
      predictor == "mean_cap"      ~ "Over-Time Mean Fiscal Capacity",
      predictor == "mean_gdppc"    ~ "Over-Time Mean Log(GDP per Capita)",
      predictor == "mean_conflict" ~ "Over-Time Mean Political Violence",
      predictor == "mean_regime"   ~ "Over-Time Mean Baseline Dem. Score"
    )
  )

# TESTS -------------------------------------------------------

library(survival)
library(dplyr)
library(ggplot2)
library(survminer)

fg_expanded_panel <- finegray(
  Surv(duration, factor(cr_status)) ~ mean_pol + mean_cap + mean_gdppc + mean_conflict + mean_regime, 
  data  = spell_data_means, 
  etype = "2" 
)

fg_global_fit <- coxph(
  Surv(fgstart, fgstop, fgstatus) ~ mean_pol + mean_cap + mean_gdppc + mean_conflict + mean_regime,
  data    = fg_expanded_panel,
  weights = fgwt # Vital: applies the subdistribution censoring weights
)

global_proportionality_test <- cox.zph(fg_global_fit, transform = "identity")

print(global_proportionality_test)

plot_global_proportionality <- ggcoxzph(
  global_proportionality_test,
  font.main = c(12, "bold", "black"),
  font.x    = c(11, "bold"),
  font.y    = c(11, "bold"),
  caption   = "Subdistribution Schoenfeld Residuals plotted across spell durations.\nSolid lines represent local splines; dashed lines bound 95% confidence intervals.",
  ggtheme   = theme_minimal(base_size = 11) + 
    theme(
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      strip.text       = element_text(face = "bold")
    )
)

print(plot_global_proportionality)

vif_check <- lm(duration ~ mean_pol + mean_cap + mean_gdppc + mean_conflict + mean_regime, 
                data = spell_data_means)

car::vif(vif_check)

fg_expanded_initial <- finegray(
  Surv(duration, factor(cr_status)) ~ baseline_pol + baseline_cap + baseline_gdppc + baseline_conflict + baseline_regime, 
  data  = spell_data, 
  etype = "2" 
)

fg_initial_fit <- coxph(
  Surv(fgstart, fgstop, fgstatus) ~ baseline_pol + baseline_cap + baseline_gdppc + baseline_conflict + baseline_regime,
  data    = fg_expanded_initial,
  weights = fgwt # Applies the subdistribution censoring weights
)

global_prop_test_initial <- cox.zph(fg_initial_fit, transform = "identity")

print(global_prop_test_initial)

plot_initial_proportionality <- ggcoxzph(
  global_prop_test_initial,
  font.main = c(12, "bold", "black"),
  font.x    = c(11, "bold"),
  font.y    = c(11, "bold"),
  caption   = "Subdistribution Schoenfeld Residuals plotted across spell durations (Initial Values Setup).\nSolid lines represent local splines; dashed lines bound 95% confidence intervals.",
  ggtheme   = theme_minimal(base_size = 11) + 
    theme(
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      strip.text       = element_text(face = "bold")
    )
)

print(plot_initial_proportionality)

vif_check2 <- lm(
  duration ~ mean_pol2 + mean_cap + mean_gdppc + mean_conflict + mean_regime, 
  data = spell_data_means
)

print(car::vif(vif_check2))

tidy_cr_initial <- bind_rows(
  lapply(destination_labels, function(dest) {
    m <- cr_models[[dest]]
    data.frame(
      destination = dest,
      term        = rownames(m$coef %>% as.matrix()),
      estimate    = as.numeric(m$coef),
      se          = sqrt(diag(m$var)),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        conf.low  = estimate - 1.96 * se,
        conf.high = estimate + 1.96 * se,
        p.value   = 2 * pnorm(-abs(estimate / se)),
        sig = case_when(
          p.value < 0.01 ~ "p < 0.01",
          p.value < 0.05 ~ "p < 0.05",
          p.value < 0.10 ~ "p < 0.10",
          TRUE           ~ "n.s."
        )
      )
  })
) %>%
  rename(predictor = term) %>%
  filter(predictor %in% c("baseline_pol", "baseline_cap", "baseline_gdppc",
                          "baseline_conflict", "baseline_regime")) %>%
  mutate(
    shr      = exp(estimate),
    shr_low  = exp(conf.low),
    shr_high = exp(conf.high),
    predictor_clean = case_when(
      predictor == "baseline_pol"      ~ "Entry-State Polarization (Primary)",
      predictor == "baseline_cap"      ~ "Entry-State Fiscal Capacity",
      predictor == "baseline_gdppc"    ~ "Entry-State Log(GDP per Capita)",
      predictor == "baseline_conflict" ~ "Entry-State Political Violence",
      predictor == "baseline_regime"   ~ "Entry-State Baseline Dem. Score"
    )
  )

tidy_cr_initial2 <- bind_rows(
  lapply(destination_labels, function(dest) {
    m <- cr_models2[[dest]]
    data.frame(
      destination = dest,
      term        = rownames(m$coef %>% as.matrix()),
      estimate    = as.numeric(m$coef),
      se          = sqrt(diag(m$var)),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        conf.low  = estimate - 1.96 * se,
        conf.high = estimate + 1.96 * se,
        p.value   = 2 * pnorm(-abs(estimate / se)),
        sig = case_when(
          p.value < 0.01 ~ "p < 0.01",
          p.value < 0.05 ~ "p < 0.05",
          p.value < 0.10 ~ "p < 0.10",
          TRUE           ~ "n.s."
        )
      )
  })
) %>%
  rename(predictor = term) %>%
  filter(predictor %in% c("baseline_pol2", "baseline_cap", "baseline_gdppc",
                          "baseline_conflict", "baseline_regime")) %>%
  mutate(
    shr      = exp(estimate),
    shr_low  = exp(conf.low),
    shr_high = exp(conf.high),
    predictor_clean = case_when(
      predictor == "baseline_pol2"     ~ "Entry-State Polarization (Alternative)",
      predictor == "baseline_cap"      ~ "Entry-State Fiscal Capacity",
      predictor == "baseline_gdppc"    ~ "Entry-State Log(GDP per Capita)",
      predictor == "baseline_conflict" ~ "Entry-State Political Violence",
      predictor == "baseline_regime"   ~ "Entry-State Baseline Dem. Score"
    )
  )

results1 <- tidy_cr_models(cr_models,  "Primary (lag_pol)")
results2 <- tidy_cr_models(cr_models2, "Robustness (lag_pol2)")

combined_results <- bind_rows(results1, results2) %>%
  mutate(spec = factor(spec, levels = c("Robustness (lag_pol2)", "Primary (lag_pol)"))) %>%
  mutate(
    predictor_label = case_when(
      grepl("pol2", term) | grepl("pol", term) ~ "Polarization Metric",
      grepl("cap", term)                       ~ "Baseline Fiscal Capacity",
      grepl("gdppc", term)                     ~ "Baseline Log(GDP per Capita)",
      grepl("conflict", term)                  ~ "Baseline Political Violence",
      grepl("regime", term)                    ~ "Baseline Dem. Score"
    )
  )

plot_compare_shr <- ggplot(combined_results, 
                           aes(x = shr, y = destination, color = spec, shape = sig)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = shr_low, xmax = shr_high), 
                  position = position_dodge(width = 0.6), size = 0.4) +
  scale_shape_manual(values = c("p < 0.01" = 8, "p < 0.05" = 16, "p < 0.10" = 17, "n.s." = 1)) +
  scale_color_manual(values = c("Primary (lag_pol)" = "purple4", 
                                "Robustness (lag_pol2)" = "seagreen4")) +
  facet_wrap(~predictor_label, scales = "free_x", ncol = 2) +
  theme_minimal(base_size = 11) +
  labs(title = "Competing Risks: Comparative Subdistribution Hazard Ratios",
       subtitle = "Comparing Primary vs. Alternative Polarization Metrics (Baseline Entry)",
       x = "Subdistribution Hazard Ratio (SHR)", y = "Transition Destination") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

print(plot_compare_shr)

results_means1 <- tidy_cr_models(cr_models_means,  "Mean-based: Primary (lag_pol)")
results_means2 <- tidy_cr_models(cr_models_means2, "Mean-based: Robustness (lag_pol2)")

combined_results_means <- bind_rows(results_means1, results_means2) %>%
  # REVERSED: Robustness is first, so it dodges to the top/first position
  mutate(spec = factor(spec, levels = c("Mean-based: Robustness (lag_pol2)", "Mean-based: Primary (lag_pol)"))) %>%
  mutate(
    predictor_label = case_when(
      grepl("pol2", term) | grepl("pol", term) ~ "Polarization Metric",
      grepl("cap", term)                       ~ "Mean Fiscal Capacity",
      grepl("gdppc", term)                     ~ "Mean Log(GDP per Capita)",
      grepl("conflict", term)                  ~ "Mean Political Violence",
      grepl("regime", term)                    ~ "Mean Dem. Score"
    )
  )

plot_compare_shr_means <- ggplot(combined_results_means, 
                                 aes(x = shr, y = destination, color = spec, shape = sig)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = shr_low, xmax = shr_high), 
                  position = position_dodge(width = 0.6), size = 0.4) +
  scale_shape_manual(values = c("p < 0.01" = 8, "p < 0.05" = 16, "p < 0.10" = 17, "n.s." = 1)) +
  scale_color_manual(values = c("Mean-based: Primary (lag_pol)" = "darkorchid4", 
                                "Mean-based: Robustness (lag_pol2)" = "darkcyan")) +
  facet_wrap(~predictor_label, scales = "free_x", ncol = 2) +
  theme_minimal(base_size = 11) +
  labs(title = "Competing Risks (Mean-based): Comparative SHRs",
       subtitle = "Comparing Primary vs. Alternative Polarization Metrics (Time-Averaged)",
       x = "Subdistribution Hazard Ratio (SHR)", y = "Transition Destination") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

print(plot_compare_shr_means)