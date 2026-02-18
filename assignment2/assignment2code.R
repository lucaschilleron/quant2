library(tidyverse)
library(broom)
library(modelsummary)
library(sandwich)

if (!dir.exists("outputs")) dir.create("outputs")

#2.1

star <- read_csv("C:/Users/Usuario/Downloads/star.csv")

star <- star %>%
  mutate(
    classtype_f = factor(
      classtype,
      levels = c(1, 2, 3),
      labels = c("Small", "Regular", "Regular+Aide")
    ),
    race_f = factor(
      race,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other")
    ),
    small = if_else(classtype_f == "Small", 1, 0)
  )

star %>%
  summarise(
    n_total = n(),
    n_nonmissing_g4reading = sum(!is.na(g4reading)),
    n_nonmissing_g4math = sum(!is.na(g4math))
  )

#2.2

reading_means <- star %>%
  group_by(classtype_f) %>%
  summarise(
    mean_g4reading = mean(g4reading, na.rm = TRUE),
    n = sum(!is.na(g4reading)),
    .groups = "drop"
  )

reading_means

# The highest mean is that of those students in small classes

m_read_biv <- star %>%
  lm(g4reading ~ small, data = .)

m_read_biv %>% tidy()

# According to this coefficient, students who are in small classes score on average 3.1 points higher than those who don't. However, the estimate is not statistically significant.

diff_means_reading <- star %>%
  group_by(small) %>%
  summarise(mean_read = mean(g4reading, na.rm = TRUE), .groups = "drop") %>%
  summarise(diff = mean_read[small == 1] - mean_read[small == 0]) %>%
  pull(diff)

coef_reg_reading <- m_read_biv %>%
  coef() %>%
  .["small"] %>%
  unname()

tibble(
  diff_means = diff_means_reading,
  regression_coef = coef_reg_reading
)

m_math_biv <- star %>%
  lm(g4math ~ small, data = .)

m_math_biv %>% tidy()

# The pattern for math goes in the same direction and is still not statistically significant, but the coefficient is much smaller.

#2.3

m_read_controls <- star %>%
  lm(g4reading ~ small + race_f + yearssmall, data = .)

m_read_controls %>% tidy()

coef_compare <- tibble(
  model = c("Bivariate", "Controls"),
  coef_small = c(
    coef(m_read_biv)["small"],
    coef(m_read_controls)["small"]
  )
)

coef_compare

# Adding controls changes the coefficient for small significantly and alters its direction, thus indicating that the structure of the selected subsample isn't close to the ideal structure of a randomised selection, where the unobserved variables have no impact on average. The coefficient on yearssmall, however, means that each additional year in a small class leads on average to 2,17 higher reading points, controlling for small and race, which points to the fact that someone currently in a small class could still see overall positive effects to its reading scores if they have spent more than 2 years in small classes, leaving statistical significance concerns to the side.

#2.4

m_read_interact <- star %>%
  lm(g4reading ~ small * race_f + yearssmall, data = .)

m_read_interact %>% tidy()

effects_by_race <- tibble(
  effect_white = coef(m_read_interact)["small"],
  effect_black = coef(m_read_interact)["small"] +
    coef(m_read_interact)["small:race_fBlack"]
)

effects_by_race

models_reading <- list(
  "Bivariate" = m_read_biv,
  "Controls" = m_read_controls,
  "Interaction" = m_read_interact
)

# Although, serving the white students as the base category, the direction of the coefficient changes from white to black students, neither of the terms are statistically significant, 

# 2.5

modelsummary(models_reading, vcov = "robust")

modelsummary(
  models_reading,
  vcov = "robust",
  output = "outputs/star_reading_models.html"
)

p <- modelplot(models_reading, vcov = "robust")
p

ggsave(
  "outputs/star_reading_coeffplot.png",
  plot = p,
  width = 10,
  height = 6
)

# 2.6

# The evidence from the STAR database is more credible than that coming from an observational study because of the randomised nature of class assignment within STAR, which ensures the experimental nature of the data generating process. Overall, the more exposure there is to small class assignment throughout time, the better for the average reading scores, even if being in a small class itself has a negative coefficient. The estimate for small itself is quite sensitive to model specifications, as we can see when going from the bivariate regression to the one with controls. This points to a limiting factor, which is the lack of successful randomisation in the subsample with non-missing observations. 
