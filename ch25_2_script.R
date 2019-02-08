library(gapminder)
library(tidyverse)
library(modelr)

by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

by_country

# Funcion para fitear modelos
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

by_country <- by_country %>% 
  mutate(model = map(data, country_model))

# Agregar residuos y hacer unnesting
by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )

resids <- unnest(by_country, resids)

# Luego de hacer unnesting se puede graficar
ggplot(resids, aes(x = year, y = resid)) +
  geom_line(aes(group = country), alpha = 1/3) +
  geom_smooth(se = FALSE)

ggplot(resids, aes(x = year, y = resid)) +
  geom_line(aes(group = country), alpha = 1/3) +
  geom_smooth(se = FALSE) +
  facet_wrap(~continent)

# Metricas de calidad de los modelos
glance_models <- 
  by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)

# Mirando modelos con bajo ajuste
glance_models %>% 
  arrange(r.squared)

glance_models %>% 
  ggplot(aes(continent, r.squared)) +
  geom_jitter()

# Extrayendo paises con particularmente bajo R cuadrado
bad_fit <- glance_models %>% filter(r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line()

# Aca se observa el efecto del genocidio de Rwanda y la epidemia del VIH