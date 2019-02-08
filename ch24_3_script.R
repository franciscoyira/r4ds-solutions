library(tidyverse)
library(lubridate)
library(nycflights13)
library(modelr)

daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())

ggplot(daily, aes(date, n)) +
  geom_line()

daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))

ggplot(daily, aes(x = wday, y = n)) +
  geom_boxplot()

mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

ggplot(daily, aes(wday, n)) + 
  geom_boxplot() +
  geom_point(data = grid, color = "red", size = 4)


# Ahora mostramos los residuos para ver las variaciones que no son explicadas por el dia de la semana

daily <- daily %>% 
  add_residuals(mod)

ggplot(daily, aes(date, resid)) +
  geom_line() +
  geom_ref_line(h = 0)

# Parece ser que despues de Junio siguen habiendo patrones fuertes asociados a dias de la semana
# Periodos donde hay mas vuelos en sabado (vacaciones) y menos vuelos (resto del ano)
ggplot(daily, aes(date, resid, color = wday)) +
  geom_line()

# Dias donde hay muchos menos vuelos de los predichos: dias especiales como ano
# nuevo, 4 de julio, dia de accion de gracias, etc
daily %>% 
  filter(resid < -100)

# Tendencia de mas largo plazo
ggplot(daily, aes(date, resid)) +
  geom_line(color = "grey50") +
  geom_ref_line(h = 0) +
  geom_smooth(se = FALSE, span = 0.2)
  
# Enfocandonos en los sabados
daily %>% 
  filter(wday == "sab\\.") %>% 
  ggplot(aes(date, n)) +
  geom_point() +
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

# Creando variable que capture los periodos del ano escolar de USA
term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall"))
}

daily <- daily %>% 
  mutate(term = term(date))

daily %>% 
  filter(wday == "sab\\.") %>% 
  ggplot(aes(date, n, color = term)) +
  geom_point(alpha = 1/3) +
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

# Visualizando variable term en los otros dias de la semana
ggplot(daily, aes(x = wday, y = n, color = term)) +
  geom_boxplot()

# Dado que n en los otros dias de semana tambien se ve afectado por el term, es
# mejor un modelo donde wday y term interactuen
mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 0.75)

# Con estos cambios modelo funciona mejor, pero no tanto. Hay que indagar mas:
grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red", size = 4) +
  facet_wrap(~ term)

# Se ve que modelo predice valor medio, pero que a veces este esta influenciado
# por outliers (en especial en term = fall), y por eso no es representantivo del
# valor tipico.

# Alternativa: usar MASS::rlm() para obtener un modelo robusto a efectos de outliers
mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) +
  geom_line() +
  geom_ref_line(h = 0)

# Otra alternativa: usar splines para no tener que depender de nuestro domain
# knowledge respecto de los school terms de EEUU
library(splines)
mod4 <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod4) %>% 
  ggplot(aes(date, pred, color = wday)) +
  geom_line() +
  geom_point()
