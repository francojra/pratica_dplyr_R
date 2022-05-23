
# Prática dplyr ----------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 22/05/22 ---------------------------------------------------------------------------------------------------------------------------

# Baixar pacote ----------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(dados)
library(forcats)

# Baixar dados -----------------------------------------------------------------------------------------------------------------------------

dados <- dados::bebes
view(dados)
glimpse(dados)
dados$nome <- as.factor(dados$nome)

# Análises ---------------------------------------------------------------------------------------------------------------------------------

### função filter

dados_fem <- dados %>%
  filter(sexo == "F", ano == 1996, n <= 20)
view(dados_fem)  

dados_mas <- dados %>%
  filter(sexo == "M", ano == 1996, n <= 20)
view(dados_mas)

### Função select

dados_fem <- dados_fem %>%
  select(nome, n, prop)
view(dados_fem)

dados_mas <- dados_mas %>%
  select(nome, n, prop)
view(dados_mas)

### função summarise

# Nomes femininos

dados_sum_f <- dados %>%
  filter(nome %in% c("Anna", "Bibi", "Cydnie", "Estella", 
                     "Anne", "Maria", "Jeanne", "Cora")) %>%
  group_by(nome) %>%
  summarise(mean(prop), sd(prop))
dados_sum_f

# Nomes masculinos

dados_sum_m <- dados %>%
  filter(nome %in% c("Bob", "Calib", "Dalan", "Chistopher",
                     "Kamel", "Martinez", "Vinicius", "Ellison")) %>%
  group_by(nome) %>%
  summarise(mean(prop), sd(prop))
dados_sum_m

### função arrange

# Nomes femininos

dados_sum_f <- dados %>%
  filter(nome %in% c("Anna", "Bibi", "Cydnie", "Estella", 
                     "Anne", "Maria", "Jeanne", "Cora")) %>%
  group_by(nome) %>%
  summarise(media_prop_f = mean(prop), desvio_f = sd(prop), 
            n = n(), se = desvio_f / sqrt(n)) %>%
  arrange(media_prop_f)
dados_sum_f

# Nomes masculinos

dados_sum_m <- dados %>%
  filter(nome %in% c("Bob", "Calib", "Dalan", "Chistopher",
                     "Kamel", "Martinez", "Vinicius", "Ellison")) %>%
  group_by(nome) %>%
  summarise(media_prop_m = mean(prop), desvio_m = sd(prop),
            n = n(), se = desvio_m / sqrt(n)) %>%
  arrange(media_prop_m)
dados_sum_m

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

gf <- ggplot(dados_sum_f) +
  geom_col(aes(x = fct_reorder(nome, media_prop_f), 
               y = media_prop_f), fill = "#66c2a5") +
  geom_errorbar(aes(x = nome, y = media_prop_f, 
                    ymin = media_prop_f - se,
                    ymax = media_prop_f + se),
                    width = 0.2)
gf 
