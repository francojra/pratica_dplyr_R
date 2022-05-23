
# Prática dplyr ----------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 22/05/22 ---------------------------------------------------------------------------------------------------------------------------

# Baixar pacote ----------------------------------------------------------------------------------------------------------------------------

library(tidyverse) # pacotes ggplot2 para gráficos e dplyr para análises e organização de tabela
library(dados) # Pacote para baixar dados traduzidos
library(forcats) # Pacote para ordenar fatores no gráfico
library(patchwork) # Pacote para juntar gráficos em janela única

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
  filter(nome %in% c("Anna", "Bertha", "Julia", "Estella", 
                     "Anne", "Maria", "Jeanne", "Cora")) %>%
  group_by(nome) %>%
  summarise(mean(prop), sd(prop))
dados_sum_f

# Nomes masculinos

dados_sum_m <- dados %>%
  filter(nome %in% c("Oswaldo", "Calib", "Dalan", "Chistopher",
                     "Kamel", "Martinez", "Vinicius", "Ellison")) %>%
  group_by(nome) %>%
  summarise(mean(prop), sd(prop))
dados_sum_m

### função arrange

# Nomes femininos

dados_sum_f <- dados %>%
  filter(nome %in% c("Anna", "Bertha", "Julia", "Estella", 
                     "Anne", "Maria", "Jeanne", "Cora")) %>%
  group_by(nome) %>%
  summarise(media_prop_f = mean(prop), desvio_f = sd(prop), 
            n = n(), se = desvio_f / sqrt(n)) %>%
  arrange(media_prop_f)
dados_sum_f

# Nomes masculinos

dados_sum_m <- dados %>%
  filter(nome %in% c("Oswaldo", "Calib", "Dalan", "Chistopher",
                     "Kamel", "Martinez", "Vinicius", "Ellison")) %>%
  group_by(nome) %>%
  summarise(media_prop_m = mean(prop), desvio_m = sd(prop),
            n = n(), se = desvio_m / sqrt(n)) %>%
  arrange(media_prop_m)
dados_sum_m

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

# Nomes femininos

gf <- ggplot(dados_sum_f) +
  geom_col(aes(x = fct_reorder(nome, media_prop_f), 
               y = media_prop_f), fill = "#66c2a5") +
  geom_errorbar(aes(x = nome, y = media_prop_f, 
                    ymin = media_prop_f - se,
                    ymax = media_prop_f + se),
                    width = 0.2) +
  labs(x = "Nomes femininos", y = "Proporção") +
  theme_dark()
gf 

# Nomes masculinos

gm <- ggplot(dados_sum_m) +
  geom_col(aes(x = fct_reorder(nome, media_prop_m), 
               y = media_prop_m), fill = "#fc8d62") +
  geom_errorbar(aes(x = nome, y = media_prop_m, 
                    ymin = media_prop_m - se,
                    ymax = media_prop_m + se),
                    width = 0.2) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(x = "Nomes masculinos", y = "Proporção") +
  theme_dark()
gm 

gf + gm
