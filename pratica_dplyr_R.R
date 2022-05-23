
# Prática dplyr ----------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 22/05/22 ---------------------------------------------------------------------------------------------------------------------------

# Baixar pacote ----------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(dados)

# Baixar dados -----------------------------------------------------------------------------------------------------------------------------

dados <- dados::bebes
view(dados)

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
  summarise(mean(prop))
dados_sum_f

# Nomes masculinos

dados_sum_m <- dados %>%
  filter(nome %in% c("Bob", "Calib", "Dalan", "Chistopher",
                     "Kamel", "Martinez", "Vinicius", "Ellison")) %>%
  group_by(nome) %>%
  summarise(mean(prop))
dados_sum_m


