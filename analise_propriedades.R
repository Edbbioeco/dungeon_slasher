# Pacotes ----

library(readxl)

library(tidyverse)

library(performance)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("dados.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

# Modelo do efeito do tipo de partida nas pedras de respiro ----

## Criando o modelo ----

lm_pedras <- lm(`Valores totais` ~ Tipo,
                data = dados)

## Pressupostos do modelo ----

lm_pedras |> performance::check_model(check = c("qq",
                                                "normality",
                                                "homogeneity"))

## Avalaindo o modelo ----

lm_pedras |> summary()

## Gráfiico ----
