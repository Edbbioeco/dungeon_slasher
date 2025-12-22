# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(performance)

library(ggbeeswarm)

library(ggview)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("dados.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

## Tratamdo ---

dados %<>%
  tidyr::drop_na()

# Modelo do efeito do tipo de partida nas pedras de respiro ----

## Criando o modelo ----

lm_pedras <- lm(`Valores Totais` ~ Tipo * `Quantidade de estagios`,
                data = dados)

## Pressupostos do modelo ----

lm_pedras |> performance::check_model(check = c("qq",
                                                "normality",
                                                "homogeneity"))

## Avalaindo o modelo ----

lm_pedras |> summary()

## Gráfiico ----

dados |>
  ggplot(aes(Tipo, `Valores Totais`, fill = `Quantidade de estagios`)) +
  ggbeeswarm::geom_quasirandom(shape = 21, stroke = 1, size = 5) +
  scale_fill_viridis_c(guide = guide_colourbar(title.hjust = 0.5,
                                               barheight = 20)) +
  ggview::canvas(height = 10, width = 12) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 17.5),
        axis.title = element_text(color = "black", size = 17.5),
        legend.text = element_text(color = "black", size = 17.5),
        legend.title = element_text(color = "black", size = 17.5))
