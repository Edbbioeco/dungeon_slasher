# Pacotes ----

library(readxl)

library(tidyverse)

library(performance)

library(ggview)

library(ggtext)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("dados.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

## Tratamdo ---

dados %<>%
  tidyr::drop_na()

dados

# Efeito do tempo de jogo nos valores de pedra de respiro ----

## Criando o modelo ----

lm_tempo <- lm(`Valores Totais` ~ Tempo * Tipo,
                data = dados)

## Pressupostos do modelo ----

lm_tempo |> performance::check_heteroscedasticity()

lm_tempo |> performance::check_normality()

lm_tempo |> performance::check_model(check = c("qq",
                                               "normality",
                                               "homogeneity"))

## Avalaindo o modelo ----

lm_pedras |>
  summary()

## Gráfico ----

dados |>
  ggplot(aes(Tempo, `Valores Totais`, fill = Tipo, color = Tipo)) +
  geom_point(shape = 21, stroke = 1, color = "black", size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Tipo, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("orangered",
                               "forestgreen")) +
  scale_color_manual(values = c("orangered",
                                "forestgreen")) +
  ggview::canvas(height = 10, width = 12) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "none",
        plot.title = ggtext::element_markdown(color = "black", size = 20),
        strip.background = element_rect(fill = "gray70"),
        strip.text = element_text(color = "black", size = 20))

# Efeito do tipo de estágio no tempo de jogo ----

## Tratando os dados ----

dados_trat <- dados |>
  dplyr::mutate(Tempo = c(Tempo |> lubridate::hour() * 60 +
                            Tempo |> lubridate::minute() +
                            Tempo |> lubridate::second() / 60))

dados_trat

dados_trat |> dplyr::glimpse()

## Criando o modelo ----

lm_tempo_tipo <- lm(Tempo ~ Tipo,
                    data = dados_trat)

## Pressupostos do modelo ----

lm_tempo_tipo |> performance::check_heteroscedasticity()

lm_tempo_tipo |> performance::check_normality()

lm_tempo_tipo |> performance::check_model(check = c("qq",
                                                    "normality",
                                                    "homogeneity"))

## Avalaindo o modelo ----

lm_tempo_tipo |>
  summary()

## Gráfico ----
