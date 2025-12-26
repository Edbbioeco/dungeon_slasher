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

### Estatística do modelo global ----

f_global <- paste0("F<sub>",
                   summary$fstatistic[[2]],
                   ", ",
                   summary$fstatistic[[3]],
                   "</sub> = ",
                   summary$fstatistic[[1]] |> round(2),
                   ", p = < 0.01, R² = ",
                   summary$adj.r.squared |> round(2))

f_global

### Gráfico ----

dados |>
  ggplot(aes(Tempo, `Valores Totais`, fill = Tipo, color = Tipo)) +
  geom_point(shape = 21, stroke = 1, color = "black", size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Tipo, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("orangered",
                               "forestgreen")) +
  scale_color_manual(values = c("orangered",
                                "forestgreen")) +
  labs(title = f_global) +
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

### Estatísticas das variáveis ----

summary <- lm_tempo_tipo |>
  summary()

tabelaestatisticas <- summary$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  dplyr::filter(!rowname |> stringr::str_detect("Intercept")) |>
  dplyr::mutate(Tipo = 0.425,
                Tempo = "1899-12-31 01:30:00" |> lubridate::as_datetime(),
                `Quantidade de estagios` = NA,
                estatistica = paste0(rowname,
                                     ":<br>β1 ± SE = ",
                                     Estimate |> round(2),
                                     " ± ",
                                     `Std. Error` |> round(2),
                                     ",<br>t = ",
                                     `t value` |> round(2),
                                     ", p = ",
                                     `Pr(>|t|)` |> round(2))) |>
  dplyr::select(6:9)

tabelaestatisticas

### Estatística do modelo global ----

f_global <- paste0("F<sub>",
                   summary$fstatistic[[2]],
                   ", ",
                   summary$fstatistic[[3]],
                   "</sub> = ",
                   summary$fstatistic[[1]] |> round(2),
                   ", p = 0.054, R² = ",
                   summary$adj.r.squared |> round(2))

f_global

### Gráfico ----

dados |>
  ggplot(aes(Tipo, Tempo, fill = `Quantidade de estagios`)) +
  ggbeeswarm::geom_quasirandom(shape = 21, stroke = 1, size = 5) +
  ggtext::geom_richtext(data = tabelaestatisticas,
                        aes(Tipo, Tempo, label = estatistica),
                        fontface = "bold",
                        fill = NA,
                        size = 5,
                        hjust = 0,
                        label.color = NA) +
  scale_fill_viridis_c(guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 25,
                                               frame.colour = "black",
                                               frame.linewidth = 1,
                                               ticks.colour = "black",
                                               ticks.linewidth = 1),
                       breaks = seq(15, 24, 1)) +
  labs(title = f_global) +
  ggview::canvas(height = 10, width = 12) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 17.5),
        axis.title = element_text(color = "black", size = 17.5),
        legend.text = element_text(color = "black", size = 17.5),
        legend.title = element_text(color = "black", size = 17.5),
        legend.position = "bottom",
        plot.title = ggtext::element_markdown(color = "black", size = 20))
