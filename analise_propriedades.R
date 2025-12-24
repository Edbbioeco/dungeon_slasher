# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(performance)

library(ggbeeswarm)

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

# ANCOVA do efeito do tipo de partida nas pedras de respiro ----

## Criando o modelo ----

lm_pedras <- lm(`Valores Totais` ~ Tipo * `Quantidade de estagios`,
                data = dados)

## Pressupostos do modelo ----

lm_pedras |> performance::check_heteroscedasticity()

lm_pedras |> performance::check_normality()

lm_pedras |> performance::check_model(check = c("qq",
                                                "normality",
                                                "homogeneity"))

## Avalaindo o modelo ----

lm_pedras |>
  summary()

## Gráfiico ----

## Estatísticas das variáveis ----

summary <- lm_pedras |>
  summary()

tabelaestatisticas <- summary$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  dplyr::filter(!rowname |> stringr::str_detect("Intercept")) |>
  dplyr::mutate(rowname = rowname |> stringr::str_remove_all("`"),
                Tipo = 0.425,
                `Valores Totais` = c(142500,
                                  132500,
                                  122500),
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

## Estatística do modelo global ----

f_global <- paste0("F<sub>",
                   summary$fstatistic[[2]],
                   ", ",
                   summary$fstatistic[[3]],
                   "</sub> = ",
                   summary$fstatistic[[1]] |> round(2),
                   ", p < 0.001, R² = ",
                   summary$adj.r.squared |> round(2))

f_global

## Gráfico ----

dados |>
  ggplot(aes(Tipo, `Valores Totais`, fill = `Quantidade de estagios`)) +
  ggbeeswarm::geom_quasirandom(shape = 21, stroke = 1, size = 5) +
  ggtext::geom_richtext(data = tabelaestatisticas,
                        aes(Tipo, `Valores Totais`, label = estatistica),
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

