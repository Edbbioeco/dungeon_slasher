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

# ANCOVA do efeito do tipo de partida nas pedras de respiro ----

## Criando o modelo ----

lm_pedras <- lm(`Valores Totais` ~ Tipo * `Quantidade de estagios`,
                data = dados)

## Pressupostos do modelo ----

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
  dplyr::mutate(Tipo = 1.45,
                `Valores Totais` = c(140000,
                                  130000,
                                  120000),
                `Quantidade de estagios` = NA,
                estatistica = paste0(rowname,
                                     ": β1 ± SE = ",
                                     Estimate |> round(2),
                                     " ± ",
                                     `Std. Error` |> round(2),
                                     ", t = ",
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
                   ", p < 0.05, R² = ",
                   summary$adj.r.squared |> round(2))

f_global

## Gráfico ----

dados |>
  ggplot(aes(Tipo, `Valores Totais`, fill = `Quantidade de estagios`)) +
  ggbeeswarm::geom_quasirandom(shape = 21, stroke = 1, size = 5) +
  geom_text(data = tabelaestatisticas,
            aes(Tipo, `Valores Totais`, label = estatistica),
            size = 4.5,
            fontface = "bold") +
  scale_fill_viridis_c(guide = guide_colourbar(title.hjust = 0.5,
                                               barheight = 20)) +
  labs(title = f_global) +
  ggview::canvas(height = 10, width = 12) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 17.5),
        axis.title = element_text(color = "black", size = 17.5),
        legend.text = element_text(color = "black", size = 17.5),
        legend.title = element_text(color = "black", size = 17.5),
        plot.title = ggtext::element_markdown(color = "black", size = 17.5))

