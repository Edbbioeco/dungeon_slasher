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

# Efeito do tipo de estágio na quantidade inimigos mortos ----

## Criando o modelo ----

modelo_inimigos <- lm(`Inimigos mortos` ~ Tipo,
                      data = dados)

## Pressupostos do modelo ----

modelo_inimigos |> performance::check_heteroscedasticity()

modelo_inimigos |> performance::check_normality()

modelo_inimigos |> performance::check_model(check = c("qq",
                                               "normality",
                                               "homogeneity"))

## Avalaindo o modelo ----

modelo_inimigos |>
  summary()

## Gráfico ----

### Estatísticas das variáveis ----

summary <- modelo_inimigos |>
  summary()

tabelaestatisticas <- summary$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  dplyr::filter(!rowname |> stringr::str_detect("Intercept")) |>
  dplyr::mutate(Tipo = 0.425,
                `Inimigos mortos` = 1800,
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
  ggplot(aes(Tipo, `Inimigos mortos`, fill = `Quantidade de estagios`)) +
  ggbeeswarm::geom_quasirandom(shape = 21, stroke = 1, size = 5) +
  ggtext::geom_richtext(data = tabelaestatisticas,
                        aes(Tipo, `Inimigos mortos`, label = estatistica),
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
