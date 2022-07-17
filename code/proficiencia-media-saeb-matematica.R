

# PACOTES NECESSARIOS -----------------------------------------------------

library(ggplot2)


# BASE DE DADOS -----------------------------------------------------------

### SAEB --------------------------------------------------------------------


saeb_rs_mt <- readr::read_rds("data/saeb_rs_mt.rds")


### GEOBR -------------------------------------------------------------------

# baixa o mapa do rio grande do sul
mapa_rs <- geobr::read_municipality(code_muni = "RS", year = 2020)

# baixa o mapa das escolas no rio grande do sul
mapa_escolas_rs <- geobr::read_schools(year = 2020) |>
  dplyr::filter(abbrev_state == "RS")

# baixa o mapa da meso regiao central do rio grande do sul
mapa_meso_regcentral <- geobr::read_meso_region(code_meso = 4303, year = 2020)

# baixa o mapa da micro regiao central do rio grande do sul
mapa_micro_regcentral <- geobr::read_micro_region(code_micro = 43018, year = 2020)

# baixa o mapa do municipio de santa maria
mapa_sm <- geobr::read_municipality(code_muni = 4316907, year = 2020)


# TIDY --------------------------------------------------------------------

### BASE - LINGUA PORTUGUESA + RS  ------------------------------------------

base_rs_mt <- saeb_rs_mt |>
  dplyr::right_join(mapa_escolas_rs, by = c("id_escola" = "code_school")) |>
  dplyr::select(-abbrev_state) |>
  tidyr::drop_na() |>
  dplyr::mutate(geometria = stringr::str_remove_all(geometria, "POINT ")) |>
  tidyr::separate(
  col = "geometria",
  into = c("lat", "lng"),
  sep = " "
  ) |>
  dplyr::mutate(
    faixa = cut_number(proficiencia_media, n = 5),
    media = round(mean(saeb_rs_mt$proficiencia_media), 2),
    label = dplyr::case_when(
      proficiencia_media == max(proficiencia_media) ~ "Maior nota",
      TRUE ~ ""
    ),
    lat = as.numeric(stringr::str_remove(lat, "\\(")),
    lng = as.numeric(stringr::str_remove(lng, "\\)"))
  )

### BASE - MATEMATICA + MESO REGIAO CENTRAL DO RS ---------------------------

base_meso_regcentral_mt <- base_rs_mt |>
  dplyr::filter(
    # microrregiao de santiago
    name_muni == "Capão do Cipó" |
      name_muni == "Itacurubi" |
      name_muni == "Jari" |
      name_muni == "Júlio de Castilhos" |
      name_muni == "Pinhal Grande" |
      name_muni == "Quevedos" |
      name_muni == "Santiago" |
      name_muni == "Tupanciretã" |
      name_muni == "Unistalda" |
      # microrregiao de santa maria
      name_muni == "Cacequi" |
      name_muni == "Dilermando de Aguiar" |
      name_muni == "Itaara" |
      name_muni == "Jaguari" |
      name_muni == "Mata" |
      name_muni == "Nova Esperança do Sul" |
      name_muni == "Santa Maria" |
      name_muni == "São Martinho da Serra" |
      name_muni == "São Pedro do Sul" |
      name_muni == "São Sepé" |
      name_muni == "São Vicente do Sul" |
      name_muni == "Toropi" |
      name_muni == "Vila Nova do Sul" |
      # microrregiao de restinga seca
      name_muni =="Agudo" |
      name_muni == "Dona Francisca" |
      name_muni == "Faxinal do Soturno" |
      name_muni == "Formigueiro" |
      name_muni == "Ivorá" |
      name_muni == "Nova Palma" |
      name_muni == "Restinga Sêca" |
      name_muni == "São João do Polêsine" |
      name_muni == "Silveira Martins"
  ) |>
  dplyr::mutate(
    label = dplyr::case_when(
      proficiencia_media == max(proficiencia_media) ~ "Maior nota",
      TRUE ~ ""
    )
  )

### BASE - LINGUA PORTUGUESA + MICRO REGIAO CENTRAL DO RS -------------------

base_micro_regcentral_mt <- base_meso_regcentral_mt |>
  dplyr::filter(
    name_muni == "Cacequi" |
      name_muni == "Dilermando de Aguiar" |
      name_muni == "Itaara" |
      name_muni == "Jaguari" |
      name_muni == "Mata" |
      name_muni == "Nova Esperança do Sul" |
      name_muni == "Santa Maria" |
      name_muni == "São Martinho da Serra" |
      name_muni == "São Pedro do Sul" |
      name_muni == "São Sepé" |
      name_muni == "São Vicente do Sul" |
      name_muni == "Toropi" |
      name_muni == "Vila Nova do Sul"
  ) |>
  dplyr::mutate(
    label = dplyr::case_when(
      proficiencia_media == max(proficiencia_media) ~ "Maior nota",
      TRUE ~ ""
    )
  )


### BASE - LINGUA PORTUGUESA + SANTA MARIA ----------------------------------

base_sm_mt <- base_rs_mt |>
  dplyr::filter(name_muni == "Santa Maria") |>
  dplyr::mutate(
    label = dplyr::case_when(
      proficiencia_media == max(proficiencia_media) ~ "Maior nota",
      TRUE ~ ""
    )
  )

# ADICIONA FONTE ----------------------------------------------------------

sysfonts::font_add_google(name = "Roboto", family = "Roboto")
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto()


# VISUALIZACAO ------------------------------------------------------------

### LINGUA PORTUGUESA + RIO GRANDE DO SUL -----------------------------------

mapa_rs_mt <- ggplot() +
  geom_sf(
    data = mapa_rs,
    aes(geometry = geom),
    fill = "white",
    color = "grey80",
    size = 0.01
  ) +
  geom_sf(
    data = mapa_meso_regcentral,
    aes(geometry = geom),
    fill = "#C4DAEF",
    size = 0.5
  ) +
  geom_segment(
    data = base_rs_mt,
    aes(
      x = -52.01591,
      y = -27.95583,
      xend = -50.8,
      yend = -27.3
    ),
    color = "black"
  ) +
  geom_sf(
    data = base_rs_mt,
    aes(
      geometry = geom,
      color = factor(faixa)
      # color = forcats::fct_rev(faixa)
    ),
    size = 0.5
  ) +
  geom_label(
    data = base_rs_mt,
    aes(
      x = -50.8,
      y = -27.3,
      label = "Maior nota",
      fontface = "bold"),
    fill = "#CA6EE0",
    color = "white",
    size = 4
  ) +
  scale_color_manual(
    values = c("#B7F0AC", "#76C897", "#59C4FC", "#9796FE", "#CA6EE0"),
    labels = c("195", "277", "300", "325", "495"),
    guide = guide_legend(),
    name = "Nota da prova"
  ) +
  labs(
    title = "Proficiência média por escolas no Rio Grande do Sul",
    subtitle = "Resultados geolocalizados da prova de Matemática do Saeb em 2019",
    caption = "Fonte: Base dos Dados -- Inep (Saeb); geobr (Geolocalização)"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.justification = c("right", "bottom"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.position = c(1, 0.05),
    legend.text.align = 0,
    plot.caption = element_text(size = 10),
    # plot.margin = unit(c(1.5, 1, 1, 1), "cm")
  )


### LINGUA PORTUGUESA + MESO REGIAO CENTRAL ---------------------------------

mapa_meso_mt <- ggplot() +
  geom_sf(
    data = mapa_meso_regcentral,
    aes(geometry = geom),
    fill = "white",
    color = "grey80",
    size = 0.01
  ) +
  geom_sf(
    data = mapa_micro_regcentral,
    aes(geometry = geom),
    fill = "#C4DAEF",
    size = 0.5
  ) +
  geom_segment(
    data = base_meso_regcentral_mt,
    aes(
      x = -54.17700,
      y = -29.61373,
      xend = -54.17700,
      yend = -29.8
    ),
    color = "black"
  ) +
  geom_sf(
    data = base_meso_regcentral_mt,
    aes(
      geometry = geom,
      color = factor(faixa)
    ),
    size = 0.5
  ) +
  geom_label(
    data = base_meso_regcentral_mt,
    aes(
      x = -54.17700,
      y = -29.8,
      label = "Maior nota",
      fontface = "bold"),
    fill = "#CA6EE0",
    color = "white",
    size = 4.5
  ) +
  scale_color_manual(
    values = c("#B7F0AC", "#76C897", "#59C4FC", "#9796FE", "#CA6EE0"),
    labels = c("195", "277", "300", "325", "495"),
    guide = guide_legend(),
    name = "Nota da prova"
  ) +
  labs(
    title = "Proficiência média por escolas na \nMesorregião do Centro Ocidental Rio-Grandense",
    subtitle = "Resultados geolocalizados da prova de Matemática do Saeb em 2019",
    caption = "Fonte: Base dos Dados -- Inep (Saeb); geobr (Geolocalização)"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.justification = c("right", "bottom"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.position = c(1, 0.01),
    legend.text.align = 0,
    plot.caption = element_text(size = 10),
    # plot.margin = unit(c(0.3, 1, 1, 1), "cm")
  )


### LINGUA PORTUGUESA + MICRO REGIAO CENTRAL --------------------------------

mapa_micro_mt <- ggplot() +
  geom_sf(
    data = mapa_micro_regcentral,
    aes(geometry = geom),
    fill = "white",
    color = "grey50",
    size = 0.01
  ) +
  geom_sf(
    data = mapa_sm,
    aes(geometry = geom),
    fill = "#C4DAEF",
    size = 0.5
  ) +
  geom_segment(
    data = base_micro_regcentral_mt,
    aes(
      x = -54.17700,
      y = -29.61373,
      xend = -54.17700,
      yend = -29.7
    ),
    color = "black"
  ) +
  geom_sf(
    data = base_micro_regcentral_mt,
    aes(
      geometry = geom,
      color = factor(faixa)
    ),
    size = 0.5
  ) +
  geom_label(
    data = base_micro_regcentral_mt,
    aes(
      x = -54.17700,
      y = -29.7,
      label = "Maior nota",
      fontface = "bold"),
    fill = "#CA6EE0",
    color = "white",
    size = 4.5
  ) +
  scale_color_manual(
    values = c("#B7F0AC", "#76C897", "#59C4FC", "#9796FE", "#CA6EE0"),
    labels = c("195", "277", "300", "325", "495"),
    guide = guide_legend(),
    name = "Nota da \nprova"
  ) +
  labs(
    title = "Proficiência média por escolas na Microrregião de Santa Maria",
    subtitle = "Resultados geolocalizados da prova de Matemática do Saeb em 2019",
    caption = "Fonte: Base dos Dados -- Inep (Saeb); geobr (Geolocalização)"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.justification = c("right", "bottom"),
    legend.position = c(0.97, 0.05),
    legend.spacing.x = unit(0.1, "cm"),
    legend.text.align = 0,
    plot.caption = element_text(size = 10),
    # plot.margin = unit(c(0.3, 1, 1, 1), "cm")
  )


### LINGUA PORTUGUESA + SANTA MARIA -----------------------------------------

mapa_sm_mt <- ggplot() +
  geom_sf(
    data = mapa_sm,
    aes(geometry = geom),
    fill = "white",
    color = "grey50",
    size = 0.01
  ) +
  geom_segment(
    data = base_sm_mt,
    aes(
      x = -53.86225,
      y = -29.69750,
      xend = -53.9,
      yend = -29.73
    ),
    color = "black"
  ) +
  geom_sf(
    data = base_sm_mt,
    aes(
      geometry = geom,
      color = factor(faixa)
    ),
    size = 1
  ) +
  geom_label(
    data = base_sm_mt,
    aes(
      x = -53.9,
      y = -29.73,
      label = "Maior nota",
      fontface = "bold"),
    fill = "#CA6EE0",
    color = "white",
    size = 4.5
  ) +
  scale_color_manual(
    values = c("#B7F0AC", "#76C897", "#59C4FC", "#9796FE", "#CA6EE0"),
    labels = c("195", "277", "300", "325", "495"),
    guide = guide_legend(),
    name = "Nota da \nprova"
  ) +
  labs(
    title = "Proficiência média por escolas na cidade de Santa Maria",
    subtitle = "Resultados geolocalizados da prova de Língua Portuguesa do Saeb em 2019",
    caption = "Fonte: Base dos Dados -- Inep (Saeb); geobr (Geolocalização)"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.justification = c("right", "bottom"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.position = c(0.95, 0.05),
    legend.text.align = 0,
    plot.caption = element_text(size = 10),
    # plot.margin = unit(c(0.3, 1, 1, 1), "cm")
  )


### LINGUA PORTUGUESA + SANTA MARIA (CENTRO) --------------------------------

mapa_sm_central_mt <- ggplot() +
  geom_sf(
    data = mapa_sm,
    aes(geometry = geom),
    fill = "white",
    color = "grey50",
    size = 0.01
  ) +
  geom_segment(
    data = base_sm_mt,
    aes(
      x = -53.86225,
      y = -29.69750,
      xend = -53.9,
      yend = -29.71
    ),
    color = "black"
  ) +
  geom_sf(
    data = base_sm_mt,
    aes(
      geometry = geom,
      color = factor(faixa)
    ),
    size = 1.5
  ) +
  coord_sf(
    xlim = c(-53.95, -53.65),
    ylim = c(-29.75, - 29.67)
  ) +
  geom_label(
    data = base_sm_mt,
    aes(
      x = -53.9,
      y = -29.71,
      label = "Maior nota",
      fontface = "bold"),
    fill = "#CA6EE0",
    color = "white",
    size = 4.5
  ) +
  scale_color_manual(
    values = c("#B7F0AC", "#76C897", "#59C4FC", "#9796FE", "#CA6EE0"),
    labels = c("195", "277", "300", "325", "495"),
    guide = guide_legend(),
    name = "Nota da prova"
  ) +
  labs(
    title = "Proficiência média por escolas na cidade de Santa Maria",
    subtitle = "Resultados geolocalizados da prova de Matemática do Saeb em 2019",
    caption = "Fonte: Base dos Dados -- Inep (Saeb); geobr (Geolocalização)"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.justification = c("right", "bottom"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.position = c(0.96, 0.05),
    legend.text.align = 0,
    plot.caption = element_text(size = 10),
    # plot.margin = unit(c(0.3, 1, 1, 1), "cm")
  )


# SALVANDO OS GRAFICOS ----------------------------------------------------

### MAPA RIO GRANDE DO SUL --------------------------------------------------

ggsave(
  filename = "mapa_rs_mt.png",
  plot = mapa_rs_mt,
  path = "output/img/matematica",
  width = 1800,
  height = 1800,
  units = "px",
  dpi = 300
)


### MAPA MESO REGIAO CENTRAL DO RS ------------------------------------------

ggsave(
  filename = "mapa_meso_mt.png",
  plot = mapa_meso_mt,
  path = "output/img/matematica",
  width = 1800,
  height = 1800,
  units = "px",
  dpi = 300
)


### MAPA MICRO REGIAO CENTRAL DO RS -----------------------------------------

ggsave(
  filename = "mapa_micro_mt.png",
  plot = mapa_micro_mt,
  path = "output/img/matematica",
  width = 1800,
  height = 1800,
  units = "px",
  dpi = 300
)


### MAPA SANTA MARIA  -------------------------------------------------------

ggsave(
  filename = "mapa_sm_mt.png",
  plot = mapa_sm_mt,
  path = "output/img/matematica",
  width = 1800,
  height = 1800,
  units = "px",
  dpi = 300
)


### MAPA SANTA MARIA (CENTRAL) ----------------------------------------------

ggsave(
  filename = "mapa_sm_central_mt.png",
  plot = mapa_sm_central_mt,
  path = "output/img/matematica",
  width = 1800,
  height = 1800,
  units = "px",
  dpi = 300
)
