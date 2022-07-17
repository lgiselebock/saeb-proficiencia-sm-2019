

# PACOTES NECESSARIOS -----------------------------------------------------

library(ggplot2)


# BASE DE DADOS -----------------------------------------------------------

### SAEB

saeb_rs_lp <- readr::read_rds("data/saeb_rs_lp.rds")


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

base_rs_lp <- saeb_rs_lp |>
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
    # faixa = factor(dplyr::case_when(
    #   # proficiencia_media == 188.6 ~ 188.6,
    #   proficiencia_media >= 188.6 & proficiencia_media <= 279.8 ~ 279.8,
    #   proficiencia_media > 279.8 & proficiencia_media <= 298.6 ~ 298.6,
    #   proficiencia_media > 298.6 & proficiencia_media <= 316.1 ~ 316.1,
    #   proficiencia_media > 316.1 & proficiencia_media < 483.89 ~ 483.9,
    #   )
    # ),
    media = round(mean(saeb_rs_lp$proficiencia_media), 2),
    label = dplyr::case_when(
      proficiencia_media == max(proficiencia_media) ~ "Maior nota",
      TRUE ~ ""
    ),
    lat = as.numeric(stringr::str_remove(lat, "\\(")),
    lng = as.numeric(stringr::str_remove(lng, "\\)"))
  )

### BASE - LINGUA PORTUGUESA + MESO REGIAO CENTRAL DO RS --------------------

base_meso_regcentral_lp <- base_rs_lp |>
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

base_micro_regcentral_lp <- base_meso_regcentral_lp |>
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

base_sm_lp <- base_rs_lp |>
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

mapa_rs_lp <- ggplot() +
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
    data = base_rs_lp,
    aes(
      x = -51.66045,
      y = -27.770127,
      xend = -50,
      yend = -27
    ),
    color = "black"
  ) +
  geom_sf(
    data = base_rs_lp,
    aes(
      geometry = geom,
      color = factor(faixa)
      # color = forcats::fct_rev(faixa)
    ),
    size = 0.5
  ) +
  geom_label(
    data = base_rs_lp,
    aes(
      x = -50.5,
      y = -27,
      label = "Maior nota",
      fontface = "bold"),
    fill = "#CA6EE0",
    color = "white",
    size = 4
  ) +
  scale_color_manual(
    values = c("#B7F0AC", "#76C897", "#59C4FC", "#9796FE", "#CA6EE0"),
    # values = c("#CA6EE0", "#9796FE", "#59C4FC", "#76C897", "#B7F0AC"),
    # values = c(
    #   "484" = "#CA6EE0",
    #   "322" = "#9796FE",
    #   "292" = "#59C4FC",
    #   "275" = "#76C897",
    #   "188" = "#B7F0AC"
    # ),
    # breaks = c("483.9", "316.1", "298.6", "279.8", "188.6"),
    labels = c("188", "275", "292", "322", "484"),
    # labels = c("484", "322", "292", "275", "188"),
    guide = guide_legend(),
    name = "Nota da prova"
  ) +
  labs(
    title = "Proficiência média por escolas no Rio Grande do Sul",
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
    legend.position = c(1, 0.05),
    legend.text.align = 0,
    plot.caption = element_text(size = 10),
    # plot.margin = unit(c(1.5, 1, 1, 1), "cm")
  )


### LINGUA PORTUGUESA + MESO REGIAO CENTRAL ---------------------------------

mapa_meso_lp <- ggplot() +
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
    data = base_meso_regcentral_lp,
    aes(
      x = -53.8030,
      y = -29.7050,
      xend = -53.67,
      yend = -29.87
    ),
    color = "black"
  ) +
  geom_sf(
    data = base_meso_regcentral_lp,
    aes(
      geometry = geom,
      color = factor(faixa)
    ),
    size = 0.5
  ) +
  geom_label(
    data = base_meso_regcentral_lp,
    aes(
      x = -53.67,
      y = -29.87,
      label = "Maior nota",
      fontface = "bold"),
    fill = "#CA6EE0",
    color = "white",
    size = 4.5
  ) +
  scale_color_manual(
    values = c("#B7F0AC", "#76C897", "#59C4FC", "#9796FE", "#CA6EE0"),
    # values = c("#CA6EE0", "#9796FE", "#59C4FC", "#76C897", "#B7F0AC"),
    # values = c(
    #   "483.9" = "#CA6EE0",
    #   "316.1" = "#9796FE",
    #   "298.6" = "#59C4FC",
    #   "279.8" = "#76C897",
    #   "188.6" = "#B7F0AC"
    # ),
    # breaks = c("483.9", "316.1", "298.6", "279.8", "188.6"),
    labels = c("188", "275", "292", "322", "484"),
    # labels = c("484", "322", "292", "275", "188"),
    guide = guide_legend(),
    name = "Nota da prova"
  ) +
  labs(
    title = "Proficiência média por escolas na \nMesorregião do Centro Ocidental Rio-Grandense",
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
    legend.position = c(1, 0.01),
    legend.text.align = 0,
    plot.caption = element_text(size = 10),
    # plot.margin = unit(c(0.3, 1, 1, 1), "cm")
  )


### LINGUA PORTUGUESA + MICRO REGIAO CENTRAL --------------------------------

mapa_micro_lp <- ggplot() +
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
    data = base_micro_regcentral_lp,
    aes(
      x = -53.8030,
      y = -29.7050,
      xend = -53.67,
      yend = -29.82
    ),
    color = "black"
  ) +
  geom_sf(
    data = base_micro_regcentral_lp,
    aes(
      geometry = geom,
      color = factor(faixa)
    ),
    size = 0.5
  ) +
  geom_label(
    data = base_micro_regcentral_lp,
    aes(
      x = -53.67,
      y = -29.82,
      label = "Maior nota",
      fontface = "bold"),
    fill = "#CA6EE0",
    color = "white",
    size = 4.5
  ) +
  scale_color_manual(
    values = c("#B7F0AC", "#76C897", "#59C4FC", "#9796FE", "#CA6EE0"),
    # values = c("#CA6EE0", "#9796FE", "#59C4FC", "#76C897", "#B7F0AC"),
    # values = c(
    #   "483.9" = "#CA6EE0",
    #   "316.1" = "#9796FE",
    #   "298.6" = "#59C4FC",
    #   "279.8" = "#76C897",
    #   "188.6" = "#B7F0AC"
    # ),
    # breaks = c("483.9", "316.1", "298.6", "279.8", "188.6"),
    labels = c("188", "275", "292", "322", "484"),
    # labels = c("484", "322", "292", "275", "188"),
    guide = guide_legend(),
    name = "Nota da \nprova"
  ) +
  labs(
    title = "Proficiência média por escolas na Microrregião de Santa Maria",
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
    legend.position = c(0.97, 0.05),
    legend.spacing.x = unit(0.1, "cm"),
    legend.text.align = 0,
    plot.caption = element_text(size = 10),
    # plot.margin = unit(c(0.3, 1, 1, 1), "cm")
  )


### LINGUA PORTUGUESA + SANTA MARIA -----------------------------------------

mapa_sm_lp <- ggplot() +
  geom_sf(
    data = mapa_sm,
    aes(geometry = geom),
    fill = "white",
    color = "grey50",
    size = 0.01
  ) +
  geom_segment(
    data = base_sm_lp,
    aes(
      x = -53.8030,
      y = -29.7050,
      xend = -53.7,
      yend = -29.77
    ),
    color = "black"
  ) +
  geom_sf(
    data = base_sm_lp,
    aes(
      geometry = geom,
      color = factor(faixa)
    ),
    size = 1
  ) +
  geom_label(
    data = base_sm_lp,
    aes(
      x = -53.7,
      y = -29.77,
      label = "Maior nota",
      fontface = "bold"),
    fill = "#CA6EE0",
    color = "white",
    size = 4.5
  ) +
  scale_color_manual(
    values = c("#B7F0AC", "#76C897", "#59C4FC", "#9796FE", "#CA6EE0"),
    # values = c("#CA6EE0", "#9796FE", "#59C4FC", "#76C897", "#B7F0AC"),
    # values = c(
    #   "483.9" = "#CA6EE0",
    #   "316.1" = "#9796FE",
    #   "298.6" = "#59C4FC",
    #   "279.8" = "#76C897",
    #   "188.6" = "#B7F0AC"
    # ),
    # breaks = c("483.9", "316.1", "298.6", "279.8", "188.6"),
    labels = c("188", "275", "292", "322", "484"),
    # labels = c("484", "322", "292", "275", "188"),
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

mapa_sm_central_lp <- ggplot() +
  geom_sf(
    data = mapa_sm,
    aes(geometry = geom),
    fill = "white",
    color = "grey50",
    size = 0.01
  ) +
  geom_segment(
    data = base_sm_lp,
    aes(
      x = -53.8030,
      y = -29.7050,
      xend = -53.77,
      yend = -29.72
    ),
    color = "black"
  ) +
  geom_sf(
    data = base_sm_lp,
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
  scale_color_manual(
    values = c("#B7F0AC", "#76C897", "#59C4FC", "#9796FE", "#CA6EE0"),
    # values = c("#CA6EE0", "#9796FE", "#59C4FC", "#76C897", "#B7F0AC"),
    # values = c(
    #   "483.9" = "#CA6EE0",
    #   "316.1" = "#9796FE",
    #   "298.6" = "#59C4FC",
    #   "279.8" = "#76C897",
    #   "188.6" = "#B7F0AC"
    # ),
    # breaks = c("483.9", "316.1", "298.6", "279.8", "188.6"),
    labels = c("188", "275", "292", "322", "484"),
    # labels = c("484", "322", "292", "275", "188"),
    guide = guide_legend(),
    name = "Nota da prova"
  ) +
  # foi colocado para cima
  # geom_segment(
  #   data = base_sm_lp,
  #   aes(
  #     x = -53.8030,
  #     y = -29.7050,
  #     xend = -53.77,
  #     yend = -29.72
  #   ),
  #   color = "grey80"
  # ) +
geom_label(
  data = base_sm_lp,
  aes(
    x = -53.77,
    y = -29.72,
    label = "Maior nota",
    fontface = "bold"),
  fill = "#CA6EE0",
  color = "white",
  size = 4.5
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
    legend.position = c(1, 0.05),
    legend.text.align = 0,
    plot.caption = element_text(size = 10),
    # plot.margin = unit(c(0.3, 1, 1, 1), "cm")
  )


# SALVANDO OS GRAFICOS ----------------------------------------------------

### MAPA RIO GRANDE DO SUL --------------------------------------------------

ggsave(
  filename = "mapa_rs_lp.png",
  plot = mapa_rs_lp,
  path = "output/img/",
  width = 1800,
  height = 1800,
  units = "px",
  dpi = 300
)


### MAPA MESO REGIAO CENTRAL DO RS ------------------------------------------

ggsave(
  filename = "mapa_meso_lp.png",
  plot = mapa_meso_lp,
  path = "output/img/",
  width = 1800,
  height = 1800,
  units = "px",
  dpi = 300
)


### MAPA MICRO REGIAO CENTRAL DO RS -----------------------------------------

ggsave(
  filename = "mapa_micro_lp.png",
  plot = mapa_micro_lp,
  path = "output/img/",
  width = 1800,
  height = 1800,
  units = "px",
  dpi = 300
)


### MAPA SANTA MARIA  -------------------------------------------------------

ggsave(
  filename = "mapa_sm_lp.png",
  plot = mapa_sm_lp,
  path = "output/img/",
  width = 1800,
  height = 1800,
  units = "px",
  dpi = 300
)


### MAPA SANTA MARIA (CENTRAL) ----------------------------------------------

ggsave(
  filename = "mapa_sm_central_lp.png",
  plot = mapa_sm_central_lp,
  path = "output/img/",
  width = 1800,
  height = 1800,
  units = "px",
  dpi = 300
)

