library(tidyverse)
library(openxlsx)

source("02 scr/03 script ggplot functies.R")
source("02 scr/01 script factor levels.R")

# pub_tabel_ams_sd <- read_rds("01 references/pub_tabel_ams_sd.rds")
# pub_tabel_sd_sd <- read_rds("01 references/pub_tabel_sd_sd.rds")
# pub_tabel_geb_geb <- read_rds("01 references/pub_tabel_geb_geb.rds")

pub_tabel_ams_sd_tot <- read_rds("01 references/pub_tabel_ams_sd_tot.rds")
pub_tabel_sd_sd_tot <- read_rds("01 references/pub_tabel_sd_sd_tot.rds")
pub_tabel_geb_geb_tot <- read_rds("01 references/pub_tabel_geb_geb_tot.rds")

pub_tabel_ams_sd_centrum <- read_rds(
  "01 references/pub_tabel_ams_sd_centrum.rds"
)
pub_tabel_sd_sd_centrum <- read_rds("01 references/pub_tabel_sd_sd_centrum.rds")
pub_tabel_geb_geb_centrum <- read_rds(
  "01 references/pub_tabel_geb_geb_centrum.rds"
)


# fig dagelijks niet dagelijks amsterdam
pub_tabel_ams_sd_tot$ams_hoofd |>
  mutate(monitor = factor(monitor, levels = lev_monitor)) |>
  my_plot(
    color_var = blauw_pal[c(9, 5, 3, 1)],
    x_var = aandeel_gew_omz,
    y_var = fct_rev(monitor),
    fill_var = eigen_gebied
  ) +
  facet_wrap(~productgroep)

ggsave("04 reports/04 figuren/fig_1_binding_totaal.svg", width = 12, height = 5)

pub_tabel_ams_sd_centrum$ams_hoofd |>
  mutate(monitor = factor(monitor, levels = lev_monitor)) |>
  my_plot(
    color_var = blauw_pal[c(9, 7, 5, 3, 1)],
    x_var = aandeel_gew_omz,
    y_var = fct_rev(monitor),
    fill_var = eigen_gebied_centrum
  ) +
  facet_wrap(~productgroep)

ggsave(
  "04 reports/04 figuren/fig_1_binding_totaal_centrum.svg",
  width = 12,
  height = 5
)


# fig dagelijks niet dagelijks amsterdam
pub_tabel_ams_sd_tot$ams_sub |>
  mutate(monitor = factor(monitor, levels = lev_monitor)) |>
  my_plot(
    color_var = blauw_pal[c(9, 5, 3, 1)],
    x_var = aandeel_gew_omz,
    y_var = fct_rev(monitor),
    fill_var = eigen_gebied
  ) +
  facet_wrap(~productgroep)

ggsave(
  "04 reports/04 figuren/fig_1_binding_subgroep.svg",
  width = 12,
  height = 5
)

# fig dagelijks niet dagelijks amsterdam
pub_tabel_ams_sd_centrum$ams_sub |>
  mutate(monitor = factor(monitor, levels = lev_monitor)) |>
  my_plot(
    color_var = blauw_pal[c(9, 7, 5, 3, 1)],
    x_var = aandeel_gew_omz,
    y_var = fct_rev(monitor),
    fill_var = eigen_gebied_centrum
  ) +
  facet_wrap(~productgroep)

ggsave(
  "04 reports/04 figuren/fig_1_binding_subgroep_centrum.svg",
  width = 12,
  height = 5
)


bind_rows(
  pub_tabel_ams_sd_tot$ams_hoofd |>
    filter(productgroep == 'niet-dagelijks'),

  pub_tabel_ams_sd_tot$ams_sub
) |>
  mutate(
    productgroep = case_when(
      productgroep == "niet-dagelijks" ~ "niet-dagelijks: totaal",
      productgroep == "doelgericht" ~ "niet-dagelijks: doelgericht",
      productgroep == "recreatief" ~ "niet-dagelijks: recreatief",
      TRUE ~ 'dagelijks'
    ),
    productgroep = factor(
      productgroep,
      levels = c(
        "dagelijks",
        "niet-dagelijks: recreatief",
        "niet-dagelijks: doelgericht",
        "niet-dagelijks: totaal"
      )
    )
  ) |>
  my_plot(
    color_var = blauw_pal[c(9, 5, 3, 1)],
    x_var = aandeel_gew_omz,
    y_var = fct_rev(monitor),
    fill_var = eigen_gebied
  ) +
  facet_wrap(~productgroep, nrow = 2)


ggsave(
  "04 reports/04 figuren/fig_1_binding_prod_en_sub_groep.svg",
  width = 10,
  height = 5
)

bind_rows(
  pub_tabel_ams_sd_centrum$ams_hoofd |>
    filter(productgroep == 'niet-dagelijks'),

  pub_tabel_ams_sd_centrum$ams_sub
) |>
  mutate(
    productgroep = case_when(
      productgroep == "niet-dagelijks" ~ "niet-dagelijks: totaal",
      productgroep == "doelgericht" ~ "niet-dagelijks: doelgericht",
      productgroep == "recreatief" ~ "niet-dagelijks: recreatief",
      TRUE ~ 'dagelijks'
    )
  ) |>
  my_plot(
    color_var = blauw_pal[c(9, 7, 5, 3, 1)],
    x_var = aandeel_gew_omz,
    y_var = fct_rev(monitor),
    fill_var = eigen_gebied_centrum
  ) +
  facet_wrap(~productgroep, nrow = 2)


ggsave(
  "04 reports/04 figuren/fig_1_binding_prod_en_sub_groep_centrum.svg",
  width = 10,
  height = 5
)


# fig dagelijks niet dagelijks stadsdelen
pub_tabel_ams_sd_tot$ams_prodgroep |>

  my_plot(
    color_var = blauw_pal[c(9, 5, 3, 1)],
    x_var = aandeel_gew_omz,
    y_var = fct_rev(monitor),
    fill_var = eigen_gebied
  ) +
  facet_wrap(~productgroep)

ggsave(
  "04 reports/04 figuren/fig_1_binding_prodgroep.svg",
  width = 12,
  height = 6
)

# alleen recreatief
pub_tabel_ams_sd_tot$ams_prodgroep |>
  filter(
    productgroep %in%
      c("modeartikelen", "huishoudelijk", "media", "sportspel")
  ) |>
  my_plot(
    color_var = blauw_pal[c(9, 5, 3, 1)],
    x_var = aandeel_gew_omz,
    y_var = fct_rev(monitor),
    fill_var = eigen_gebied
  ) +
  facet_wrap(~productgroep, nrow = 2)
ggsave(
  "04 reports/04 figuren/fig_1_binding_prodgroep_recr.svg",
  width = 10,
  height = 5
)

# allen doelgericht
pub_tabel_ams_sd_tot$ams_prodgroep |>
  filter(
    productgroep %in%
      c("elektronica", "woninginrichting", "planten", "doehetzelf")
  ) |>
  my_plot(
    color_var = blauw_pal[c(9, 5, 3, 1)],
    x_var = aandeel_gew_omz,
    y_var = fct_rev(monitor),
    fill_var = eigen_gebied
  ) +
  facet_wrap(~productgroep, nrow = 2)
ggsave(
  "04 reports/04 figuren/fig_1_binding_prodgroep_doelg.svg",
  width = 10,
  height = 5
)


# fig dagelijks niet dagelijks stadsdelen
pub_tabel_ams_sd_centrum$ams_prodgroep |>

  my_plot(
    color_var = blauw_pal[c(9, 7, 5, 3, 1)],
    x_var = aandeel_gew_omz,
    y_var = fct_rev(monitor),
    fill_var = eigen_gebied_centrum
  ) +
  facet_wrap(~productgroep)

ggsave(
  "04 reports/04 figuren/fig_1_binding_prodgroep_centrum.svg",
  width = 12,
  height = 6
)


# fig dagelijks niet dagelijks naar stadsdeel
pub_tabel_sd_sd_tot$ams_hoofd |>
  mutate(
    woon_gebied_naam = factor(woon_gebied_naam, levels = woongebied_naam_levels)
  ) |>
  filter(productgroep == 'dagelijks') |>
  filter(
    woon_gebied_naam != 'Westpoort',
    !is.na(woon_gebied_naam)
  ) |>
  filter(
    woon_gebied_naam != 'Weesp' | monitor != 'monitor 2020',
    woon_gebied_naam != 'Weesp' | monitor != 'monitor 2022'
  ) |>
  my_plot(
    color_var = blauw_pal[c(9, 7, 5, 3, 1)],
    x_var = aandeel_gew_omz,
    y_var = fct_rev(woon_gebied_naam),
    fill_var = eigen_gebied
  ) +
  facet_wrap(~monitor, nrow = 2)

ggsave(
  "04 reports/04 figuren/fig_2_binding_dagelijks_sd.svg",
  width = 10,
  height = 6
)


# fig dagelijks niet dagelijks naar stadsdeel
pub_tabel_sd_sd_centrum$ams_hoofd |>
  mutate(
    woon_gebied_naam = factor(woon_gebied_naam, levels = woongebied_naam_levels)
  ) |>
  filter(productgroep == 'dagelijks') |>
  filter(
    woon_gebied_naam != 'Westpoort',
    !is.na(woon_gebied_naam)
  ) |>
  filter(
    woon_gebied_naam != 'Weesp' | monitor != 'monitor 2020',
    woon_gebied_naam != 'Weesp' | monitor != 'monitor 2022'
  ) |>
  my_plot(
    y_var = fct_rev(woon_gebied_naam),
    fill_var = eigen_gebied_centrum,
    color_var = blauw_pal[c(9, 7, 6, 5, 3, 1)],
    x_var = aandeel_gew_omz
  ) +
  facet_wrap(~monitor, nrow = 1)

ggsave(
  "04 reports/04 figuren/fig_2_binding_dagelijks_sd_centrum.svg",
  width = 12,
  height = 4
)


# fig dagelijks niet dagelijks naar stadsdeel
pub_tabel_sd_sd_tot$ams_hoofd |>
  mutate(
    woon_gebied_naam = factor(woon_gebied_naam, levels = woongebied_naam_levels)
  ) |>
  filter(productgroep == 'niet-dagelijks') |>
  filter(
    woon_gebied_naam != 'Westpoort',
    !is.na(woon_gebied_naam)
  ) |>
  filter(
    woon_gebied_naam != 'Weesp' | monitor != 'monitor 2020',
    woon_gebied_naam != 'Weesp' | monitor != 'monitor 2022'
  ) |>
  my_plot(
    y_var = fct_rev(woon_gebied_naam),
    fill_var = eigen_gebied,
    color_var = blauw_pal[c(9, 7, 5, 3, 1)],
    x_var = aandeel_gew_omz
  ) +
  facet_wrap(~monitor, nrow = 1)

ggsave(
  "04 reports/04 figuren/fig_2_binding_niet-dagelijks_sd.svg",
  width = 12,
  height = 4
)


# fig dagelijks niet dagelijks naar stadsdeel
pub_tabel_sd_sd_centrum$ams_hoofd |>
  mutate(
    woon_gebied_naam = factor(woon_gebied_naam, levels = woongebied_naam_levels)
  ) |>
  filter(productgroep == 'niet-dagelijks') |>
  filter(
    woon_gebied_naam != 'Westpoort',
    !is.na(woon_gebied_naam)
  ) |>
  filter(
    woon_gebied_naam != 'Weesp' | monitor != 'monitor 2020',
    woon_gebied_naam != 'Weesp' | monitor != 'monitor 2022'
  ) |>
  my_plot(
    y_var = fct_rev(woon_gebied_naam),
    fill_var = eigen_gebied_centrum,
    color_var = blauw_pal[c(9, 7, 6, 5, 3, 1)],
    x_var = aandeel_gew_omz
  ) +
  facet_wrap(~monitor, nrow = 1)

ggsave(
  "04 reports/04 figuren/fig_2_binding_niet-dagelijks_sd_centrum.svg",
  width = 12,
  height = 4
)


# fig dagelijks niet dagelijks naar stadsdeel
pub_tabel_sd_sd_centrum$ams_sub |>
  mutate(
    woon_gebied_naam = factor(woon_gebied_naam, levels = woongebied_naam_levels)
  ) |>
  filter(productgroep == 'recreatief') |>
  filter(
    woon_gebied_naam != 'Westpoort',
    !is.na(woon_gebied_naam)
  ) |>
  filter(
    woon_gebied_naam != 'Weesp' | monitor != 'monitor 2020',
    woon_gebied_naam != 'Weesp' | monitor != 'monitor 2022'
  ) |>
  my_plot(
    y_var = fct_rev(woon_gebied_naam),
    fill_var = eigen_gebied_centrum,
    color_var = blauw_pal[c(9, 7, 6, 5, 3, 1)],
    x_var = aandeel_gew_omz
  ) +
  facet_wrap(~monitor, nrow = 2)

ggsave(
  "04 reports/04 figuren/fig_2_binding_recreatief_sd_centrum.svg",
  width = 10,
  height = 5
)


# fig dagelijks niet dagelijks naar stadsdeel
pub_tabel_sd_sd_centrum$ams_sub |>
  mutate(
    woon_gebied_naam = factor(woon_gebied_naam, levels = woongebied_naam_levels)
  ) |>
  filter(productgroep == 'doelgericht') |>
  filter(
    woon_gebied_naam != 'Westpoort',
    !is.na(woon_gebied_naam)
  ) |>
  filter(
    woon_gebied_naam != 'Weesp' | monitor != 'monitor 2020',
    woon_gebied_naam != 'Weesp' | monitor != 'monitor 2022'
  ) |>
  my_plot(
    y_var = fct_rev(woon_gebied_naam),
    fill_var = eigen_gebied_centrum,
    color_var = blauw_pal[c(9, 7, 6, 5, 3, 1)],
    x_var = aandeel_gew_omz
  ) +
  facet_wrap(~monitor, nrow = 2)

ggsave(
  "04 reports/04 figuren/fig_2_binding_doelgericht_sd_centrum.svg",
  width = 10,
  height = 5
)


######

# fig dagelijks niet dagelijks naar stadsdeel
pub_tabel_sd_sd_tot$ams_sub |>
  mutate(
    woon_gebied_naam = factor(woon_gebied_naam, levels = woongebied_naam_levels)
  ) |>
  filter(
    monitor == 'monitor 2026',
    productgroep %in% c('recreatief', 'doelgericht', 'niet-dagelijks'),
    woon_gebied_naam != 'Westpoort',
    !is.na(woon_gebied_naam)
  ) |>
  my_plot(
    y_var = fct_rev(woon_gebied_naam),
    fill_var = eigen_gebied,
    color_var = blauw_pal[c(9, 7, 5, 3, 1)],
    x_var = aandeel_gew_omz
  ) +
  facet_wrap(~productgroep, nrow = 1)

ggsave(
  "04 reports/04 figuren/fig_2_binding_recr_doelg_sd.svg",
  width = 12,
  height = 4
)


# fig dagelijks niet dagelijks naar stadsdeel
pub_tabel_sd_sd_centrum$ams_sub |>
  mutate(
    woon_gebied_naam = factor(woon_gebied_naam, levels = woongebied_naam_levels)
  ) |>
  filter(
    monitor == 'monitor 2026',
    productgroep %in% c('recreatief', 'doelgericht', 'niet-dagelijks'),
    woon_gebied_naam != 'Westpoort',
    !is.na(woon_gebied_naam)
  ) |>
  my_plot(
    y_var = fct_rev(woon_gebied_naam),
    fill_var = eigen_gebied_centrum,
    color_var = blauw_pal[c(9, 7, 6, 5, 3, 1)],
    x_var = aandeel_gew_omz
  ) +
  facet_wrap(~productgroep, nrow = 1)

ggsave(
  "04 reports/04 figuren/fig_2_binding_recr_doelg_sd_centrum.svg",
  width = 12,
  height = 4
)


### kaarten met binding per ggw-gebied ---

geo_gebieden <- sf::read_sf(
  "https://onderzoek.amsterdam.nl/static/datavisualisatie-onderzoek-en-statistiek/geo/amsterdam/2022/gebieden-2022-zw-geo.json"
)

df_gebied <- bind_rows(
  pub_tabel_geb_geb_tot$ams_hoofd |>
    filter(
      eigen_gebied == 'zelfde GGW-gebied',
      monitor == 'monitor 2026',
      productgroep == 'niet-dagelijks'
    ),

  pub_tabel_geb_geb_tot$ams_sub |>
    filter(
      eigen_gebied == 'zelfde GGW-gebied',
      monitor == 'monitor 2026',
      !is.na(productgroep)
    )
) |>
  group_by(productgroep) |>
  mutate(
    value_kl = gtools::quantcut(aandeel_gew_omz, 4),
    value_kl_labels = gtools::quantcut(
      aandeel_gew_omz,
      4,
      labels = c(
        "veel lager dan gemiddelde",
        "lager dan gemiddelde",
        "hoger dan gemiddelde",
        "veel hoger dan gemiddelde"
      )
    )
  ) |>
  mutate(
    productgroep = case_when(
      productgroep == 'dagelijks' ~ 'dagelijks',
      productgroep == 'niet-dagelijks' ~ 'niet-dagelijks: totaal',
      productgroep == 'recreatief' ~ 'niet-dagelijks: recreatief',
      productgroep == 'doelgericht' ~ 'niet-dagelijks: doelgericht'
    ),
    productgroep = factor(
      productgroep,
      levels = c(
        "dagelijks",
        "niet-dagelijks: recreatief",
        "niet-dagelijks: doelgericht",
        "niet-dagelijks: totaal"
      )
    )
  )


kaart <- df_gebied |>
  left_join(geo_gebieden, by = c("woon_gebied_code" = "code")) |>
  sf::st_as_sf()

ggplot() +
  geom_sf(data = geo_gebieden, color = "white", size = 0.5) +
  geom_sf(
    data = kaart,
    aes(
      fill = value_kl_labels
    ),
    color = "white",
    size = 0.5
  ) +
  facet_wrap(~productgroep, ncol = 2) +
  theme_os3("bottom") +
  geom_sf_text(
    data = kaart,
    font = font,
    aes(
      label = label_percent(accuracy = 1)(aandeel_gew_omz),
      color = value_kl_labels
    )
  ) +
  scale_fill_manual(name = NULL, values = blauw_pal[c(6, 4, 2, 1)]) +
  scale_color_manual(name = NULL, values = label_col[c(6, 4, 2, 1)]) +
  guides(
    fill = guide_legend(ncol = 2, reverse = F),
    colour = "none"
  )


ggsave(
  "04 reports/04 figuren/fig_3_binding_geb.svg",
  width = 10,
  height = 8
)
