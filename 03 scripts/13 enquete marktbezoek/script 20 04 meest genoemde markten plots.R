# meest genoemde markt per jaar

library(tidyverse)
library(svglite)
library(patchwork)

### figuren ---

# open tabellen ---

# script met themes en factor -
source("02 scr/03 script ggplot functies.R")
source("02 scr/01 script factor levels.R")

list_tabellen_markt <- read_rds("01 references/tabellen_markt_basis.rds")


# meest genoemde martken in Amsterdam
list_tabellen_markt[["marktnaam"]] |>
  filter(
    monitor %in% c('monitor 2026'),
    v15_schoon != 'Zuidasmarkt',
    v15_schoon != 'onbekend',
    v15_schoon != 'anders',
    gebied_naam == 'Amsterdam'
  ) |>
  group_by(monitor) |>
  slice_max(aandeel, n = 10, with_ties = T) |>
  ggplot(aes(
    y = fct_relevel(fct_reorder(v15_schoon, aandeel), "bezoekt geen markt"),
    x = aandeel
  )) +
  geom_col(
    position = "dodge",
    fill = "#004699"
  ) +
  geom_text(
    aes(
      label = as.character(round(aandeel_gew))
    ),
    color = "#ffffffff",
    hjust = 1.5,
    family = font,
    lineheight = .8
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_os2() +
  scale_fill_manual(name = NULL, values = blauw_pal[c(9, 8, 7, 6, 4, 3, 1)]) +
  scale_color_manual(name = NULL, values = label_col[c(9, 8, 7, 6, 4, 3, 1)]) +
  guides(fill = guide_legend(reverse = T, ncol = 2), color = 'none') +
  theme(legend.position = "bottom") +
  facet_wrap(~monitor, nrow = 1)
ggsave(
  "04 reports/04 figuren/fig_meest_genoemde_markt_ams.svg",
  width = 6,
  height = 6
)


### per stadsdeel

my_plot_sd <- function(sd_var) {
  list_tabellen_markt[["marktnaam"]] |>
    filter(
      monitor == 'monitor 2026',
      gebied_naam == sd_var,
      !is.na(gebied_naam),
      v15_schoon != 'Zuidasmarkt',
      v15_schoon != 'onbekend',
      v15_schoon != 'anders'
    ) |>
    slice_max(aandeel, n = 6, with_ties = F) |>
    ggplot(aes(
      y = fct_relevel(fct_reorder(v15_schoon, aandeel), "bezoekt geen markt"),
      x = aandeel
    )) +
    theme_os2() +
    geom_col(
      width = 0.9,
      fill = "#004699"
    ) +
    geom_text(
      aes(
        label = round(aandeel_gew)
      ),
      color = "white",
      hjust = 1.5,
      family = font
    ) +
    labs(title = sd_var, x = NULL, y = NULL) +
    #facet_wrap(~gebied_naam, ncol = 1) +
    guides(fill = 'none', color = 'none')
}


my_plot_sd("Centrum") +
  my_plot_sd("West") +
  my_plot_sd("Nieuw-West") +
  my_plot_sd("Zuid") +
  my_plot_sd("Oost") +
  my_plot_sd("Noord") +
  my_plot_sd("Weesp") +
  my_plot_sd("Zuidoost") +
  my_plot_sd("Amsterdam")


ggsave(
  "04 reports/04 figuren/fig_meest_genoemde_markt_sd.svg",
  width = 14,
  height = 8
)
#### meest genoemde markt per wijk

markt_kleur <- c(
  "#004699",
  "#f39f34",
  "#a4ccf3",
  "#dceafa",
  "#53b361",
  "#bed200",
  "#ffe600",
  "#ff9100",
  "#ec0000",
  "#fdb0cb",
  "#d48fb9",
  "#71abdd",
  "#e50082",
  "#fb9bbe",
  "#efd2e3",
  "#7c84b2",
  "#a2bad3",
  "#a00078",
  "#009de6",
  "#fff3a7"
)

library(sf)
### plot kaart met meest genoemde markt per gebied -

# wijkenkaart van amsterdam -
kaart_wijk <- read_sf(
  "https://onderzoek.amsterdam.nl/static/datavisualisatie-onderzoek-en-statistiek/geo/amsterdam/2022/wijken-2022-zw-topo.json"
)
st_crs(kaart_wijk) = 4326

# tabel meest genoemde markt per wijk  -
markt_wijk <- list_tabellen_markt$marktnaam |>
  filter(v15_schoon != 'bezoekt geen markt') |>
  filter(v15_schoon != 'bezoekt markt, markt onbekend') |>

  filter(gebied_type == 'wijk') |>
  group_by(monitor, gebied_naam, gebied_code) |>
  filter(aandeel_gew == max(aandeel_gew))

# samenvoegen tabel aan wijkenkaart -
kaart_markt <- kaart_wijk |>

  left_join(markt_wijk, by = c("code" = "gebied_code"))

#plot de kaart
kaart_markt |>
  filter(monitor == 'monitor 2026') |>
  filter(aantal > 5) |>
  ggplot(aes(fill = v15_schoon)) +
  geom_sf(data = kaart_wijk, aes(fill = NULL), color = "white", size = 0.9) +
  geom_sf(color = "white", size = 0.9) +
  scale_fill_manual(name = NULL, values = markt_kleur) +
  theme_map(legend_position = "bottom") +
  guides(fill = guide_legend(title = NULL, ncol = 3))
ggsave("04 reports/04 figuren/fig1_marktbez_wijk_d.svg", width = 12, height = 8)


### aandeel dat niet naar de markt gaat per wijk

niet_markt_wijk <- tabel_list_marktnaam$tab_v1_wijk |>
  group_by(v15_schoon, gbd_wijk_code, gbd_wijk_naam) |>
  summarise(aantal = sum(aantal)) |>
  group_by(gbd_wijk_code, gbd_wijk_naam) |>
  mutate(aandeel = aantal / sum(aantal) * 100) |>
  filter(v15_schoon == 'bezoekt geen markt') |>
  mutate(
    aandeel_kl = case_when(
      aandeel < 20 ~ '0 tot 20%',
      aandeel >= 20 & aandeel < 40 ~ '20 tot 40%',
      aandeel >= 40 & aandeel < 50 ~ '40 tot 50%',
      aandeel >= 50 & aandeel < 60 ~ '50 tot 60%',
      aandeel >= 60 ~ '60% of hoger'
    )
  )


#### kaarten ----

kaart_niet_markt <- kaart_wijk |>
  left_join(niet_markt_wijk, by = c("code" = "gbd_wijk_code"))

kaart_niet_markt |>
  filter(aantal > 8) |>
  ggplot(aes(fill = aandeel_kl)) +
  geom_sf(data = kaart_wijk, aes(fill = NULL), color = "white", size = 2.1) +
  geom_sf(color = "white", size = 2.1) +
  theme_os_map(legend_position = "bottom") +
  scale_fill_manual(name = NULL, values = rev(baby_blauw)) +
  guides(fill = guide_legend())
ggsave("04 output tabellen/fig_geenmarkt_perwijk.png", width = 7, height = 5)
