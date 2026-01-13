library(tidyverse)
library(svglite)

### figuren ---

# open tabellen ---

# script met themes en factor -
source("02 scr/03 script ggplot functies.R")
source("02 scr/01 script factor levels.R")

list_tabellen_markt <- read_rds("01 references/tabellen_markt_basis.rds")

# frequentie ams en stadseel --
list_tabellen_markt[["freq_bezoek"]] |>
  filter(
    gebied_type %in% c("Gemeente", "stadsdeel"),
    gebied_naam != 'Westpoort',
    !is.na(gebied_naam)
  ) |>
  filter(
    (gebied_naam != 'Weesp' | monitor %in% c('monitor 2024', 'monitor 2026'))
  ) |>
  mutate(
    v13 = factor(v13, levels = lev_v13),
    gebied_naam = factor(gebied_naam, levels = lev_sd)
  ) |>

  ggplot(aes(
    x = aandeel_gew,
    y = fct_rev(monitor),
    fill = fct_rev(v13)
  )) +
  geom_col() +
  geom_text(
    aes(
      label = if_else(aandeel_gew > 5, as.character(round(aandeel_gew)), ""),
      color = fct_rev(v13)
    ),
    position = position_stack(vjust = 0.5),
    family = font,
    lineheight = .8
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_os2() +
  scale_fill_manual(name = NULL, values = blauw_pal[c(9, 8, 7, 6, 4, 3, 1)]) +
  scale_color_manual(name = NULL, values = label_col[c(9, 8, 7, 6, 4, 3, 1)]) +
  guides(fill = guide_legend(reverse = T, nrow = 3), color = 'none') +
  theme(legend.position = "bottom") +
  facet_wrap(~gebied_naam)
ggsave(
  "04 reports/04 figuren/fig1_freqmarktbez_ams.svg",
  width = 12,
  height = 6
)


### zelfde maar dan alleen 24 2026 voor monitor warenmarkten
list_tabellen_markt[["freq_bezoek"]] |>
  filter(
    gebied_type %in% c("Gemeente", "stadsdeel"),
    gebied_naam != 'Westpoort',
    !is.na(gebied_naam)
  ) |>
  filter(
    (monitor %in% c('monitor 2024', 'monitor 2026'))
  ) |>
  mutate(
    v13 = factor(v13, levels = lev_v13),
    gebied_naam = factor(gebied_naam, levels = lev_sd)
  ) |>
  ggplot(aes(
    x = aandeel_gew,
    y = fct_rev(gebied_naam),
    fill = fct_rev(v13)
  )) +
  geom_col() +
  geom_text(
    aes(
      label = if_else(aandeel_gew > 5, as.character(round(aandeel_gew)), ""),
      color = fct_rev(v13)
    ),
    position = position_stack(vjust = 0.5),
    family = font,
    lineheight = .8
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_os2() +
  scale_fill_manual(name = NULL, values = blauw_pal[c(9, 8, 7, 6, 4, 3, 1)]) +
  scale_color_manual(name = NULL, values = label_col[c(9, 8, 7, 6, 4, 3, 1)]) +
  guides(fill = guide_legend(reverse = T, ncol = 2), color = 'none') +
  theme(legend.position = "bottom") +
  facet_wrap(~monitor)
ggsave(
  "04 reports/04 figuren/fig1_freqmarktbez_ams_2224.svg",
  width = 12,
  height = 6
)

## dit is een plot voor v13 frequentie
plot_functie <- function(x, yvar, facetvar) {
  x |>
    filter(
      gebied_naam == 'Amsterdam',
      achtergrond_cat != 'onbekend'
    ) |>
    mutate(v13 = factor(v13, levels = lev_v13)) |>
    ggplot(aes(
      x = aandeel_gew,
      y = fct_rev({{ yvar }}),
      fill = fct_rev(v13)
    )) +
    geom_col() +
    geom_text(
      aes(
        label = if_else(aandeel_gew > 5, as.character(round(aandeel_gew)), ""),
        color = fct_rev(v13)
      ),
      position = position_stack(vjust = 0.5),
      family = font,
      lineheight = .8
    ) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_os2() +
    scale_fill_manual(name = NULL, values = blauw_pal[c(9, 8, 7, 6, 4, 3, 1)]) +
    scale_color_manual(
      name = NULL,
      values = label_col[c(9, 8, 7, 6, 4, 3, 1)]
    ) +
    guides(fill = guide_legend(reverse = T, nrow = 2), color = 'none') +
    theme(legend.position = "bottom") +
    facet_wrap(vars({{ facetvar }}), scales = "free_y")
}


# naar leeftijd alle jaren
list_tabellen_markt[["freq_achtergr"]] |>
  filter(achtergrond_naam == 'leeftijd_klas') |>
  plot_functie(yvar = monitor, facetvar = achtergrond_cat)
ggsave(
  "04 reports/04 figuren/fig1_freqmarktbez_ams_leeft.svg",
  width = 12,
  height = 5
)

# naar leeftijd en opleiding 2026 voor marktmonitor
list_tabellen_markt[["freq_achtergr"]] |>
  filter(
    achtergrond_cat != 'opleiding onbekend',
    monitor %in% c('monitor 2026'),
    achtergrond_naam %in% c('leeftijd_klas', 'opleiding_klas')
  ) |>
  mutate(
    achtergrond_naam = case_when(
      achtergrond_naam == 'leeftijd_klas' ~ 'leeftijd',
      achtergrond_naam == 'opleiding_klas' ~ 'opleiding',
      TRUE ~ achtergrond_naam
    )
  ) |>
  mutate(
    achtergrond_cat = case_when(
      achtergrond_cat == 'basisopgeleid (maximaal mbo-1)' ~ 'basisopgeleid',
      achtergrond_cat ==
        'mbo-opgeleid (mbo-2 tm 4 opleiding of havo of vwo afgerond)' ~ 'mbo-opgeleid',
      achtergrond_cat ==
        'hbo/wo-opgeleid (hbo- of wo-opleiding afgerond)' ~ 'hbo/wo-opgeleid',
      TRUE ~ achtergrond_cat
    )
  ) |>
  mutate(
    achtergrond_cat = factor(
      achtergrond_cat,
      levels = (c(lev_opl, lev_leefkl))
    )
  ) |>
  plot_functie(yvar = achtergrond_cat, facetvar = fct_rev(achtergrond_naam))
ggsave(
  "04 reports/04 figuren/fig1_freqmarktbez_ams_leeft_opl.svg",
  width = 12,
  height = 4
)

# naar inkomen en huishouden 2026 voor marktmonitor
list_tabellen_markt[["freq_achtergr"]] |>
  filter(
    achtergrond_cat != 'inkomen onbekend',
    achtergrond_cat != 'overig, onbekend',
    monitor %in% c('monitor 2026'),
    achtergrond_naam %in% c('inkomen_klas', 'huishouden_klas')
  ) |>
  mutate(
    achtergrond_naam = case_when(
      achtergrond_naam == 'huishouden_klas' ~ 'huishouden',
      achtergrond_naam == 'inkomen_klas' ~ 'inkomen',
      TRUE ~ achtergrond_naam
    )
  ) |>
  mutate(
    achtergrond_cat = factor(achtergrond_cat, levels = c(lev_huish, lev_ink))
  ) |>
  plot_functie(yvar = achtergrond_cat, facetvar = achtergrond_naam)
ggsave(
  "04 reports/04 figuren/fig1_freqmarktbez_ams_ink_hh.svg",
  width = 12,
  height = 4
)


## dit is een plot voor spreiding achtergrondkenmerken
plot_functie_achtergrond <- function(x, yvar, fillvar, facetvar) {
  x |>
    ggplot(aes(
      x = aandeel_gew,
      y = fct_rev({{ yvar }}),
      fill = fct_rev({{ fillvar }})
    )) +
    geom_col() +
    geom_text(
      aes(
        label = if_else(aandeel_gew > 5, as.character(round(aandeel_gew)), ""),
        color = fct_rev({{ fillvar }})
      ),
      position = position_stack(vjust = 0.5),
      family = font,
      lineheight = .8
    ) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_os2() +
    scale_fill_manual(name = NULL, values = blauw_pal[c(9, 8, 6, 4, 1)]) +
    scale_color_manual(
      name = NULL,
      values = label_col[c(9, 8, 6, 4, 1)]
    ) +
    guides(fill = guide_legend(reverse = T, nrow = 2), color = 'none') +
    theme(legend.position = "bottom") +
    facet_wrap(vars(!!enquo(facetvar)), nrow = 1)
}


list_tabellen_markt[["achtergrond"]] |>
  filter(
    monitor %in% c('monitor 2024', 'monitor 2026'),
    achtergrond_naam == 'leeftijd_klas'
  ) |>
  #mutate(achtergrond_cat = factor(achtergrond_cat, levels = )) |>
  plot_functie_achtergrond(
    yvar = fct_relevel(fct_rev(marktbezoek), "totaal", after = Inf),
    fillvar = achtergrond_cat,
    facetvar = monitor
  )
ggsave(
  "04 reports/04 figuren/fig1_bezoek_ams_leefklas2224.svg",
  width = 12,
  height = 4
)


list_tabellen_markt[["achtergrond"]] |>
  filter(
    monitor %in% c('monitor 2024', 'monitor 2026'),
    achtergrond_naam == 'huishouden_klas'
  ) |>
  #mutate(achtergrond_cat = factor(achtergrond_cat, levels = )) |>
  plot_functie_achtergrond(
    yvar = fct_relevel(fct_rev(marktbezoek), "totaal", after = Inf),
    fillvar = fct_relevel(achtergrond_cat, "overig, onbekend", after = Inf),
    facetvar = monitor
  )
ggsave(
  "04 reports/04 figuren/fig1_bezoek_ams_huishoud2224.svg",
  width = 12,
  height = 4
)
