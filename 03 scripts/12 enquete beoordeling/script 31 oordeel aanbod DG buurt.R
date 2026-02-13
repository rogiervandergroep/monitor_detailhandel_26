#### oud moet herschreven worden ---

library(tidyverse)
library(haven)

data_totaal <- read_rds("01 references/data_totaal_20_22_24_26.rds")

source("02 scr/01 script factor levels.R")
source("02 scr/03 script ggplot functies.R")


data_v8 <- data_totaal |>
  map(\(x) {
    select(
      x,
      monitor,
      ends_with("_klas"),
      starts_with("gebied"),
      starts_with("v8"),
      weeg_ams_ink
    )
  }) |>
  map(\(x) mutate(x, v8_naam = haven::as_factor(v8))) |>
  map(\(x) janitor::clean_names(x)) |>
  map(\(x) {
    mutate(
      x,
      v8_naam = case_when(
        is.na(v8_naam) ~ 'weet ik niet, geen antwoord',
        v8_naam == 'weet niet, geen antwoord' ~ 'weet ik niet, geen antwoord',
        TRUE ~ v8_naam
      )
    )
  })


my_summary <- function(x, groupvars = NULL) {
  x |>
    group_by(monitor, v8_naam, {{ groupvars }}) |>

    summarise(
      aantal = n(),
      aantal_gew = sum(weeg_ams_ink, na.rm = T)
    ) |>

    group_by(monitor, {{ groupvars }}) |>

    mutate(
      aandeel = round(aantal / sum(aantal, na.rm = T) * 100, 2),
      aandeel_gew = round(aantal_gew / sum(aantal_gew, na.rm = T) * 100, 2)
    )
}


v8_levels <- c("ja", "enigszins", "nee", "weet ik niet, geen antwoord")


tabel_v8 <- bind_rows(
  # totaal
  data_v8 |>
    map_df(\(x) my_summary(x)) |>
    add_column(
      gebied_stadsdeel_code = 'Amsterdam',
      gebied_stadsdeel_naam = 'Amsterdam'
    ),
  # naar stadsdeel
  data_v8 |>
    map_df(\(x) {
      my_summary(
        x,
        groupvars = across(all_of(c(
          "gebied_stadsdeel_code",
          "gebied_stadsdeel_naam"
        )))
      )
    }) |>
    filter(!is.na(gebied_stadsdeel_naam)),

  # naar gebied
  data_v8 |>
    map_df(\(x) {
      my_summary(
        x,
        groupvars = across(all_of(c(
          "gebied_ggw_code",
          "gebied_ggw_naam"
        )))
      )
    }) |>
    filter(!is.na(gebied_ggw_naam))
) |>
  mutate(
    v8_naam = factor(v8_naam, levels = v8_levels),
    gebied_stadsdeel_naam = factor(gebied_stadsdeel_naam, levels = lev_sd)
  ) |>
  rename(categorie = v8_naam) |>
  add_column(
    vraag = 'v8 oordeel aanbod winkels dagelijkse boodschappen in de buurt'
  )


tabel_v8 |>
  # verwijderen data Weesp uit vorige monitors
  mutate(
    aandeel_gew = case_when(
      gebied_stadsdeel_naam == 'Weesp' &
        monitor %in% c("monitor 2020", "monitor 2022") ~ NA,
      TRUE ~ aandeel_gew
    )
  ) |>
  filter(gebied_stadsdeel_naam != 'Westpoort') |>
  my_plot(
    x_var = aandeel_gew / 100,
    y_var = fct_rev(gebied_stadsdeel_naam),
    fill_var = categorie,
    color_var = blauw_pal[c(9, 6, 4, 1)]
  ) +
  facet_wrap(~monitor, nrow = 2)
ggsave(
  "04 reports/04 figuren/fig_v8_aanbod_dg_buurt.svg",
  width = 10,
  height = 6
)


# url met ggw-gebieden
geb_url <- 'https://onderzoek.amsterdam.nl/static/datavisualisatie-onderzoek-en-statistiek/geo/amsterdam/2022/gebieden-2022-zw-geo.json'

# inlezen geojson van de stadsdelen en omzetten naar spatial feature
ggw_gebieden_geo <- sf::st_read(geb_url) |>
  rename(
    gebied_ggw_naam = naam,
    gebied_ggw_code = code
  )

tabel_v8_gebied <- tabel_v8 |>
  filter(
    monitor == 'monitor 2024',
    !is.na(gebied_ggw_naam)
  )

kaart_gebieden_v8 <- ggw_gebieden_geo |>
  left_join(
    tabel_v8_gebied,
    by = c("gebied_ggw_naam", "gebied_ggw_code")
  ) |>
  filter(categorie == 'ja')


kaart_gebieden_v8 |>
  ggplot() +
  geom_sf(
    data = ggw_gebieden_geo,
    color = 'white',
    fill = "#cfcfcf",
    linewidth = 0.6
  ) +
  geom_sf(
    data = kaart_gebieden_v8,
    aes(fill = aandeel_gew, geometry = geometry),
    color = NA,
    linewidth = 0.7
  ) +
  geom_sf_text(
    data = kaart_gebieden_v8,
    aes(label = round(aandeel_gew), geometry = geometry),
    font = font
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradient(name = NULL, low = "#f2dae7", high = "#a00078") +
  theme_clk(legenda_pos = "right") +
  guides(
    fill = guide_colourbar(reverse = FALSE)
  )
ggsave(
  "04 reports/04 figuren/fig_v8_kaart_dg_buurt.svg",
  width = 8,
  height = 6
)


# redenen van ontevredenheid

# maak de labels
redenen_labels <- data_v8[["data_26_weeg"]] |>
  select(v8b_nw01:v8b_nw11) |>
  names() |>
  map_df(\(i) {
    tibble(v8_labels = attr(data_v8[["data_26_weeg"]][[i]], "label"), name = i)
  }) |>
  mutate(
    v8_labels = case_when(
      v8_labels ==
        'te weinig aanbod van streekproducten/lokaal geproduceerde producten' ~ 'weinig aanbod streek-, lokaal prod.',
      v8_labels ==
        'aanbod producten in winkels in deze buurt heeft lage kwaliteit' ~ 'aanbod heeft lage kwaliteit',
      v8_labels ==
        'te weinig aanbod van seizoensgebonden producten' ~ 'weinig aanbod seizoensgebonden prod.',
      v8_labels ==
        'winkels in de buurt zijn ongezellig, hebben weinig sfeer' ~ 'winkels in buurt zijn ongezellig',
      v8_labels ==
        'te weinig winkels met biologische producten' ~ 'weinig winkels met biol. prod.',
      v8_labels ==
        'ik mis bepaalde producten, namelijk' ~ 'ik mis bepaalde producten',
      TRUE ~ v8_labels
    )
  )


data_redenen <- data_v8[["data_26_weeg"]] |>
  pivot_longer(cols = c(v8b_nw01:v8b_nw11)) |>
  left_join(redenen_labels, by = "name")

tabel_reden <- bind_rows(
  # amsterdam
  data_redenen |>
    filter(!is.na(value)) |>
    group_by(name, v8_labels, value) |>
    summarise(aantal_genoemd = n()) |>
    group_by(name, v8_labels) |>
    mutate(aandeel = aantal_genoemd / sum(aantal_genoemd) * 100) |>
    filter(value == 1) |>
    add_column(gebied_stadsdeel_naam = 'Amsterdam'),

  # stasdeel
  data_redenen |>
    filter(!is.na(value)) |>
    group_by(gebied_stadsdeel_naam, name, v8_labels, value) |>
    summarise(aantal_genoemd = n()) |>
    group_by(gebied_stadsdeel_naam, name, v8_labels) |>
    mutate(aandeel = aantal_genoemd / sum(aantal_genoemd) * 100) |>
    filter(value == 1),

  # gew-gebied
  data_redenen |>
    filter(!is.na(value)) |>
    group_by(gebied_ggw_naam, name, v8_labels, value) |>
    summarise(aantal_genoemd = n()) |>
    group_by(gebied_ggw_naam, name, v8_labels) |>
    mutate(aandeel = aantal_genoemd / sum(aantal_genoemd) * 100) |>
    filter(value == 1)
) |>
  mutate(gebied_stadsdeel_naam = factor(gebied_stadsdeel_naam, levels = lev_sd))


tabel_reden |>
  filter(
    gebied_stadsdeel_naam != 'Westpoort',
    v8_labels != 'weet niet',
    v8_labels != 'anders, namelijk',
    !is.na(gebied_stadsdeel_naam)
  ) |>

  ggplot(aes(
    y = fct_reorder(v8_labels, aandeel),
    x = aandeel
  )) +
  geom_col(fill = blauw_pal[1]) +
  geom_text(
    aes(label = if_else(aandeel > 10, as.character(round(aandeel)), "")),
    color = 'white',
    position = position_stack(vjust = 0.5),
    family = font,
    lineheight = .8
  ) +

  labs(title = NULL, x = NULL, y = NULL) +
  theme_os2() +
  guides(fill = 'none') +
  facet_wrap(~gebied_stadsdeel_naam)

ggsave("04 reports/04 figuren/fig_v8_redenen.svg", width = 12, height = 8)
