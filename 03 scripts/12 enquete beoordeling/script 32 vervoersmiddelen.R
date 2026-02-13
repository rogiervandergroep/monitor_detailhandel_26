# V5 hoe gaat u normaliter boodschappen doen?
library(tidyverse)
library(haven)

data_totaal <- read_rds("01 references/data_totaal_20_22_24_26.rds")

source("02 scr/01 script factor levels.R")
source("02 scr/03 script ggplot functies.R")


data_v5 <- data_totaal |>
  map(\(x) {
    select(
      x,
      monitor,
      ends_with("_klas"),
      starts_with("gebied"),
      starts_with("v5"),
      weeg_ams_ink
    )
  }) |>
  map(\(x) mutate(x, v5 = haven::as_factor(v5))) |>
  map(\(x) janitor::clean_names(x)) |>
  bind_rows() |>
  filter(!is.na(v5)) |>
  mutate(v5 = str_trim(v5, "both")) |>
  mutate(
    v5 = case_when(
      v5 %in% c('fiets', 'gewone fiets', 'elektrische fiets') ~ 'fiets',
      v5 %in% c('bromfiets', 'brommer, bromfiets, scooter') ~ 'brommer',
      v5 %in%
        c(
          'scootmobiel, rolstoel, canta, Birò',
          'scootmobiel, rolstoel, canta'
        ) ~ 'scootmobiel, canta, Birò',
      v5 == 'anders, namelijk' ~ 'anders',
      TRUE ~ v5
    )
  )


my_v5_function <- function(x, var, var_levels) {
  x |>
    group_by(monitor, onderwerp = {{ var }}, v5) |>
    summarise(
      aantal = n(),
      aantal_gew = sum(weeg_ams_ink, na.rm = T)
    ) |>
    group_by(monitor, onderwerp) |>
    mutate(
      aandeel = aantal / sum(aantal, na.rm = T),
      aandeel_gew = aantal_gew / sum(aantal_gew, na.rm = T)
    )
}

tabel_v5_achtergr <- bind_rows(
  data_v5 |>
    group_by(v5, monitor) |>
    summarise(
      aantal = n(),
      aantal_gew = sum(weeg_ams_ink, na.rm = T)
    ) |>
    group_by(monitor) |>
    mutate(
      aandeel = aantal / sum(aantal, na.rm = T),
      aandeel_gew = aantal_gew / sum(aantal_gew, na.rm = T)
    ) |>
    add_column(
      thema = "Amsterdam",
      onderwerp = 'Amsterdam'
    ),

  data_v5 |>
    my_v5_function(gebied_stadsdeel_naam) |>
    add_column(thema = "stadsdeel") |>
    mutate(onderwerp = factor(onderwerp, levels = lev_sd)),

  data_v5 |>
    my_v5_function(huishouden_klas) |>
    add_column(thema = "huishoudtype") |>
    mutate(onderwerp = factor(onderwerp, levels = lev_huish)),

  data_v5 |>
    my_v5_function(leeftijd_klas) |>
    add_column(thema = "leeftijdsklasse") |>
    mutate(onderwerp = factor(onderwerp, levels = lev_leefkl)),

  data_v5 |>
    my_v5_function(inkomen_klas) |>
    add_column(thema = "inkomensklasse") |>
    mutate(onderwerp = factor(onderwerp, levels = lev_ink))
) |>
  add_column(
    vraag = 'v5 meest gekozen vervoersmiddel om boodschappen te doen'
  ) |>
  mutate(
    categorie = factor(
      v5,
      levels = c(
        "te voet",
        "fiets",
        "auto of motor",
        "openbaar vervoer",
        "brommer",
        "scootmobiel, canta, Birò",
        "anders",
        "weet niet, geen antwoord"
      )
    )
  )

tabel_v5_achtergr |>
  filter(onderwerp == 'Amsterdam') |>
  my_plot(
    x_var = aandeel_gew,
    y_var = fct_rev(monitor),
    fill_var = categorie,
    color_var = discreet9[c(1, 2, 3, 5, 6, 8, 9, 10)]
  )


ggsave("04 reports/04 figuren/fig_v5_verv_ams.svg", width = 12, height = 4)


tabel_v5_achtergr |>
  mutate(
    onderwerp = factor(
      onderwerp,
      levels = c(lev_huish, lev_ink, lev_sd, lev_leefkl)
    )
  ) |>

  filter(
    monitor == 'monitor 2026',
    onderwerp != 'Amsterdam',
    onderwerp != 'Westpoort',
    onderwerp != 'inkomen onbekend',
    onderwerp != 'overig, onbekend',
    onderwerp != 'onbekend',
    !is.na(onderwerp)
  ) |>

  my_plot(
    x_var = aandeel_gew,
    y_var = fct_rev(onderwerp),
    fill_var = categorie,
    color_var = discreet9[c(1, 2, 3, 5, 6, 8, 9, 10)]
  ) +
  facet_wrap(~thema, scales = "free", ncol = 2)
ggsave("04 reports/04 figuren/fig_v5_verv_ams_acht.svg", width = 12, height = 8)
