# V5 hoe gaat u normaliter boodschappen doen?
library(tidyverse)
library(haven)

data_totaal <- read_rds("01 references/data_totaal_20_22_24_26.rds")

source("02 scr/01 script factor levels.R")
source("02 scr/03 script ggplot functies.R")


data_v2 <- data_totaal |>
  map(\(x) {
    select(
      x,
      monitor,
      v2,
      starts_with("v3"),
      ends_with("_klas"),
      starts_with("gebied"),
      weeg_ams_ink
    )
  }) |>
  map(\(x) janitor::clean_names(x))

data_v2$data_20_weeg <- data_v2$data_20_weeg |>
  rename(
    v3_rij1_num = v3_1,
    v3_rij2_num = v3_2,
    v3_rij3_num = v3_3,
    v3_rij4_num = v3_4,
    v3_rij5_num = v3_5
  )

data_v2 <- data_v2 |>
  bind_rows() |>
  filter(!is.na(v2)) |>
  mutate(across(v3_rij1_num:v3_rij5_num, ~ replace_na(.x, 0)))


my_v2_function <- function(x, var, var_levels) {
  x |>
    group_by(monitor, onderwerp = {{ var }}) |>
    summarise(
      gem_uitgaven = mean(v2, na.rm = T),
      gem_uitgaven_gew = weighted.mean(v2, w = weeg_ams_ink, na.rm = T)
    )
}

tabel_v2_uitgaven <- bind_rows(
  data_v2 |>
    group_by(monitor) |>
    summarise(
      gem_uitgaven = mean(v2, na.rm = T),
      gem_uitgaven_gew = weighted.mean(v2, w = weeg_ams_ink, na.rm = T)
    ) |>
    add_column(
      thema = "Amsterdam",
      onderwerp = 'Amsterdam'
    ),

  data_v2 |>
    my_v2_function(gebied_stadsdeel_naam) |>
    add_column(thema = "stadsdeel") |>
    mutate(onderwerp = factor(onderwerp, levels = lev_sd)),

  data_v2 |>
    my_v2_function(huishouden_klas) |>
    add_column(thema = "huishoudtype") |>
    mutate(onderwerp = factor(onderwerp, levels = lev_huish)),

  data_v2 |>
    my_v2_function(leeftijd_klas) |>
    add_column(thema = "leeftijdsklasse") |>
    mutate(onderwerp = factor(onderwerp, levels = lev_leefkl)),

  data_v2 |>
    my_v2_function(inkomen_klas) |>
    add_column(thema = "inkomensklasse") |>
    mutate(onderwerp = factor(onderwerp, levels = lev_ink))
) |>
  add_column(
    vraag = 'v2 wekelijkse uitgaven aan boodschapppen'
  )

tabel_v2_uitgaven |>
  filter(onderwerp == 'Amsterdam') |>
  my_plot_een(
    x_var = gem_uitgaven_gew,
    y_var = fct_rev(monitor)
  )


ggsave("04 reports/04 figuren/fig_v2_uitgaven_ams.svg", width = 12, height = 4)


tabel_v2_uitgaven |>
  mutate(
    onderwerp = factor(
      onderwerp,
      levels = c(lev_huish, lev_ink, lev_sd, lev_leefkl)
    )
  ) |>

  filter(
    onderwerp != 'Westpoort',
    monitor == 'monitor 2026',
    onderwerp != 'Amsterdam',
    onderwerp != 'inkomen onbekend',
    onderwerp != 'overig, onbekend',
    onderwerp != 'onbekend',
    !is.na(onderwerp)
  ) |>

  my_plot_een(
    x_var = gem_uitgaven_gew,
    y_var = fct_rev(onderwerp)
  ) +
  facet_wrap(~thema, scales = "free", ncol = 2)
ggsave("04 reports/04 figuren/fig_v2_uitgaven.svg", width = 12, height = 8)


### v3 verdeeld

my_v3_function <- function(x, var, var_levels) {
  x |>
    group_by(monitor, onderwerp = {{ var }}) |>
    summarise(
      v3_1 = mean(v3_rij1_num, na.rm = T),
      v3_2 = mean(v3_rij2_num, na.rm = T),
      v3_3 = mean(v3_rij3_num, na.rm = T),
      v3_4 = mean(v3_rij4_num, na.rm = T),
      v3_5 = mean(v3_rij5_num, na.rm = T),
    )
}


tabel_v3 <- bind_rows(
  data_v2 |>
    group_by(monitor) |>
    summarise(
      v3_1 = mean(v3_rij1_num, na.rm = T),
      v3_2 = mean(v3_rij2_num, na.rm = T),
      v3_3 = mean(v3_rij3_num, na.rm = T),
      v3_4 = mean(v3_rij4_num, na.rm = T),
      v3_5 = mean(v3_rij5_num, na.rm = T)
    ) |>
    add_column(
      thema = "Amsterdam",
      onderwerp = 'Amsterdam'
    ),

  data_v2 |>
    my_v3_function(gebied_stadsdeel_naam) |>
    add_column(thema = "stadsdeel") |>
    mutate(onderwerp = factor(onderwerp, levels = lev_sd)),

  data_v2 |>
    my_v3_function(huishouden_klas) |>
    add_column(thema = "huishoudtype") |>
    mutate(onderwerp = factor(onderwerp, levels = lev_huish)),

  data_v2 |>
    my_v3_function(leeftijd_klas) |>
    add_column(thema = "leeftijdsklasse") |>
    mutate(onderwerp = factor(onderwerp, levels = lev_leefkl)),

  data_v2 |>
    my_v3_function(inkomen_klas) |>
    add_column(thema = "inkomensklasse") |>
    mutate(onderwerp = factor(onderwerp, levels = lev_ink))
) |>
  add_column(
    vraag = 'v3 uistplitsing kosten'
  ) |>
  mutate(v3_5 = 100 - (v3_1 + v3_2 + v3_3 + v3_4)) |>
  pivot_longer(cols = c(v3_1:v3_5)) |>
  mutate(
    name_name = case_when(
      name == 'v3_1' ~ 'in de supermarkt',
      name == 'v3_2' ~ 'in andere winkels',
      name == 'v3_3' ~ 'op de markt',
      name == 'v3_4' ~ 'online',
      name == 'v3_5' ~ 'anders'
    )
  ) |>
  mutate(
    name_name = factor(
      name_name,
      levels = c(
        'in de supermarkt',
        'in andere winkels',
        'op de markt',
        'online',
        'anders'
      )
    )
  )


tabel_v3 |>
  filter(onderwerp == 'Amsterdam') |>
  my_plot(
    x_var = value / 100,
    y_var = fct_rev(monitor),
    fill_var = name_name,
    color_var = discreet9[c(1, 3, 5, 8, 10)]
  )


ggsave("04 reports/04 figuren/fig_v3_cat_ams.svg", width = 12, height = 4)


tabel_v3 |>
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
    x_var = value / 100,
    y_var = fct_rev(onderwerp),
    fill_var = name_name,
    color_var = discreet9[c(1, 3, 5, 8, 10)]
  ) +
  facet_wrap(~thema, scales = "free", ncol = 2)
ggsave("04 reports/04 figuren/fig_v3_cat_acht.svg", width = 12, height = 8)
