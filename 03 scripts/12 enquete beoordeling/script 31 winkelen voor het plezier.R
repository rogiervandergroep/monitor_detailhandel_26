library(tidyverse)
library(haven)

data_totaal <- read_rds("01 references/data_totaal_20_22_24_26.rds")

source("02 scr/01 script factor levels.R")
source("02 scr/03 script ggplot functies.R")


# gaat u wel eens winkelen voor het plezier

data_v19 <- data_totaal |>
  map(\(x) janitor::clean_names(x)) |>
  map(\(x) {
    select(
      x,
      monitor,
      ends_with("_klas"),
      starts_with("gebied"),
      v19,
      v20,
      starts_with("v24"),
      weeg_ams_ink
    )
  }) |>
  map(\(x) mutate(x, v19_naam = haven::as_factor(v19))) |>
  map(\(x) {
    mutate(
      x,
      v19_naam = case_when(
        is.na(v19_naam) ~ 'weet ik niet, geen antwoord',
        v19_naam == 'weet niet, geen antwoord' ~ 'weet ik niet, geen antwoord',
        TRUE ~ v19_naam
      )
    )
  })

my_summary <- function(x, groupvars = NULL) {
  x |>
    group_by(monitor, v19_naam, {{ groupvars }}) |>

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

v19_levels <- c(
  "ja, elke dag",
  "ja, enkele keren per week",
  "ja, een tot drie keer per maand",
  "ja, enkele keren per jaar",
  "nee, nooit",
  "weet ik niet, geen antwoord"
)


my_table <- function(x, achtergrondvar, achtergrondvar_naam) {
  x |>

    map(\(x) filter(x, !is.na({{ achtergrondvar }}))) |>
    map_df(\(x) my_summary(x, {{ achtergrondvar }})) |>
    rename(item = {{ achtergrondvar }}) |>
    add_column(onderwerp = achtergrondvar_naam)
}


## Amsterdam totaal ---
tabel_v19 <- bind_rows(
  data_v19 |>
    map_df(\(x) my_summary(x)) |>
    add_column(item = 'Amsterdam') |>
    add_column(onderwerp = 'Amsterdam'),

  data_v19 |>
    my_table(gebied_stadsdeel_naam, 'stadsdeel') |>
    mutate(item = factor(item, levels = lev_sd)),

  data_v19 |>
    my_table(leeftijd_klas, 'leeftijd') |>
    mutate(item = factor(item, levels = lev_leefkl)),

  data_v19 |>
    my_table(inkomen_klas, 'inkomen') |>
    mutate(item = factor(item, levels = lev_ink))
)

data_v19 |>
  map_df(\(x) my_summary(x)) |>
  mutate(v19_naam = factor(v19_naam, levels = v19_levels)) |>
  add_column(onderwerp = 'Amsterdam', item = 'Amsterdam') |>
  mutate(item = factor(item)) |>
  mutate(v19_naam = factor(v19_naam, levels = v19_levels)) |>
  rename(categorie = v19_naam) |>
  add_column(vraag = 'v19 winkelen voor het plezier')


# fig dagelijks niet dagelijks amsterdam
tabel_v19 |>
  filter(
    onderwerp %in% c("Amsterdam", "stadsdeel"),
    item != 'Westpoort'
  ) |>
  mutate(
    aandeel_gew = case_when(
      item == 'Weesp' &
        monitor %in% c("monitor 2020", "monitor 2022") ~ NA,
      TRUE ~ aandeel_gew
    )
  ) |>
  mutate(v19_naam = factor(v19_naam, levels = v19_levels)) |>
  my_plot(
    x_var = aandeel_gew / 100,
    y_var = fct_relevel(fct_rev(item), "Amsterdam"),
    fill_var = v19_naam,
    color_var = blauw_pal[c(9, 8, 7, 5, 3, 1)],
    guide_nr = 2
  ) +
  facet_wrap(~monitor, nrow = 2)

ggsave(
  "04 reports/04 figuren/fig_v19_winkelen_plezier.svg",
  width = 10,
  height = 6
)

# "opleiding_klas",
# "inkomen_klas",
# "huishouden_klas",
# "geslacht",
# "leeftijd_klas

wg_kol_namen <- c(
  "wink_code",
  "wink_naam",
  "wink_stadsdeel_code",
  "wink_stadsdeel_naam",
  "wink_ggw_code",
  "wink_ggw_naam",
  "wink_oisnaam",
  "wink_oiscode"
)


winkelgebieden <- read.xlsx(
  "01 references/winkelgebieden_20_22_24_26_def.xlsx",
  sheet = 1
) |>
  set_names(wg_kol_namen) |>
  mutate(wink_code = str_trim(wink_code))

### meest genoemde winkelgebied ---
# totaal
meest_genoemd <- bind_rows(
  # totaal
  data_v19 |>
    map(\(x) mutate(x, v20_naam = haven::as_factor(v20))) |>
    map(\(x) group_by(x, monitor, v20, v20_naam)) |>
    map_df(\(x) summarise(x, aantal = sum(weeg_ams_ink))) |>
    group_by(monitor) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    add_column(gebied_stadsdeel_naam = "Amsterdam") |>
    mutate(v20 = as.character(v20)),

  # naar stadsdeel
  data_v19 |>
    map(\(x) mutate(x, v20_naam = haven::as_factor(v20))) |>
    map(\(x) group_by(x, monitor, gebied_stadsdeel_naam, v20, v20_naam)) |>
    map_df(\(x) summarise(x, aantal = sum(weeg_ams_ink))) |>
    group_by(monitor, gebied_stadsdeel_naam) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    mutate(v20 = as.character(v20))
) |>
  left_join(winkelgebieden, by = c("v20" = "wink_code")) |>
  mutate(
    wink_oisnaam = case_when(
      is.na(wink_oisnaam) ~ wink_naam,
      TRUE ~ wink_oisnaam
    )
  )

# per stadsdeel
meest_genoemd |>
  filter(
    gebied_stadsdeel_naam != 'Westpoort',
    gebied_stadsdeel_naam != 'Amsterdam',
    !is.na(gebied_stadsdeel_naam),
    monitor == 'monitor 2026',
    !is.na(v20_naam)
  ) |>
  mutate(
    aandeel = case_when(
      gebied_stadsdeel_naam == 'Weesp' &
        monitor %in% c("monitor 2020", "monitor 2022") ~ NA,
      TRUE ~ aandeel
    )
  ) |>
  mutate(
    gebied_stadsdeel_naam = factor(gebied_stadsdeel_naam, levels = lev_sd)
  ) |>
  group_by(gebied_stadsdeel_naam) |>
  slice_max(order_by = aantal, n = 5) |>
  arrange(aandeel) |>

  my_plot_een(
    y_var = fct_reorder(wink_oisnaam, aandeel),
    x_var = aandeel
  ) +
  scale_x_continuous(labels = scales::label_percent()) +
  facet_wrap(~gebied_stadsdeel_naam, scales = 'free', ncol = 2)

ggsave(
  "04 reports/04 figuren/fig_v19_winkstraten_plezier_sd.svg",
  width = 12,
  height = 10
)


#### totaal door de jaren heen
meest_genoemd |>
  filter(
    !is.na(v20_naam),
    gebied_stadsdeel_naam == 'Amsterdam'
  ) |>
  mutate(
    aandeel = case_when(
      gebied_stadsdeel_naam == 'Weesp' &
        monitor %in% c("monitor 2020", "monitor 2022") ~ NA,
      TRUE ~ aandeel
    )
  ) |>
  slice_max(order_by = aantal, n = 10) |>
  my_plot_een(
    y_var = fct_reorder(wink_oisnaam, aandeel),
    x_var = aandeel,
    treshhold = 0
  ) +
  scale_x_continuous(labels = scales::label_percent()) +
  facet_wrap(~monitor, nrow = 2, scales = 'free')


ggsave(
  "04 reports/04 figuren/fig_v19_winkstraten_plezier.svg",
  width = 12,
  height = 8
)
