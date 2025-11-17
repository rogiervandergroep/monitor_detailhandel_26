# locatus figuren
# kleurenschema's staan in script 00 references

source(
  "03 scripts/20 ABR LISA LOCATUS/script 01 LOCATUS detailhandel tabellen.R"
)


### lijnfiguur ---
df_loc_ams |>
  # bij horeca en diensten is de winkelvloeroppervlakte pas beschikbaar vanaf 2020
  filter(
    dg_nd != 'leegstand',
    case_when(
      dg_nd == "horeca en vrije tijd" ~ name != 'winkelvloeroppervlakte (m2)',
      dg_nd == "diensten" ~ name != 'winkelvloeroppervlakte (m2)',
      TRUE ~ name %in% c('winkelvloeroppervlakte (m2)', 'aantal vestigingen')
    )
  ) |>
  ggplot(aes(
    x = lubridate::year(peildatum),
    y = value,
    group = dg_nd,
    colour = dg_nd
  )) +
  geom_line(linewidth = 0.9) +
  labs(y = NULL, x = NULL) +
  theme_os(orientation = "vertical") +
  scale_color_manual(name = NULL, values = discreet) +
  scale_linewidth_manual(name = NULL, values = c(0.8, 1, 2)) +
  guides(colour = guide_legend(nrow = 1, reverse = T), linewidth = 'none') +
  scale_x_continuous(
    limits = c(2010, 2025),
    breaks = c(2010, 2015, 2020, 2025)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = scales::label_comma(big.mark = ".", decimal.mark = ",")
  ) +
  facet_wrap(~name, scale = 'free_y')
ggsave("04 reports/04 figuren/fig_locatus_01.svg", width = 12, height = 4)

### functie voor staafdiagrammen ---
# defaultkleurenschema - blauwpal

### percentage leegstand per stadsdeel ---
df_loc_totaal |>
  # bij horeca en diensten is de winkelvloeroppervlakte pas beschikbaar vanaf 2020
  filter(
    gbd_sdl_code != 'B',
    dg_nd == 'leegstand',
    peildatum %in% c("2019", "2025")
  ) |>
  my_col_figure(aan_per = 'aandeel')
ggsave(
  "04 reports/04 figuren/fig_locatus_02_leegstand.svg",
  width = 12,
  height = 5
)

### vestigignen en oppervlakte dagelijks ---
df_loc_totaal |>
  # bij horeca en diensten is de winkelvloeroppervlakte pas beschikbaar vanaf 2020
  filter(
    gbd_sdl_naam != 'Westpoort',
    gbd_sdl_naam != 'Amsterdam',
    dg_nd == 'detailhandel dagelijks',
    peildatum %in% c("2019", "2025")
  ) |>
  my_col_figure(aan_per = 'value')
ggsave(
  "04 reports/04 figuren/fig_locatus_02_dagelijks.svg",
  width = 12,
  height = 5
)

#### vestigignen en oppervlakte niet-dagelijks ---
df_loc_totaal |>
  # bij horeca en diensten is de winkelvloeroppervlakte pas beschikbaar vanaf 2020
  filter(
    gbd_sdl_naam != 'Westpoort',
    gbd_sdl_naam != 'Amsterdam',
    dg_nd == 'detailhandel niet-dagelijks',
    peildatum %in% c("2019", "2025")
  ) |>
  my_col_figure(aan_per = 'value')
ggsave(
  "04 reports/04 figuren/fig_locatus_02_niet-dagelijks.svg",
  width = 12,
  height = 5
)
