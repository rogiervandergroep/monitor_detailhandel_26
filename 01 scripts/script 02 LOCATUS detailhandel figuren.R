# locatus figuren

# kleurenschema's staan in script 00 references

source("01 scripts/script 01 LOCATUS detailhandel tabellen.R")

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
#ggsave("03 tabellen/fig_locatus_01.svg", width = 12, height = 4)


df_loc_totaal |>
  # bij horeca en diensten is de winkelvloeroppervlakte pas beschikbaar vanaf 2020
  filter(
    gbd_sdl_code != 'B',
    dg_nd == 'leegstand',
    peildatum %in% c("2019-01-01", "2023-01-01", "2025-01-01")
  ) |>
  mutate(
    name = case_when(
      name == "aantal vestigingen" ~ "vestigingen (%)",
      name == "winkelvloeroppervlakte (m2)" ~ "vloeroppervlakte (%)"
    ),
    peildatum = as.character(lubridate::year(peildatum))
  ) |>
  ggplot(aes(
    x = aandeel * 100,
    y = fct_relevel(fct_rev(gbd_sdl_naam), "Amsterdam"),
    group = fct_rev(peildatum),
    fill = peildatum
  )) +
  geom_col(position = "dodge") +
  labs(y = NULL, x = NULL) +
  theme_os(orientation = "vertical") +
  scale_fill_manual(name = NULL, values = blauw_pal[c(6, 3, 1)]) +
  guides(colour = guide_legend(nrow = 1, reverse = T)) +
  facet_wrap(~name)
#ggsave("03 tabellen/fig_locatus_02.svg", width = 12, height = 4)
