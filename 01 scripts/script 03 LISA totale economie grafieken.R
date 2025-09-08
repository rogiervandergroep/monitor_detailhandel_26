library(svglite)
source("01 scripts/SvdS XIV script 20 LISA totale economie basis.R")
source("01 scripts/SvdS XIV script 21 LISA grafieken.R")

### lijnfiguren banen en vestigingen totaal ---

df_totaal_ams |>
  my_line_figure(
    fil_var = name
  )
ggsave("03 output figuren/fig_lisa_wp_vest_totaal.svg", width = 12, height = 6)


### staafdiagrammen banen en vestigingen totaal ---
df_totaal_ams_compact |>
  my_col_figure(
    group_var = fct_rev(fct_relevel(
      fct_reorder(sbi_sectie_omschrijving, value),
      "overig"
    )),
    facet_var = fct_rev(name)
  )
ggsave("03 output figuren/fig_lisa_sbi.svg", width = 12, height = 6)

df_totaal_ams_compact |>
  my_col_figure(
    y_var = aandeel,
    group_var = fct_rev(fct_relevel(
      fct_reorder(sbi_sectie_omschrijving, aandeel),
      "overig"
    ))
  ) +
  facet_wrap(~ fct_rev(name), scales = 'free_y', ncol = 1)
ggsave("03 output figuren/fig_lisa_sbi_aand.svg", width = 12, height = 6)


### index lijndiagram banen en vestigingen grootste sector = ---
df_totaal_ams_compact |>
  filter(
    sbi_sectie_code %in% c("M", "Q", "G2", "I", "J")
  ) |>
  my_line_figure(
    y_val = index,
    fil_var = sbi_sectie_omschrijving
  ) +
  ylim(c(80, 200)) +
  facet_wrap(~ fct_rev(name), ncol = 1)
ggsave("03 output figuren/fig_lisa_index_sbi.svg", width = 12, height = 6)

# vestigingen en banen naar klasse werkzame personen
df_totaal_ams_gr |>
  my_col_figure(
    facet_var = fct_rev(name),
    group_var = klasse
  ) +
  scale_fill_manual(name = NULL, values = discreet)
ggsave("03 output figuren/fig_lisa_wp_gr.svg", width = 12, height = 6)


# groei eenmanszaken en overige zaken per sector
df_totaal_ams_sbi2_compact |>
  filter(
    sbi_sectie_omschrijving != 'overig',
    sbi_sectie_omschrijving != 'overheid',
    sbi_sectie_omschrijving != 'cultuur, sport en recreatie',
    sbi_sectie_omschrijving != 'detailhandel',
    sbi_sectie_omschrijving != 'groothandel',
    sbi_sectie_omschrijving != 'financiële instellingen',

    peildatum %in% c("2024-04-01")
  ) |>
  my_name_mutate() |>
  my_col_figure_2(
    positie = "dodge",
    x_var = groei,
    y_var = sbi_sectie_omschrijving,
    group_var = klasse_wp_twee
  ) +
  theme_os2(orientation = "horizontal", legend_position = "bottom") +
  facet_wrap(~ fct_rev(name), scales = "free_y", nrow = 1)
ggsave("03 output figuren/fig_lisa_wp2_vest.svg", width = 12, height = 6)


groen <- c("#00a03c", "#5bb667", "#bbe0bc", "#e8f5e8")
grijs <- "#e6e6e6"

my_gsub <- function(x, var_klas) {
  x |>
    mutate(
      "{{ var_klas }}" := gsub("\\[", "", {{ var_klas }}),
      "{{ var_klas }}" := gsub("\\(", "", {{ var_klas }}),
      "{{ var_klas }}" := gsub(",", " - ", {{ var_klas }}),
      "{{ var_klas }}" := gsub("\\]", "", {{ var_klas }})
    )
}

my_gsub_procent <- function(x, var_klas) {
  x |>
    mutate(
      "{{ var_klas }}" := gsub("\\[", "", {{ var_klas }}),
      "{{ var_klas }}" := gsub("\\(", "", {{ var_klas }}),
      "{{ var_klas }}" := gsub(",", "% - ", {{ var_klas }}),
      "{{ var_klas }}" := gsub("\\]", "%", {{ var_klas }})
    )
}

# basiskaarten wijk
kaart_wijk_totaal <- os_get_geom("wijken") |>
  right_join(df_totaal_wijk, by = c("code" = "gbd_wijk_code")) |>
  ungroup() |>
  filter(peildatum == '2024-04-01') |>
  group_by(name) |>
  mutate(
    value_kl = gtools::quantcut(value, na.rm = T, dig.lab = 5),
    groei_kl = gtools::quantcut(groei, na.rm = T),
    aandeel_kl = gtools::quantcut(aandeel, na.rm = T)
  ) |>
  my_gsub(var_klas = value_kl) |>
  my_gsub_procent(var_klas = groei_kl) |>
  my_gsub_procent(var_klas = aandeel_kl)


kaart_wijk_totaal |>
  filter(
    name == 'aant_werkz_pers'
  ) |>
  mutate(
    value_kl = factor(
      value_kl,
      levels = c(
        "283 - 2295.8",
        "2295.8 - 3552",
        "3552 - 8713.8",
        "8713.8 - 60877"
      )
    )
  ) |>
  ggplot() +
  geom_sf(color = "white", aes(fill = value_kl), linewidth = 0.4) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_manual(name = NULL, values = groen[c(4, 3, 2, 1)]) +
  theme_os3(legenda_pos = 'right') +
  guides(fill = guide_legend(ncol = 1))
ggsave("03 output figuren/fig_lisa_wijk_banen.svg", width = 12, height = 5)

kaart_wijk_totaal |>
  filter(
    name == 'aant_werkz_pers'
  ) |>
  # mutate(value_kl = factor(value_kl, levels = c(
  #   "283 - 2295.8", "2295.8 - 3552", "3552 - 8713.8", "8713.8 - 60877")
  # ))|>
  ggplot() +
  geom_sf(color = "white", aes(fill = groei_kl), linewidth = 0.4) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_manual(name = NULL, values = groen[c(4, 3, 2, 1)]) +
  theme_os3(legenda_pos = 'right') +
  guides(fill = guide_legend(ncol = 1))
ggsave("03 output figuren/fig_lisa_wijk_groei.svg", width = 12, height = 5)
