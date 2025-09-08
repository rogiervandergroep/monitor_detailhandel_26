library(tidyverse)


my_compact <- function(x) {
  x |>
    mutate(
      sbi_sectie_omschrijving = case_when(
        sbi_sectie_code %in%
          c("A", "B", "C", "D", "E", "L", "N", "S", "U", "null") ~
          "overig",
        TRUE ~ sbi_sectie_omschrijving
      )
    ) |>
    mutate(
      sbi_sectie_code = case_when(
        sbi_sectie_code %in%
          c("A", "B", "C", "D", "E", "L", "N", "S", "U", "null") ~
          "Z",
        TRUE ~ sbi_sectie_code
      )
    )
}

my_name_mutate <- function(x) {
  x |>

    mutate(
      sbi_sectie_omschrijving = case_when(
        sbi_sectie_code == 'I' ~ 'Horeca',
        sbi_sectie_code == 'J' ~ 'ICT',
        sbi_sectie_code == 'M' ~ 'Advies, onderzoek',
        sbi_sectie_code == 'N' ~ 'Verhuur van roerende goederen',
        sbi_sectie_code == 'O' ~ 'Overheid',
        sbi_sectie_code == 'Q' ~ 'Zorg',

        TRUE ~ sbi_sectie_omschrijving
      )
    ) |>
    mutate(sbi_sectie_omschrijving = str_to_lower(sbi_sectie_omschrijving))
}


########################
### totaal amsterdam ---
########################

df_totaal_ams <- readr::read_csv("00 ruwe data/notebook_lisa_totaal_ams.csv") |>
  pivot_longer(cols = c("aant_vestigingen", "aant_werkz_pers")) |>
  mutate(
    name = case_when(
      name == 'aant_werkz_pers' ~ 'aantal banen',
      name == 'aant_vestigingen' ~ 'aantal vestigingen'
    )
  ) |>
  group_by(name) |>
  mutate(groei = round((value - first(value)) / first(value) * 100))

##########################
### totaal naar sector ---
##########################

df_totaal_ams_sbi <- readr::read_csv(
  "00 ruwe data/notebook_lisa_totaal_ams_sbi.csv"
)

df_totaal_ams_compact <- df_totaal_ams_sbi |>
  my_compact() |>
  group_by(peildatum, sbi_sectie_code, sbi_sectie_omschrijving) |>
  summarise(
    `aantal vestigingen` = sum(aant_vestigingen),
    `aantal banen` = sum(aant_werkz_pers)
  ) |>
  pivot_longer(cols = c(`aantal vestigingen`, `aantal banen`)) |>
  group_by(name, sbi_sectie_code, sbi_sectie_omschrijving) |>
  mutate(index = value / first(value) * 100) |>
  group_by(name, peildatum) |>
  mutate(aandeel = value / sum(value) * 100) |>
  my_name_mutate()


tabel_wide <- df_totaal_ams_compact |>
  pivot_longer(cols = c(value, index, aandeel), names_to = 'unit') |>
  select(-sbi_sectie_code)


tabel_wide2 <- split.data.frame(tabel_wide, tabel_wide$unit)


tabel_wide3 <- tabel_wide2 |>
  map(\(x) {
    mutate(
      x,
      sbi_sectie_omschrijving = fct_reorder(sbi_sectie_omschrijving, value)
    )
  }) |>
  map(\(x) {
    pivot_wider(
      x,
      values_from = value,
      names_from = sbi_sectie_omschrijving
    )
  })


openxlsx::write.xlsx(
  tabel_wide3,
  "04 output tabellen/tabel_lisa_totaal_wide.xlsx"
)
#############################
# totaal naar bedrijfsgrootte
#############################

kl_levels = c(
  '1 werkzaam persoon',
  '2 tot 9 werkzame personen',
  '10 tot 49 werkzame personen',
  '50 tot 199 werkzame personen',
  '200 werkzame personen of meer'
)

# totaal naar bedrijfsgrootte
df_totaal_ams_gr <- readr::read_csv(
  "00 ruwe data/notebook_lisa_totaal_ams_grootte.csv"
) |>
  pivot_longer(cols = c("aant_vestigingen", "aant_werkz_pers")) |>
  mutate(
    name = case_when(
      name == 'aant_werkz_pers' ~ 'aantal banen',
      name == 'aant_vestigingen' ~ 'aantal vestigingen'
    )
  ) |>
  mutate(
    klasse = case_when(
      klasse_wp_code %in% c('01', '02') ~ '1 werkzaam persoon',
      klasse_wp_code %in% c('03', '04') ~ '2 tot 9 werkzame personen',
      klasse_wp_code %in% c('05', '06') ~ '10 tot 49 werkzame personen',
      klasse_wp_code %in% c('07', '08') ~ '50 tot 199 werkzame personen',
      TRUE ~ '200 werkzame personen of meer'
    )
  ) |>
  group_by(peildatum, name, klasse) |>
  summarise(value = sum(value)) |>
  mutate(
    klasse = factor(
      klasse,
      levels = kl_levels
    )
  ) |>
  group_by(peildatum, name) |>
  mutate(aandeel = value / sum(value))


df_totaal_ams_gr_wide <- df_totaal_ams_gr |>
  select(-aandeel) |>
  pivot_wider(names_from = klasse, values_from = value)

###########################################
# totaal naar eenmanszaken vs grotere zaken
###########################################

df_totaal_ams_sbi2 <- readr::read_csv(
  "00 ruwe data/notebook_lisa_totaal_wp_twee_ams.csv"
)

df_totaal_ams_sbi2_compact <- df_totaal_ams_sbi2 |>
  my_compact() |>
  group_by(
    peildatum,
    sbi_sectie_code,
    sbi_sectie_omschrijving,
    klasse_wp_twee
  ) |>
  summarise(
    `aantal vestigingen` = sum(aant_vestigingen),
    `aantal banen` = sum(aant_werkz_pers)
  ) |>
  pivot_longer(cols = c(`aantal vestigingen`, `aantal banen`)) |>
  group_by(name, sbi_sectie_code, sbi_sectie_omschrijving, klasse_wp_twee) |>
  mutate(groei = (value - first(value)) / first(value) * 100) |>
  my_name_mutate()

df_totaal_ams_sbi2_compact_wide <- df_totaal_ams_sbi2_compact |>
  select(-value) |>
  filter(peildatum == '2024-04-01') |>
  pivot_wider(values_from = groei, names_from = klasse_wp_twee)


openxlsx::write.xlsx(
  list(
    df_totaal_ams_gr_wide,
    df_totaal_ams_sbi2_compact_wide
  ),
  "04 output tabellen/tabel_lisa_gr_wide.xlsx"
)

####################
# totaal naar wijken
####################

df_totaal_wijk <- readr::read_csv(
  "00 ruwe data/notebook_lisa_totaal_wijk.csv"
) |>
  filter(gbd_wijk_naam != 'null') |>
  pivot_longer(
    cols = c(aant_vestigingen, aant_werkz_pers)
  ) |>
  group_by(name, gbd_wijk_naam, gbd_wijk_code) |>
  mutate(groei = round((value - first(value)) / first(value), 1)) |>
  group_by(peildatum, name) |>
  mutate(aandeel = round(value / sum(value) * 100, 1))


df_sbi_wijk_compact <- readr::read_csv(
  "00 ruwe data/notebook_lisa_sbi_wijk.csv"
) |>
  filter(gbd_wijk_naam != 'null') |>
  my_compact() |>
  pivot_longer(
    cols = c(aant_vestigingen, aant_werkz_pers)
  ) |>
  group_by(
    peildatum,
    gbd_wijk_naam,
    gbd_wijk_code,
    sbi_sectie_code,
    sbi_sectie_omschrijving,
    name
  ) |>
  summarise(value = sum(value)) |>
  mutate(groei = (value - first(value)) / first(value)) |>
  group_by(peildatum, name) |>
  mutate(aandeel = value / sum(value))


####################
# eenmanszaak per wijk
####################

df_wijk_wp2 <- readr::read_csv("00 ruwe data/notebook_lisa_wijk_wp2.csv") |>
  filter(gbd_wijk_naam != 'null') |>
  pivot_longer(
    cols = c(aant_vestigingen, aant_werkz_pers)
  ) |>
  group_by(name, gbd_wijk_naam, gbd_wijk_code, klasse_wp_twee) |>
  mutate(groei = (value - first(value)) / first(value)) |>
  group_by(peildatum, gbd_wijk_naam, gbd_wijk_code, name) |>
  mutate(aandeel = value / sum(value) * 100)


library(openxlsx)
write.xlsx(
  list(
    df_totaal_ams,
    df_totaal_ams_gr,
    df_totaal_ams_sbi,
    df_totaal_ams_sbi2_compact,
    df_totaal_ams_compact,
    df_wijk_wp2,
    df_totaal_wijk
  ),
  "04 output tabellen/tabel_lisa_totaal.xlsx"
)
