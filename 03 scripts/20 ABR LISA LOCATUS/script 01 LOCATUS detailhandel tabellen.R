#  script september 2025
#  tbv monitor detailhandel 2026
#  rogier van der groep

source(
  "02 scr/03 script references.R"
)
source(
  "03 scripts/20 ABR LISA LOCATUS/script 01 LOCATUS detailhandel functies.R"
)

# locatus data voor winkeldynamiek
data_locatus <- readr::read_csv(
  "00 data/ruw locatus/notebook_locatus.csv"
) |>
  mutate(winkelvloeroppervlakte = as.double(winkelvloeroppervlakte))


# amsterdam totaal
df_loc_ams <- data_locatus |>
  my_group_by() |>
  group_by(peildatum, dg_nd) |>
  my_summarise() |>
  group_by(peildatum, name) |>
  mutate(aandeel = value / sum(value)) |>
  add_column(
    gbd_sdl_code = '0363',
    gbd_sdl_naam = 'Amsterdam'
  )

# naar stadsdelen
df_loc_sd <- data_locatus |>
  my_group_by() |>
  group_by(peildatum, gbd_sdl_code, gbd_sdl_naam, dg_nd) |>
  my_summarise() |>
  group_by(peildatum, gbd_sdl_code, gbd_sdl_naam, name) |>
  mutate(aandeel = value / sum(value))

# samenvoegen stadsdelen en amsterdam totaal
df_loc_totaal <- bind_rows(
  df_loc_ams,
  df_loc_sd
) |>
  pivot_longer(
    cols = c(value, aandeel),
    names_to = 'type'
  ) |>
  mutate(
    name = case_when(
      (type == 'aandeel' & name == 'aantal vestigingen') ~ 'vestigingen (%)',
      (type == 'value' & name == 'aantal vestigingen') ~ 'aantal vestigingen',
      (type == 'aandeel' & name == 'winkelvloeroppervlakte (m2)') ~
        'winkelvloeroppervlakte (%)',
      (type == 'value' & name == 'winkelvloeroppervlakte (m2)') ~
        'winkelvloeroppervlakte (m2)'
    ),
    peildatum = as.character(lubridate::year(peildatum))
  )


# tabel met totalen 2019 2025
tab_totaal <- df_loc_totaal |>
  filter(
    dg_nd %in% c('detailhandel dagelijks', 'detailhandel niet-dagelijks'),
    peildatum %in% c('2019', '2025'),
    type == 'value',
    gbd_sdl_code == '0363'
  ) |>
  select(peildatum, dg_nd, value)


######################
### WINKELGEBIEDEN ---
######################

## leegstand naar winkelgebieden
data_locatus_wg <- readr::read_csv2(
  "00 data/ruw locatus/tabel_locatus_leegst_winkelgeb.csv"
) |>
  filter(
    peildatum %in% c('2025-01-01', '2022-01-01', '2019-01-01'),
    !is.na(winkelgebied_code),
    groep != '45-Transp&Brand',
    groep != '80-ATM'
  ) |>
  mutate(vloeroppervlakte = as.double(vloeroppervlakte)) |>
  group_by(peildatum, winkelgebied_naam, winkelgebied_code) |>
  mutate(
    aand_vestigingen = aant_vestigingen / sum(aant_vestigingen) * 100,
    aand_vloeropp = vloeroppervlakte / sum(vloeroppervlakte) * 100
  )
