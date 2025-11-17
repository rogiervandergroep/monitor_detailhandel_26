
source("01 scripts/script 00 references.R")
source("01 scripts/script 20 ABR POLIS functies.R")



opl_levels <- c(
  "basisopgeleid",
  "havo/vwo, mbo-2 t/m 4",
  "hbo of wo",
  "opleiding onbekend"
)

lft_levels <- c(
  "onder de 30",
  "30 jaar tot en met 49 jaar",
  "50 jaar tot en met 66 jaar",
  "67 jaar en ouder",
  "leeftijd onbekend"
)

geb_levels <- c(
  "Centrum",
  "Westpoort",
  "West",
  "Nieuw-West",
  "Zuid",
  "Oost",
  "Noord",
  "Zuidoost",
  "Weesp",
  "Buiten Amsterdam"
)

contract_levels <- c(
  "contract voor bepaalde tijd",
  "contract voor onbepaalde tijd",
  "directeur, aandeelhouder"
)


##########################################
### data naar sector achtergrond vars ####
##########################################

# aandeel van bedrijven per stadsdeel
data_woonwerk_sector_sd <- read.xlsx(
  "00 ruwe data/ruw abr polis/tabel_ap_sector_ww.xlsx"
) |>
  group_by(jaar, gbd_sdl_code, gbd_sdl_naam, sectie) |>
  mutate(aandeel = aantal_banen / sum(aantal_banen)) |>
  my_mutate_sd()

# aandeel van bedrijven per stadsdeel
data_woonwerk_sector_ams <- read.xlsx(
  "00 ruwe data/ruw abr polis/tabel_ap_sector_ww.xlsx"
) |>
  filter(gbd_sdl_naam != "vestiging buiten Amsterdam") |>
  group_by(jaar, woondeelregio_sd, sectie) |>
  summarise(aantal_banen = sum(aantal_banen)) |>
  group_by(jaar, sectie) |>
  mutate(aandeel = aantal_banen / sum(aantal_banen)) |>
  add_column(
    gbd_sdl_code = '0363',
    gbd_sdl_naam = 'Amsterdam'
  ) |>
  my_mutate_sd() |>
  select(jaar, sectie, aantal_banen, aandeel, woondeelregio_sd_naam) |>
  pivot_longer(cols = c(aantal_banen, aandeel)) |>
  group_by(jaar, sectie, woondeelregio_sd_naam, name) |>
  summarise(value = sum(value))


data_sector_ams <- read.xlsx(
  "00 ruwe data/ruw abr polis/tabel_ap_sector_av.xlsx"
) |>
  filter(mra_deelregio == 'Amsterdam') |>
  group_by(jaar, sectie, achtergrond) |>
  mutate(aandeel_banen = aantal_banen / sum(aantal_banen)) |>
  pivot_longer(cols = c(aantal_banen, aandeel_banen, loonperverl_uur)) |>
  mutate(
    achtergrond_type = str_replace_all(
      achtergrond_type,
      "50 jaar tot en met 67 jaar",
      "50 jaar tot en met 66 jaar"
    ),
    jaar = as.numeric(jaar),
    achtergrond_type = str_trim(achtergrond_type),

    name = case_when(
      name == 'loonperverl_uur' ~ '(uitbetaald) loon per uur',
      name == 'aantal_banen' ~ 'aantal banen',
      name == 'aandeel_banen' ~ 'aandeel banen'
    ),

    achtergrond_type2 = case_when(
      achtergrond == 'totaal' ~ 'totaal',
      achtergrond == 'leeftijdsklasse' & is.na(achtergrond_type) ~
        'leeftijd onbekend',
      achtergrond_type == 'laag' ~ "basisopgeleid",
      achtergrond_type == 'midden' ~ "havo/vwo, mbo-2 t/m 4",
      achtergrond_type == 'hoog' ~ "hbo of wo",
      achtergrond_type == 'onbekend' ~ "opleiding onbekend",
      achtergrond_type == 'O' ~ "contract voor onbepaalde tijd",
      achtergrond_type == 'N' ~ "directeur, aandeelhouder",
      achtergrond_type == 'B' ~ "contract voor bepaalde tijd",
      TRUE ~ achtergrond_type
    ),

    achtergrond_type2 = factor(
      achtergrond_type2,
      levels = c(
        opl_levels,
        lft_levels,
        geb_levels,
        contract_levels,
        "totaal"
      )
    )
  )

data_tot_ams2 <- data_sector_ams |>
  mutate(
    sectie = case_when(
      sectie == 'G' ~ 'detailhandel',
      sectie == 'I' ~ 'horeca',
      sectie == 'J' ~ 'ict',
      sectie == 'M' ~ 'advies, onderzoek',
      sectie == 'Q' ~ 'zorg',
      sectie == 'totale economie' ~ 'totale economie',
      TRUE ~ 'overige sectoren'
    )
  ) |>
  filter(sectie != 'overige sectoren') |>
  rename(sector = sectie) |>
  add_column(sector_type = 'totaal') |>
  mutate(
    sector = factor(
      sector,
      levels = c(
        'advies, onderzoek',
        'zorg',
        'detailhandel',
        'ict',
        'horeca',
        'totale economie'
      )
    )
  )

### alleen detailhandel horeca en totaal
data_tot_ams3 <- data_tot_ams2 |>
  filter(sector %in% c('detailhandel', 'horeca', 'totale economie'))



