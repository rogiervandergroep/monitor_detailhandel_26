# Figuren detailhandel en horeca

source("01 scripts/script 21 ABR POLIS tabellen.R")




# aandelen per achtergrondkenmerk
a <- data_tot_ams3 |>
  mutate(
    achtergrond = str_replace_all(achtergrond, "opl3", "opleiding"),
    value = value * 100
  ) |>
  my_figuur_banen_sector(achtergrond_var = c("opleiding"))
ggsave("03 tabellen/fig_abrpolis_opl.svg", height = 4, width = 12)
a

b <- data_tot_ams3 |>
  mutate(
    value = value * 100
  ) |>
  my_figuur_banen_sector(achtergrond_var = c("leeftijdsklasse"))
ggsave("03 tabellen/fig_abrpolis_lft.svg", height = 4, width = 12)
b

# woonwerk
c <- data_woonwerk_sector_ams |>
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
  filter(
    sectie %in% c('detailhandel', 'horeca', 'totale economie')
  ) |>
  mutate(value = value * 100) |>
  mutate(
    sectie = factor(
      sectie,
      levels = c(
        'advies, onderzoek',
        'zorg',
        'detailhandel',
        'ict',
        'horeca',
        'totale economie'
      )
    )
  ) |>
  my_woon_werk_bar()
ggsave("03 tabellen/fig_abrpolis_ww.svg", height = 4, width = 12)

# samenvoegen figuren
a + b + c
ggsave("03 tabellen/fig_abrpolis_totaal.svg", height = 4, width = 12)


# loonontwikkeling
data_tot_ams3 |>
  filter(achtergrond_type2 != 'opleiding onbekend') |>
  my_figuur_uren_lijn("opl3")
ggsave(
  "03 tabellen/fig_abrpolis_sectoren_ink_opl.svg",
  height = 5,
  width = 12
)
