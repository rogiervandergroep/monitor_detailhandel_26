###################################
# rapportcijfers per winkelgebied -
###################################

library(tidyverse)

# vragen -
rap <- read_rds("01 references/vragen_rapportcijfers.rds")
# data -
list_rapportcijfers <- read_rds("01 references/data_rapportcijfers.rds")


gem_rap <- bind_rows(
  # rapportcijfer per item per winkelgebied Dg
  list_rapportcijfers[["rap_20_dg"]] |>
    pivot_longer(cols = any_of(rap[["20_22_v7"]])) |>
    group_by(
      wink_code,
      wink_naam,
      wink_stadsdeel_code,
      wink_stadsdeel_naam,
      name,
      productgroep
    ) |>
    summarise(
      aantal = n(),
      gemiddelde = round(mean(value, na.rm = T), 2)
    ),

  # gemiddelde rapportcijfer per winkelgebied Dg
  df_rap_dg |>
    pivot_longer(cols = any_of(rap_v7)) |>
    group_by(
      wink_code,
      wink_naam,
      afzet_stadsdeel_code,
      name,
      productgroep
    ) |>
    summarise(aantal = n(), gemiddelde = round(mean(value, na.rm = T), 2)) |>
    filter(item != 'Wat is uw totaaloordeel over dit winkelgebied?') |>
    group_by(
      winkel_code,
      winkel_naam,
      afzet_stadsdeel_code,
      productgroep
    ) |>
    summarise(
      aantal = mean(aantal),
      gemiddelde = round(mean(gemiddelde, na.rm = T), 2)
    ) |>
    add_column(item = 'gemiddeld rapportcijfer'),

  # rapportcijfer per item per winkelgebied NDg
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by = "name") |>
    group_by(
      winkel_code,
      winkel_naam,
      afzet_stadsdeel_code,
      item = labels,
      productgroep
    ) |>
    summarise(aantal = n(), gemiddelde = round(mean(value, na.rm = T), 2)),

  # rapportcijfer per winkelgebied NDg
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by = "name") |>
    group_by(
      winkel_code,
      winkel_naam,
      afzet_stadsdeel_code,
      item = labels,
      productgroep
    ) |>
    summarise(aantal = n(), gemiddelde = round(mean(value, na.rm = T), 2)) |>
    filter(item != 'Wat is uw totaaloordeel over dit winkelgebied?') |>
    group_by(
      winkel_code,
      winkel_naam,
      afzet_stadsdeel_code,
      productgroep
    ) |>
    summarise(
      aantal = mean(aantal),
      gemiddelde = round(mean(gemiddelde, na.rm = T), 2)
    ) |>
    add_column(item = 'gemiddeld rapportcijfer')
)

############################
# rapportcijfers Amsterdam -
############################

gem_rap_ams <- bind_rows(
  # rapportcijfers per item amsterdam Dg
  df_rap_dg |>
    pivot_longer(cols = any_of(rap_v7)) |>
    left_join(labels_dg, by = "name") |>
    group_by(item = labels, productgroep) |>
    summarise(aantal = n(), gemiddelde = round(mean(value, na.rm = T), 2)),

  # gemiddeld raportcijfer amsterdam Dg
  df_rap_dg |>
    pivot_longer(cols = any_of(rap_v7)) |>
    left_join(labels_dg, by = "name") |>
    group_by(item = labels, productgroep) |>
    summarise(aantal = n(), gemiddelde = round(mean(value, na.rm = T), 2)) |>
    filter(item != 'Wat is uw totaaloordeel over dit winkelgebied?') |>
    group_by(productgroep) |>
    summarise(
      aantal = mean(aantal),
      gemiddelde = round(mean(gemiddelde, na.rm = T), 2)
    ) |>
    add_column(item = 'gemiddeld rapportcijfer'),

  # rapportcijfers per item amsterdam NDg
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by = "name") |>
    group_by(item = labels, productgroep) |>
    summarise(aantal = n(), gemiddelde = round(mean(value, na.rm = T), 2)),

  # gemiddeld raportcijfer amsterdam NDg
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by = "name") |>
    group_by(item = labels, productgroep) |>
    summarise(aantal = n(), gemiddelde = round(mean(value, na.rm = T), 2)) |>
    filter(item != 'Wat is uw totaaloordeel over dit winkelgebied?') |>
    group_by(productgroep) |>
    summarise(
      aantal = mean(aantal),
      gemiddelde = round(mean(gemiddelde, na.rm = T), 2)
    ) |>
    add_column(item = 'gemiddeld rapportcijfer')
) |>
  add_column(
    winkelgebied_code = 'ams',
    winkelgebied_naam = 'Amsterdam totaal'
  )


############################
# rapportcijfers stadsdeel -
############################

gem_rap_sd <- bind_rows(
  # rapportcijfers per item sd Dg
  df_rap_dg |>
    pivot_longer(cols = any_of(rap_v7)) |>
    left_join(labels_dg, by = "name") |>
    group_by(item = labels, productgroep, afzet_stadsdeel_code) |>
    summarise(aantal = n(), gemiddelde = round(mean(value, na.rm = T), 2)),

  # gemiddeld raportcijfer sd Dg
  df_rap_dg |>
    pivot_longer(cols = any_of(rap_v7)) |>
    left_join(labels_dg, by = "name") |>
    group_by(item = labels, productgroep, afzet_stadsdeel_code) |>
    summarise(aantal = n(), gemiddelde = round(mean(value, na.rm = T), 2)) |>
    filter(item != 'Wat is uw totaaloordeel over dit winkelgebied?') |>
    group_by(productgroep, afzet_stadsdeel_code) |>
    summarise(
      aantal = mean(aantal),
      gemiddelde = round(mean(gemiddelde, na.rm = T), 2)
    ) |>
    add_column(item = 'gemiddeld rapportcijfer'),

  # rapportcijfers per item sd NDg
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by = "name") |>
    group_by(item = labels, productgroep, afzet_stadsdeel_code) |>
    summarise(aantal = n(), gemiddelde = round(mean(value, na.rm = T), 2)),

  # gemiddeld raportcijfer sd NDg
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by = "name") |>
    group_by(item = labels, productgroep, afzet_stadsdeel_code) |>
    summarise(aantal = n(), gemiddelde = round(mean(value, na.rm = T), 2)) |>
    filter(item != 'Wat is uw totaaloordeel over dit winkelgebied?') |>
    group_by(productgroep, afzet_stadsdeel_code) |>
    summarise(
      aantal = mean(aantal),
      gemiddelde = round(mean(gemiddelde, na.rm = T), 2)
    ) |>
    add_column(item = 'gemiddeld rapportcijfer')
) |>

  mutate(
    winkelgebied_naam = case_when(
      afzet_stadsdeel_code == "A" ~ "Centrum",
      afzet_stadsdeel_code == "E" ~ "West",
      afzet_stadsdeel_code == "F" ~ "Nieuw-West",
      afzet_stadsdeel_code == "K" ~ "Zuid",
      afzet_stadsdeel_code == "M" ~ "Oost",
      afzet_stadsdeel_code == "N" ~ "Noord",
      afzet_stadsdeel_code == "S" ~ "Weesp",
      afzet_stadsdeel_code == "T" ~ "Zuidoost"
    )
  )

###############
# categorieën -
###############

totaal <- c(
  "totaaloordeel winkelgebied"
)

gemiddelde <- c(
  "gemiddeld rapportcijfer"
)

sfeer <- c(
  "uiterlijk van de winkels",
  "aankleding en inrichting",
  "sfeer en de gezelligheid"
)

aanbod <- c(
  "aanbod van daghoreca",
  "het algemeen prijsniveau",
  "het diverse aanbod van food-winkels",
  "het diverse aanbod van non-food-winkels"
)

duurzaam_bio <- c(
  "het biologische/duurzame aanbod van food-winkels",
  "het duurzame aanbod van non-food winkels"
)

overlast <- c(
  "het schoonhouden van de straten",
  "overlast door horeca",
  "overlast van andere mensen",
  "veiligheid winkelomgeving overdag",
  "veiligheid winkelomgeving ‘s avonds",
  "overlast door vervuiling"
)

bereik <- c(
  "parkeermogelijkheden voor auto",
  "parkeermogelijkheden voor fiets",
  "algemene bereikbaarheid"
)


# samenvoegen -
gem_totaal <- bind_rows(gem_rap, gem_rap_ams, gem_rap_sd) |>

  mutate(
    item = str_trim(item, "both")
  ) |>

  mutate(
    item = case_when(
      item ==
        "Wat is uw totaaloordeel over dit winkelgebied?" ~ "totaaloordeel winkelgebied",
      item ==
        "het biologische/duurzame aanbod van food-winkels" ~ "het biologische/duurzame aanbod food",
      item ==
        "het schoonhouden van de straten/ stoepen/ passage" ~ "schoonhouden van de straten",
      item ==
        "de sfeer en de gezelligheid van het winkelgebied" ~ "sfeer en de gezelligheid",
      item ==
        "de aankleding en inrichting van het winkelgebied (faciliteiten, verlichting, bankjes)" ~ "aankleding en inrichting",
      item ==
        "het uiterlijk van de winkels (denk aan gevels, etalages, inrichting)" ~ "uiterlijk van de winkels",
      TRUE ~ item
    )
  ) |>

  mutate(
    thema = case_when(
      item %in% sfeer ~ 'sfeer en gezelligheid',
      item %in% totaal ~ 'totaal oordeel',
      item == "gemiddeld rapportcijfer" ~ 'gemiddelde rapportcijfer',
      item %in% duurzaam_bio ~ 'duurz. en biol. producten',
      item %in% aanbod ~ 'aanbod producten',
      item %in% overlast ~ 'overlast',
      item %in% bereik ~ 'bereik'
    )
  ) |>

  filter(
    winkelgebied_naam != 'geen winkelstraat/gebied',
    winkelgebied_naam != 'overig'
  ) |>

  mutate(
    winkelgebied_naam_kort = str_split(
      winkelgebied_naam,
      ",",
      simplify = TRUE
    )[, 1]
  ) |>

  mutate(
    winkelgebied_naam_kort = case_when(
      winkelgebied_naam_kort ==
        'Waddenweg / Meeuwenlaan / gedempt Hamerkanaal' ~ 'Meeuwenlaan e.o.',
      winkelgebied_naam_kort ==
        'Bezaanjachtplein / Winkelcentrum in de Banne' ~ 'Bezaanjachtplein / In de Banne',
      winkelgebied_naam_kort ==
        'Zeilstraat / Hoofddorpplein/ Sloterkade' ~ 'Zeilstraat / Hoofddorpplein',
      winkelgebied_naam_kort ==
        'Ferdinand Bolstraat / Marie Heinekenplein' ~ 'F. Bolstraat / M. Heinekenplein',
      TRUE ~ winkelgebied_naam_kort
    )
  )

gem_thema <- gem_totaal |>
  group_by(
    winkelgebied_code,
    winkelgebied_naam,
    afzet_stadsdeel_code,
    productgroep,
    aantal,
    thema,
    winkelgebied_naam_kort
  ) |>
  summarise(gemiddelde = mean(gemiddelde, na.rm = T)) |>
  filter(!is.na(gemiddelde))


###############
### KAARTEN ---
###############

# inlezen theme en ggplot
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

wg_sd_kl <- wg_sd |>
  select(winkelgebied_code, winkelgebied_oisnaam, winkelgebied_oiscode) |>
  filter(!is.na(winkelgebied_code))


tabel_figuur <- gem_totaal |>
  filter(aantal > 19) |>

  pivot_wider(
    names_from = item,
    values_from = gemiddelde,
    values_fill = 0,
    id_cols = -thema
  ) |>
  filter(
    winkelgebied_naam != 'geen winkelstraat/gebied',
    winkelgebied_naam != 'overig'
  ) |>
  left_join(wg_sd_kl, by = "winkelgebied_code") |>
  relocate(
    winkelgebied_code,
    winkelgebied_naam,
    winkelgebied_oiscode,
    winkelgebied_oisnaam
  )

write.xlsx(
  tabel_figuur,
  "04 output tabellen/tab_mondet24_rapportcijfers24.xlsx",
  withFilter = T,
  overwrite = T
)

tabel_figuur <- gem_totaal |>
  filter(aantal > 19) |>

  filter(
    winkelgebied_naam != 'geen winkelstraat/gebied',
    winkelgebied_naam != 'overig'
  ) |>
  left_join(wg_sd_kl, by = "winkelgebied_code") |>
  relocate(
    winkelgebied_code,
    winkelgebied_naam,
    winkelgebied_oiscode,
    winkelgebied_oisnaam
  )

write.xlsx(
  tabel_figuur,
  "04 output tabellen/tabel_rapportcijfers24_Dg_NDg.xlsx",
  withFilter = T,
  overwrite = T
)


tabel_figuur_lng <- gem_totaal |>
  filter(
    aantal > 19,
    productgroep == 'winkelgebied voor dagelijkse boodschappen'
  ) |>

  filter(
    winkelgebied_naam != 'geen winkelstraat/gebied',
    winkelgebied_naam != 'overig'
  ) |>
  left_join(wg_sd_kl, by = "winkelgebied_code") |>
  relocate(
    winkelgebied_code,
    winkelgebied_naam,
    winkelgebied_oiscode,
    winkelgebied_oisnaam
  )

write.xlsx(
  tabel_figuur_lng,
  "04 output tabellen/tabel_rapportcijfers24_lng.xlsx",
  withFilter = T,
  overwrite = T
)


# figuur met rapportcijfers
gem_totaal |>
  filter(
    aantal > 49,
    productgroep == 'winkelgebied voor dagelijkse boodschappen',
    winkelgebied_naam != 'geen winkelstraat/gebied',
    winkelgebied_naam != 'overig',
    winkelgebied_naam_kort != 'Noord overig',
    !is.na(winkelgebied_code)
  ) |>

  ggplot(aes(
    x = fct_relevel(
      fct_rev(fct_reorder(item, gemiddelde)),
      c("totaaloordeel winkelgebied", "gemiddeld rapportcijfer"),
      after = Inf
    ),
    y = fct_relevel(
      fct_reorder(winkelgebied_naam_kort, gemiddelde),
      "Amsterdam totaal"
    ),
    fill = gemiddelde
  )) +
  geom_tile(color = "white", lwd = 0.9, linetype = 1) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
  theme_os2() +
  coord_fixed(0.6)
ggsave("04 output tabellen/fig4_rap_20_24.png", width = 7, height = 8)


# figuur met samenvatting thema's

gem_totaal |>
  filter(
    aantal > 49,
    productgroep == 'winkelgebied voor dagelijkse boodschappen',
    winkelgebied_naam != 'geen winkelstraat/gebied',
    winkelgebied_naam != 'overig',
    winkelgebied_naam_kort != 'Noord overig',
    !is.na(winkelgebied_code)
  ) |>

  ggplot(aes(
    y = fct_relevel(
      fct_reorder(thema, gemiddelde),
      c("totaal oordeel", "gemiddelde rapportcijfer"),
      after = Inf
    ),
    x = fct_rev(fct_relevel(
      fct_reorder(winkelgebied_naam_kort, gemiddelde),
      "Amsterdam totaal"
    )),
    fill = gemiddelde
  )) +
  geom_tile(color = "white", lwd = 1.1, linetype = 1) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
  theme_os2() +
  coord_fixed(0.6)
ggsave("04 output tabellen/fig8_rap_thema_24.png", width = 7, height = 4)


### tabel per stadsdeel ---

gem_totaal |>
  filter(
    productgroep == 'winkelgebied voor dagelijkse boodschappen',
    is.na(winkelgebied_code),
    afzet_stadsdeel_code != 'MRA',
    afzet_stadsdeel_code != 'overig NL'
  ) |>

  ggplot(aes(
    y = fct_relevel(
      fct_reorder(item, gemiddelde),
      c("totaaloordeel winkelgebied", "gemiddeld rapportcijfer")
    ),
    x = fct_rev(fct_reorder(winkelgebied_naam_kort, gemiddelde)),
    fill = gemiddelde
  )) +
  geom_tile(color = "white", lwd = 1.1, linetype = 1) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
  theme_os2() +
  coord_fixed(0.6)
ggsave("04 output tabellen/fig5_rap_20_24.png", width = 7, height = 5)


gem_totaal |>
  filter(
    aantal > 19,
    productgroep != 'winkelgebied voor dagelijkse boodschappen',
    winkelgebied_naam != 'geen winkelstraat/gebied',
    winkelgebied_naam != 'overig',
    winkelgebied_naam_kort != 'Noord overig',
    winkelgebied_naam_kort != 'Centrum overig'
  ) |>

  ggplot(aes(
    x = fct_relevel(
      fct_rev(fct_reorder(item, gemiddelde)),
      c("totaaloordeel winkelgebied", "gemiddeld rapportcijfer"),
      after = Inf
    ),
    y = fct_relevel(
      fct_reorder(winkelgebied_naam_kort, gemiddelde),
      "Amsterdam totaal"
    ),
    fill = gemiddelde
  )) +
  geom_tile(color = "white", lwd = 1.1, linetype = 1) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
  theme_os2() +
  coord_fixed(0.6)


cor_matrix |>
  pivot_longer(if_numeric)
ggplot(aes(
  x = fct_relevel(
    fct_rev(fct_reorder(item, gemiddelde)),
    c("totaaloordeel winkelgebied", "gemiddeld rapportcijfer"),
    after = Inf
  ),
  y = fct_relevel(
    fct_reorder(winkelgebied_naam_kort, gemiddelde),
    "Amsterdam totaal"
  ),
  fill = gemiddelde
)) +
  geom_tile(color = "white", lwd = 1.1, linetype = 1) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
  theme_os2() +
  coord_fixed(0.6)


ggsave("04 output tabellen/fig6_rap_20_24.png", width = 7, height = 5)
