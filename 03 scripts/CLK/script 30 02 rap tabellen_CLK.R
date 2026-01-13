###################################
# rapportcijfers per winkelgebied -
###################################

library(tidyverse)

# lijstje met vragen -
rap <- read_rds("01 references/vragen_rapportcijfers.rds")

df_rapcijfer_vragen <- bind_rows(
  tibble(
    name = rap[["20_22_v7"]],
    label = rap[["20_22_v7_v22_label"]]
  ),

  tibble(
    name = rap[["24_26_v7"]],
    label = rap[["24_26_v7_v22_label"]]
  ),

  tibble(
    name = rap[["20_22_v22"]],
    label = rap[["20_22_v7_v22_label"]]
  ),

  tibble(
    name = rap[["24_26_v22"]],
    label = rap[["24_26_v7_v22_label"]]
  )
) |>
  distinct(name, .keep_all = T)

# data -
list_rapportcijfers <- read_rds("01 references/data_rapportcijfers.rds")


my_rapport_cijfers <- function(data, group_vars, vragen) {
  list_rapportcijfers[[data]] |>
    mutate(weeg_ams_ink = replace_na(weeg_ams_ink, 1)) |>
    pivot_longer(cols = any_of(rap[[vragen]])) |>
    group_by(
      name,
      productgroep,
      across(all_of(group_vars))
    ) |>
    summarise(
      aantal = n(),
      gemiddelde = round(mean(value, na.rm = T), 2),
      gem_gewogen = round(weighted.mean(value, weeg_ams_ink, na.rm = T), 2)
    ) |>
    left_join(df_rapcijfer_vragen, by = 'name')
}


gem_rap <- list()

### dit zijn de grouping_vars om tabellen te maken

# group by winkelgebied
vars_wk = c(
  "wink_code",
  "wink_naam",
  "wink_stadsdeel_code",
  "wink_stadsdeel_naam"
)
# group by stadsdeel
vars_sd = c(
  "wink_stadsdeel_code",
  "wink_stadsdeel_naam"
)
# group by Amsterdam
vars_ams = NULL


# data 2020
gem_rap[["rap_20"]] <- bind_rows(
  # gem vd winkelgebieden
  my_rapport_cijfers(
    data = "rap_20_dg",
    group_vars = vars_wk,
    vragen = "20_22_v7"
  ),

  my_rapport_cijfers(
    data = "rap_20_ndg",
    group_vars = vars_wk,
    vragen = "20_22_v22"
  ),

  # gem vd stadsdelen
  my_rapport_cijfers(
    data = "rap_20_dg",
    group_vars = vars_sd,
    vragen = "20_22_v7"
  ) |>
    add_column(
      wink_code = "stadsdelen",
      wink_naam = "stadsdelen"
    ),
  my_rapport_cijfers(
    data = "rap_20_ndg",
    group_vars = vars_sd,
    vragen = "20_22_v22"
  ) |>
    add_column(
      wink_code = "stadsdelen",
      wink_naam = "stadsdelen"
    ),

  # gem vd ams
  my_rapport_cijfers(
    data = "rap_20_dg",
    group_vars = vars_ams,
    vragen = "20_22_v7"
  ) |>
    add_column(
      wink_code = "Amsterdam",
      wink_naam = "Amsterdam",
      wink_stadsdeel_code = "Amsterdam",
      wink_stadsdeel_naam = "Amsterdam"
    ),
  my_rapport_cijfers(
    data = "rap_20_ndg",
    group_vars = vars_ams,
    vragen = "20_22_v22"
  ) |>
    add_column(
      wink_code = "Amsterdam",
      wink_naam = "Amsterdam",
      wink_stadsdeel_code = "Amsterdam",
      wink_stadsdeel_naam = "Amsterdam"
    )
)

### data 2022 ---

gem_rap[["rap_22"]] <- bind_rows(
  # gem winkelgebieden
  my_rapport_cijfers(
    data = "rap_22_dg",
    group_vars = vars_wk,
    vragen = "20_22_v7"
  ),
  my_rapport_cijfers(
    data = "rap_22_ndg",
    group_vars = vars_wk,
    vragen = "20_22_v22"
  ),

  # gem stadsdelen
  my_rapport_cijfers(
    data = "rap_22_dg",
    group_vars = vars_sd,
    vragen = "20_22_v7"
  ) |>
    add_column(
      wink_code = "stadsdelen",
      wink_naam = "stadsdelen"
    ),

  my_rapport_cijfers(
    data = "rap_22_ndg",
    group_vars = vars_sd,
    vragen = "20_22_v22"
  ) |>
    add_column(
      wink_code = "stadsdelen",
      wink_naam = "stadsdelen"
    ),

  # gem amsterdam
  my_rapport_cijfers(
    data = "rap_22_dg",
    group_vars = vars_ams,
    vragen = "20_22_v7"
  ) |>
    add_column(
      wink_code = "Amsterdam",
      wink_naam = "Amsterdam",
      wink_stadsdeel_code = "Amsterdam",
      wink_stadsdeel_naam = "Amsterdam"
    ),

  my_rapport_cijfers(
    data = "rap_22_ndg",
    group_vars = vars_ams,
    vragen = "20_22_v22"
  ) |>
    add_column(
      wink_code = "Amsterdam",
      wink_naam = "Amsterdam",
      wink_stadsdeel_code = "Amsterdam",
      wink_stadsdeel_naam = "Amsterdam"
    )
)

### data 2024 ---

gem_rap[["rap_24"]] <- bind_rows(
  # gem winkelgebieden
  my_rapport_cijfers(
    data = "rap_24_dg",
    group_vars = vars_wk,
    vragen = "24_26_v7"
  ),
  my_rapport_cijfers(
    data = "rap_24_ndg",
    group_vars = vars_wk,
    vragen = "24_26_v22"
  ),

  # gem stadsdeel
  my_rapport_cijfers(
    data = "rap_24_dg",
    group_vars = vars_sd,
    vragen = "24_26_v7"
  ) |>
    add_column(
      wink_code = "stadsdelen",
      wink_naam = "stadsdelen"
    ),

  my_rapport_cijfers(
    data = "rap_24_ndg",
    group_vars = vars_sd,
    vragen = "24_26_v22"
  ) |>
    add_column(
      wink_code = "stadsdelen",
      wink_naam = "stadsdelen"
    ),

  # gem ams
  my_rapport_cijfers(
    data = "rap_24_dg",
    group_vars = vars_ams,
    vragen = "24_26_v7"
  ) |>
    add_column(
      wink_code = "Amsterdam",
      wink_naam = "Amsterdam",
      wink_stadsdeel_code = "Amsterdam",
      wink_stadsdeel_naam = "Amsterdam"
    ),
  my_rapport_cijfers(
    data = "rap_24_ndg",
    group_vars = vars_ams,
    vragen = "24_26_v22"
  ) |>
    add_column(
      wink_code = "Amsterdam",
      wink_naam = "Amsterdam",
      wink_stadsdeel_code = "Amsterdam",
      wink_stadsdeel_naam = "Amsterdam"
    )
)

### data 2026 ---

gem_rap[["rap_26"]] <- bind_rows(
  # gem winkelgebieden
  my_rapport_cijfers(
    data = "rap_26_dg",
    group_vars = vars_wk,
    vragen = "24_26_v7"
  ),
  my_rapport_cijfers(
    data = "rap_26_ndg",
    group_vars = vars_wk,
    vragen = "24_26_v22"
  ),

  # gem stadsdelen
  my_rapport_cijfers(
    data = "rap_26_dg",
    group_vars = vars_sd,
    vragen = "24_26_v7"
  ) |>
    add_column(
      wink_code = "stadsdelen",
      wink_naam = "stadsdelen"
    ),
  my_rapport_cijfers(
    data = "rap_26_ndg",
    group_vars = vars_sd,
    vragen = "24_26_v22"
  ) |>
    add_column(
      wink_code = "stadsdelen",
      wink_naam = "stadsdelen"
    ),

  # gem amsterdam
  my_rapport_cijfers(
    data = "rap_26_dg",
    group_vars = vars_ams,
    vragen = "24_26_v7"
  ) |>
    add_column(
      wink_code = "Amsterdam",
      wink_naam = "Amsterdam",
      wink_stadsdeel_code = "Amsterdam",
      wink_stadsdeel_naam = "Amsterdam"
    ),
  my_rapport_cijfers(
    data = "rap_26_ndg",
    group_vars = vars_ams,
    vragen = "24_26_v22"
  ) |>
    add_column(
      wink_code = "Amsterdam",
      wink_naam = "Amsterdam",
      wink_stadsdeel_code = "Amsterdam",
      wink_stadsdeel_naam = "Amsterdam"
    )
)


# verwijder de lage aantallen
gem_rap_filter <- gem_rap |>
  map(\(x) filter(x, aantal > 20)) |>
  map(\(x) filter(x, wink_code != "300")) |>
  map(\(x) filter(x, wink_code != "900")) |>
  map(\(x) filter(x, wink_code != "999")) |>
  map(\(x) filter(x, wink_code != "800")) |>
  map(\(x) filter(x, !str_detect(wink_naam, "overig"))) |>
  map(\(x) select(x, name, label, everything()))

openxlsx::write.xlsx(
  gem_rap_filter,
  "04 reports/03 tabellen/rapportcijfers_22_tm_26.xlsx",
  overwrite = T,
  withFilter = T
)

### nette publicatietabel ---

tabel_netjes <- gem_rap_filter |>
  map(\(x) ungroup(x)) |>
  map(\(x) {
    select(
      x,
      label,
      productgroep,
      wink_code,
      wink_naam,
      wink_stadsdeel_code,
      wink_stadsdeel_naam,
      gem_gewogen
    )
  }) |>
  map(\(x) pivot_wider(x, names_from = label, values_from = gem_gewogen))

openxlsx::write.xlsx(
  tabel_netjes,
  "04 reports/03 tabellen/rapportcijfers_22_tm_26_netjes.xlsx",
  overwrite = T,
  withFilter = T
)

wg_sd <- openxlsx::read.xlsx("01 references/winkelgebieden_20_22_24_26_def.xlsx")


###########################
#Gemiddelde Rapportcijfers#

list_rapportcijfers[["rap_26_dg"]]|>
  mutate(gem_rap= mean (V7)




# ###############
# # categorieën -
# ###############

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

# ###############
# ### KAARTEN ---
# ###############

# # inlezen theme en ggplot
# source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

wg_sd_kl <- wg_sd |>
   select(w1, winkelgebied_oisnaam, winkelgebied_oiscode) |>
   filter(!is.na(w1))

 tabel_figuur <- gem_totaal |>
   filter(aantal > 19) |>

   pivot_wider(
     names_from = item,
     values_from = gemiddelde,
     values_fill = 0,
     id_cols = -thema
   ) |>
   filter(
     w1_label != 'geen winkelstraat/gebied',
     w1_label != 'overig'
   ) |>
   left_join(wg_sd_kl, by = "w1") |>
   relocate(
     w1,
     w1_label,
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

#   filter(
#     winkelgebied_naam != 'geen winkelstraat/gebied',
#     winkelgebied_naam != 'overig'
#   ) |>
#   left_join(wg_sd_kl, by = "winkelgebied_code") |>
#   relocate(
#     winkelgebied_code,
#     winkelgebied_naam,
#     winkelgebied_oiscode,
#     winkelgebied_oisnaam
#   )

# write.xlsx(
#   tabel_figuur,
#   "04 output tabellen/tabel_rapportcijfers24_Dg_NDg.xlsx",
#   withFilter = T,
#   overwrite = T
# )

# tabel_figuur_lng <- gem_totaal |>
#   filter(
#     aantal > 19,
#     productgroep == 'winkelgebied voor dagelijkse boodschappen'
#   ) |>

#   filter(
#     winkelgebied_naam != 'geen winkelstraat/gebied',
#     winkelgebied_naam != 'overig'
#   ) |>
#   left_join(wg_sd_kl, by = "winkelgebied_code") |>
#   relocate(
#     winkelgebied_code,
#     winkelgebied_naam,
#     winkelgebied_oiscode,
#     winkelgebied_oisnaam
#   )

# write.xlsx(
#   tabel_figuur_lng,
#   "04 output tabellen/tabel_rapportcijfers24_lng.xlsx",
#   withFilter = T,
#   overwrite = T
# )

# # figuur met rapportcijfers
# gem_totaal |>
#   filter(
#     aantal > 49,
#     productgroep == 'winkelgebied voor dagelijkse boodschappen',
#     winkelgebied_naam != 'geen winkelstraat/gebied',
#     winkelgebied_naam != 'overig',
#     winkelgebied_naam_kort != 'Noord overig',
#     !is.na(winkelgebied_code)
#   ) |>

#   ggplot(aes(
#     x = fct_relevel(
#       fct_rev(fct_reorder(item, gemiddelde)),
#       c("totaaloordeel winkelgebied", "gemiddeld rapportcijfer"),
#       after = Inf
#     ),
#     y = fct_relevel(
#       fct_reorder(winkelgebied_naam_kort, gemiddelde),
#       "Amsterdam totaal"
#     ),
#     fill = gemiddelde
#   )) +
#   geom_tile(color = "white", lwd = 0.9, linetype = 1) +
#   labs(title = NULL, x = NULL, y = NULL) +
#   scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
#   theme_os2() +
#   coord_fixed(0.6)
# ggsave("04 output tabellen/fig4_rap_20_24.png", width = 7, height = 8)

# # figuur met samenvatting thema's

# gem_totaal |>
#   filter(
#     aantal > 49,
#     productgroep == 'winkelgebied voor dagelijkse boodschappen',
#     winkelgebied_naam != 'geen winkelstraat/gebied',
#     winkelgebied_naam != 'overig',
#     winkelgebied_naam_kort != 'Noord overig',
#     !is.na(winkelgebied_code)
#   ) |>

#   ggplot(aes(
#     y = fct_relevel(
#       fct_reorder(thema, gemiddelde),
#       c("totaal oordeel", "gemiddelde rapportcijfer"),
#       after = Inf
#     ),
#     x = fct_rev(fct_relevel(
#       fct_reorder(winkelgebied_naam_kort, gemiddelde),
#       "Amsterdam totaal"
#     )),
#     fill = gemiddelde
#   )) +
#   geom_tile(color = "white", lwd = 1.1, linetype = 1) +
#   labs(title = NULL, x = NULL, y = NULL) +
#   scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
#   theme_os2() +
#   coord_fixed(0.6)
# ggsave("04 output tabellen/fig8_rap_thema_24.png", width = 7, height = 4)

# ### tabel per stadsdeel ---

# gem_totaal |>
#   filter(
#     productgroep == 'winkelgebied voor dagelijkse boodschappen',
#     is.na(winkelgebied_code),
#     afzet_stadsdeel_code != 'MRA',
#     afzet_stadsdeel_code != 'overig NL'
#   ) |>

#   ggplot(aes(
#     y = fct_relevel(
#       fct_reorder(item, gemiddelde),
#       c("totaaloordeel winkelgebied", "gemiddeld rapportcijfer")
#     ),
#     x = fct_rev(fct_reorder(winkelgebied_naam_kort, gemiddelde)),
#     fill = gemiddelde
#   )) +
#   geom_tile(color = "white", lwd = 1.1, linetype = 1) +
#   labs(title = NULL, x = NULL, y = NULL) +
#   scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
#   theme_os2() +
#   coord_fixed(0.6)
# ggsave("04 output tabellen/fig5_rap_20_24.png", width = 7, height = 5)

# gem_totaal |>
#   filter(
#     aantal > 19,
#     productgroep != 'winkelgebied voor dagelijkse boodschappen',
#     winkelgebied_naam != 'geen winkelstraat/gebied',
#     winkelgebied_naam != 'overig',
#     winkelgebied_naam_kort != 'Noord overig',
#     winkelgebied_naam_kort != 'Centrum overig'
#   ) |>

#   ggplot(aes(
#     x = fct_relevel(
#       fct_rev(fct_reorder(item, gemiddelde)),
#       c("totaaloordeel winkelgebied", "gemiddeld rapportcijfer"),
#       after = Inf
#     ),
#     y = fct_relevel(
#       fct_reorder(winkelgebied_naam_kort, gemiddelde),
#       "Amsterdam totaal"
#     ),
#     fill = gemiddelde
#   )) +
#   geom_tile(color = "white", lwd = 1.1, linetype = 1) +
#   labs(title = NULL, x = NULL, y = NULL) +
#   scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
#   theme_os2() +
#   coord_fixed(0.6)

# cor_matrix |>
#   pivot_longer(if_numeric)
# ggplot(aes(
#   x = fct_relevel(
#     fct_rev(fct_reorder(item, gemiddelde)),
#     c("totaaloordeel winkelgebied", "gemiddeld rapportcijfer"),
#     after = Inf
#   ),
#   y = fct_relevel(
#     fct_reorder(winkelgebied_naam_kort, gemiddelde),
#     "Amsterdam totaal"
#   ),
#   fill = gemiddelde
# )) +
#   geom_tile(color = "white", lwd = 1.1, linetype = 1) +
#   labs(title = NULL, x = NULL, y = NULL) +
#   scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
#   theme_os2() +
#   coord_fixed(0.6)

# ggsave("04 output tabellen/fig6_rap_20_24.png", width = 7, height = 5)
