###################################
# rapportcijfers per winkelgebied -
###################################
#install.packages("svglite")
library(svglite)
library(tidyverse)

#Aangepaste OS themes Rogier
source("02 scr/03 script ggplot functies.R")

# lijstje met vragen -
rap <- read_rds("01 references/vragen_rapportcijfers.rds")

df_rapcijfer_vragen <- bind_rows(
  tibble(
    name = c(rap[["20_22_v7"]], "v6_gem", "v21_gem"),
    label = c(
      rap[["20_22_v7_v22_label"]],
      "gemiddelde rapportcijfer dagelijks",
      "gemiddelde rapportcijfer niet-dagelijks"
    )
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
    pivot_longer(cols = any_of(c(rap[[vragen]], "v6_gem", "v21_gem"))) |>
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
  "wink_stadsdeel_naam",
  "winkelgebied_oisnaam",
  "winkelgebied_oiscode"
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
) |>
  add_column(monitor = 'monitor 2020')

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
) |>
  add_column(monitor = 'monitor 2022')

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
) |>
  add_column(monitor = 'monitor 2024')

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
) |>
  add_column(monitor = 'monitor 2026')


# verwijder de lage aantallen
gem_rap_filter <- gem_rap |>
  map(\(x) filter(x, aantal > 20)) |>
  map(\(x) filter(x, wink_code != "300")) |>
  map(\(x) filter(x, wink_code != "900")) |>
  map(\(x) filter(x, wink_code != "999")) |>
  map(\(x) filter(x, wink_code != "800")) |>
  map(\(x) filter(x, wink_stadsdeel_code != "onbekend")) |>
  map(\(x) filter(x, wink_stadsdeel_code != "online")) |>
  map(\(x) filter(x, !str_detect(wink_naam, "overig"))) |>
  map_df(\(x) select(x, monitor, name, label, everything())) |>
  mutate(
    winkelgebied_oisnaam = case_when(
      is.na(winkelgebied_oisnaam) ~ wink_naam,
      TRUE ~ winkelgebied_oisnaam
    )
  )

openxlsx::write.xlsx(
  gem_rap_filter,
  "04 reports/03 tabellen/rapportcijfers_20_tm_26.xlsx",
  overwrite = T,
  withFilter = T
)

### nette publicatietabel ---

# gemiddelde
tabel_netjes_dg <- bind_rows(
  gem_rap_filter |>
    ungroup() |>
    filter(productgroep == 'winkelgebied voor dagelijkse boodschappen') |>
    select(
      monitor,
      label,
      productgroep,
      wink_code,
      wink_naam,
      wink_stadsdeel_code,
      wink_stadsdeel_naam,
      gem_gewogen
    ) |>
    pivot_wider(
      names_from = label,
      values_from = gem_gewogen
    ) |>
    add_column(item = 'rapportcijfer'),

  gem_rap_filter |>
    ungroup() |>
    filter(productgroep == 'winkelgebied voor dagelijkse boodschappen') |>
    select(
      monitor,
      label,
      productgroep,
      wink_code,
      wink_naam,
      wink_stadsdeel_code,
      wink_stadsdeel_naam,
      aantal
    ) |>
    pivot_wider(
      names_from = label,
      values_from = aantal
    ) |>
    add_column(item = 'aantal')
)


tabel_netjes_ndg <- bind_rows(
  gem_rap_filter |>
    ungroup() |>
    filter(productgroep == 'winkelgebied om te winkelen') |>
    select(
      monitor,
      label,
      productgroep,
      wink_code,
      wink_naam,
      wink_stadsdeel_code,
      wink_stadsdeel_naam,
      gem_gewogen
    ) |>
    pivot_wider(
      names_from = label,
      values_from = gem_gewogen
    ) |>
    add_column(item = 'rapportcijfer'),

  gem_rap_filter |>
    ungroup() |>
    filter(productgroep == 'winkelgebied om te winkelen') |>
    select(
      monitor,
      label,
      productgroep,
      wink_code,
      wink_naam,
      wink_stadsdeel_code,
      wink_stadsdeel_naam,
      aantal
    ) |>
    pivot_wider(
      names_from = label,
      values_from = aantal
    ) |>
    add_column(item = 'aantal')
)


openxlsx::write.xlsx(
  list(
    rap_dg = tabel_netjes_dg,
    rap_ndg = tabel_netjes_ndg
  ),
  "04 reports/03 tabellen/rapportcijfers_20_tm_26_netjes.xlsx",
  overwrite = T,
  withFilter = T
)

#########################
#        KAARTEN        #
#########################

# url met winkelgebieden
wg_url <- 'https://onderzoek.amsterdam.nl/static/datavisualisatie-onderzoek-en-statistiek/geo/winkelgebieden/2024/winkelgebieden-2024-geo.json'

# url met stadsdelen
sd_url <- 'https://onderzoek.amsterdam.nl/static/datavisualisatie-onderzoek-en-statistiek/geo/amsterdam/2022/stadsdelen-2022-zw-geo.json'


# inlezen geojson van de winkelgebieden en omzetten naar spatial feature
winkelgebieden_geo <- sf::st_read(wg_url) |>
  rename(winkelgebied_oiscode = code)

# inlezen geojson van de stadsdelen en omzetten naar spatial feature
stadsdelen_geo <- sf::st_read(sd_url) |>
  rename(
    gbd_sdl_naam = naam,
    gbd_sdl_code = code
  )

# Data voor winkelgebiedkaart voor alle jaartallen
kaart_winkelgeb_rap <- winkelgebieden_geo |>
  left_join(gem_rap_filter, by = "winkelgebied_oiscode")

#Kaart algemeen winkelgebieden dagelijks en niet dagelijks
kaart_winkelgeb_rap_alg <- kaart_winkelgeb_rap |>
  filter(name %in% c('v6', "v21")) |>
  # data staat niet meer in list maar in df, dus extra filter toevoegen
  filter(monitor == 'monitor 2026') |>
  mutate(
    name = recode(
      name,
      "v6" = "totaaloordeel dagelijks",
      "v21" = "totaaloordeel niet dagelijks"
    )
  )
fig_wg <- ggplot() +
  geom_sf(
    data = stadsdelen_geo,
    color = 'white',
    fill = "#cfcfcf",
    linewidth = 0.6
  ) +
  geom_sf(
    data = kaart_winkelgeb_rap_alg,
    aes(fill = gem_gewogen, geometry = geometry),
    color = NA,
    linewidth = 0.7
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradient(name = NULL, low = "#f2dae7", high = "#a00078") +
  theme_clk(legenda_pos = "right") +
  guides(
    fill = guide_colourbar(reverse = FALSE)
  ) +
  facet_wrap(~name, ncol = 1)

#ggsave(
#  ""04 reports/04 figuren/>naam<.svg",
#  width = 12,
#  height = 6
#)

# Data alle jaren voor kaart stadsdelen algemeeen
kaart_sd_rap <- stadsdelen_geo |>
  left_join(
    gem_rap_filter,
    by = c("gbd_sdl_code" = "wink_stadsdeel_code")
  )

#Kaart algemeen stadsdelen dagelijks en niet dagelijks
kaart_sd_rap_alg <- kaart_sd_rap |>
  filter(name %in% c('v6', "v21")) |>
  # data staat niet meer in list maar in df, dus extra filter toevoegen
  filter(monitor == 'monitor 2026') |>
  mutate(
    name = recode(
      name,
      "v6" = "totaaloordeel dagelijks",
      "v21" = "totaaloordeel niet dagelijks"
    )
  )

fig_sd <- ggplot() +
  geom_sf(
    data = stadsdelen_geo,
    color = 'white',
    fill = "#cfcfcf",
    linewidth = 0.6
  ) +
  geom_sf(
    data = kaart_sd_rap_alg,
    aes(fill = gem_gewogen, geometry = geometry),
    color = NA,
    linewidth = 0.7
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradient(name = NULL, low = "#f2dae7", high = "#a00078") +
  theme_clk(legenda_pos = "right") +
  guides(
    fill = guide_colourbar(reverse = TRUE)
  ) +
  facet_wrap(~name, ncol = 1)

# #Samenvoegen kaarten sd en wg
# library(patchwork)
#
# fig_patch <- fig_sd/fig_wg

#kaart Veiligheid stadsdelen overdag dagelijks & niet dagelijks
kaart_sd_rap_veiligoverdag <- kaart_sd_rap |>
  filter(name %in% c('v7_nw_veiligheid_gv1', "v22_nw_veiligheid_gv1")) |>
  mutate(
    name = recode(
      name,
      "v7_nw_veiligheid_gv1" = "veiligheid overdag dagelijks",
      "v22_nw_veiligheid_gv1" = "veiligheid overdag niet dagelijks"
    )
  )
fig_sd_veiligoverdag <- ggplot() +
  geom_sf(
    data = stadsdelen_geo,
    color = 'white',
    fill = "#cfcfcf",
    linewidth = 0.6
  ) +
  geom_sf(
    data = kaart_sd_rap_veiligoverdag,
    aes(fill = gem_gewogen, geometry = geometry),
    color = NA,
    linewidth = 0.7
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradient(name = NULL, low = "#f2dae7", high = "#a00078") +
  theme_clk(legenda_pos = "right") +
  guides(
    fill = guide_colourbar(reverse = TRUE)
  ) +
  facet_wrap(~name, ncol = 1)


#kaart veiligheid stadsdelen avond dagelijks & niet dagelijks
kaart_sd_rap_veiligavond <- kaart_sd_rap |>
  filter(
    name %in% c('v7_nw_veiligheid_avond_gv1', "v22_nw_veiligheid_avond_gv1")
  ) |>
  mutate(
    name = recode(
      name,
      "v7_nw_veiligheid_avond_gv1" = "veiligheid avond dagelijks",
      "v22_nw_veiligheid_avond_gv1" = "veiligheid avond niet dagelijks"
    )
  )
fig_sd_veiligavond <- ggplot() +
  geom_sf(
    data = stadsdelen_geo,
    color = 'white',
    fill = "#cfcfcf",
    linewidth = 0.6
  ) +
  geom_sf(
    data = kaart_sd_rap_veiligavond,
    aes(fill = gem_gewogen, geometry = geometry),
    color = NA,
    linewidth = 0.7
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradient(name = NULL, low = "#f2dae7", high = "#a00078") +
  theme_clk(legenda_pos = "right") +
  guides(
    fill = guide_colourbar(reverse = TRUE)
  ) +
  facet_wrap(~name, ncol = 1)


######################
#   GG TILES         #
######################

hcl <- farver::decode_colour(hcl.colors(20, "RdYlgn"), "rgb", "hcl")

label_col <- ifelse(hcl[, "l"] > 50, "black", "white")


# # figuur met totaaloordeel winkelgebieden
gem_rap_filter |>
  mutate(monitor = str_replace_all(monitor, "monitor ", "")) |>
  filter(
    wink_code != 32,
    winkelgebied_oisnaam != 'Oost Overig, Oost'
  ) |>

  mutate(
    winkelgebied_oisnaam = str_replace_all(
      winkelgebied_oisnaam,
      "stadsdelen",
      "gemiddelde stadsdeel"
    )
  ) |>
  # mutate(
  #   wink_stadsdeel_naam = case_when(
  #     wink_stadsdeel_naam == 'Weesp'    ~ 'Zuidoost en Weesp',
  #     wink_stadsdeel_naam == 'Zuidoost' ~ 'Zuidoost en Weesp',
  #     TRUE ~ wink_stadsdeel_naam))|>
  filter(
    # !is.na(winkelgebied_oisnaam),
    productgroep == 'winkelgebied voor dagelijkse boodschappen',
    label == 'totaaloordeel winkelgebied?',
    wink_stadsdeel_naam != 'Amsterdam',
    wink_stadsdeel_naam != 'MRA',
    wink_stadsdeel_naam != 'Buiten MRA',
  ) |>

  ggplot(aes(
    x = monitor,
    y = fct_relevel(
      fct_reorder(winkelgebied_oisnaam, gem_gewogen),
      'gemiddelde stadsdeel'
    ),
    fill = gem_gewogen
  )) +
  geom_tile(color = "white", lwd = 0.9, linetype = 1) +
  geom_text(
    aes(
      label = round(gem_gewogen, 1),
      color = gem_gewogen
    ),
    family = font
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
  scale_color_gradientn(name = NULL, colors = label_col) +
  theme_os(legend_position = 'right') +
  #coord_fixed(0.6)+
  facet_wrap(~wink_stadsdeel_naam, scales = 'free_y', ncol = 2) +
  guides(color = 'none')

ggsave(
  "04 reports/04 figuren/tile_rapportcifer_dagelijks.svg",
  width = 12,
  height = 10
)


# # figuur met totaaloordeel winkelgebieden
gem_rap_filter |>
  mutate(monitor = str_replace_all(monitor, "monitor ", "")) |>
  filter(
    wink_naam != 'stadsdelen',
    productgroep == 'winkelgebied om te winkelen',
    label == 'totaaloordeel winkelgebied?'
  ) |>
  mutate(
    wink_naam = case_when(
      wink_code == '312' ~ 'Weesp, Centrum',
      TRUE ~ wink_naam
    )
  ) |>
  ggplot(aes(
    x = monitor,
    y = fct_reorder(winkelgebied_oisnaam, gem_gewogen),
    fill = gem_gewogen
  )) +
  geom_tile(color = "white", lwd = 0.9, linetype = 1) +
  geom_text(
    aes(
      label = round(gem_gewogen, 1),
      color = gem_gewogen
    ),
    family = font
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
  scale_color_gradientn(name = NULL, colors = label_col) +
  theme_os(legend_position = 'right') +
  #coord_fixed(0.6)+
  # facet_wrap(~wink_stadsdeel_naam, scales = 'free_y', ncol =2)+
  guides(color = 'none')

ggsave(
  "04 reports/04 figuren/tile_rapportcifer_niet-dagelijks.svg",
  width = 12,
  height = 8
)

######HIER SPECIFICEREN ######
#Naar kort winkel naam en bvb thema veiligheid

# ###############
# # categorieën -
# ###############

# totaal <- c(
#   "totaaloordeel winkelgebied"
# )

# gemiddelde <- c(
#   "gemiddeld rapportcijfer"
# )

# sfeer <- c(
#   "uiterlijk van de winkels",
#   "aankleding en inrichting",
#   "sfeer en de gezelligheid"
# )

# aanbod <- c(
#   "aanbod van daghoreca",
#   "het algemeen prijsniveau",
#   "het diverse aanbod van food-winkels",
#   "het diverse aanbod van non-food-winkels"
# )

# duurzaam_bio <- c(
#   "het biologische/duurzame aanbod van food-winkels",
#   "het duurzame aanbod van non-food winkels"
# )

# overlast <- c(
#   "het schoonhouden van de straten",
#   "overlast door horeca",
#   "overlast van andere mensen",
#   "veiligheid winkelomgeving overdag",
#   "veiligheid winkelomgeving ‘s avonds",
#   "overlast door vervuiling"
# )

# bereik <- c(
#   "parkeermogelijkheden voor auto",
#   "parkeermogelijkheden voor fiets",
#   "algemene bereikbaarheid"
# )

# # samenvoegen -
# gem_totaal <- bind_rows(gem_rap, gem_rap_ams, gem_rap_sd) |>

#   mutate(
#     item = str_trim(item, "both")
#   ) |>

#   mutate(
#     item = case_when(
#       item ==
#         "Wat is uw totaaloordeel over dit winkelgebied?" ~ "totaaloordeel winkelgebied",
#       item ==
#         "het biologische/duurzame aanbod van food-winkels" ~ "het biologische/duurzame aanbod food",
#       item ==
#         "het schoonhouden van de straten/ stoepen/ passage" ~ "schoonhouden van de straten",
#       item ==
#         "de sfeer en de gezelligheid van het winkelgebied" ~ "sfeer en de gezelligheid",
#       item ==
#         "de aankleding en inrichting van het winkelgebied (faciliteiten, verlichting, bankjes)" ~ "aankleding en inrichting",
#       item ==
#         "het uiterlijk van de winkels (denk aan gevels, etalages, inrichting)" ~ "uiterlijk van de winkels",
#       TRUE ~ item
#     )
#   ) |>

#   mutate(
#     thema = case_when(
#       item %in% sfeer ~ 'sfeer en gezelligheid',
#       item %in% totaal ~ 'totaal oordeel',
#       item == "gemiddeld rapportcijfer" ~ 'gemiddelde rapportcijfer',
#       item %in% duurzaam_bio ~ 'duurz. en biol. producten',
#       item %in% aanbod ~ 'aanbod producten',
#       item %in% overlast ~ 'overlast',
#       item %in% bereik ~ 'bereik'
#     )
#   ) |>

#   mutate(
#     winkelgebied_naam_kort = str_split(
#       winkelgebied_naam,
#       ",",
#       simplify = TRUE
#     )[, 1]
#   ) |>

#   mutate(
#     winkelgebied_naam_kort = case_when(
#       winkelgebied_naam_kort ==
#         'Waddenweg / Meeuwenlaan / gedempt Hamerkanaal' ~ 'Meeuwenlaan e.o.',
#       winkelgebied_naam_kort ==
#         'Bezaanjachtplein / Winkelcentrum in de Banne' ~ 'Bezaanjachtplein / In de Banne',
#       winkelgebied_naam_kort ==
#         'Zeilstraat / Hoofddorpplein/ Sloterkade' ~ 'Zeilstraat / Hoofddorpplein',
#       winkelgebied_naam_kort ==
#         'Ferdinand Bolstraat / Marie Heinekenplein' ~ 'F. Bolstraat / M. Heinekenplein',
#       TRUE ~ winkelgebied_naam_kort
#     )
#   )

# gem_thema <- gem_totaal |>
#   group_by(
#     winkelgebied_code,
#     winkelgebied_naam,
#     afzet_stadsdeel_code,
#     productgroep,
#     aantal,
#     thema,
#     winkelgebied_naam_kort
#   ) |>
#   summarise(gemiddelde = mean(gemiddelde, na.rm = T)) |>
#   filter(!is.na(gemiddelde))

# ###############
# ### KAARTEN ---
# ###############

# # inlezen theme en ggplot
# source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

# wg_sd_kl <- wg_sd |>
#   select(winkelgebied_code, winkelgebied_oisnaam, winkelgebied_oiscode) |>
#   filter(!is.na(winkelgebied_code))

# tabel_figuur <- gem_totaal |>
#   filter(aantal > 19) |>

#   pivot_wider(
#     names_from = item,
#     values_from = gemiddelde,
#     values_fill = 0,
#     id_cols = -thema
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
#   tabel_figuur,
#   "04 output tabellen/tab_mondet24_rapportcijfers24.xlsx",
#   withFilter = T,
#   overwrite = T
# )

# tabel_figuur <- gem_totaal |>
#   filter(aantal > 19) |>

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
