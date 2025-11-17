library(tidyverse)
library(haven)

data_totaal <- read_rds("01 references/data_totaal_20_22_24_26.rds")

### inlezen rapportcijfervariabelen
rap <- read_rds("01 references/vragen_rapportcijfers.rds")

# toevoegen stadsdeel en gebied van winkelgebied
wg_kol_namen <- c(
  "wink_code",
  "wink_naam",
  "wink_stadsdeel_code",
  "wink_stadsdeel_naam",
  "wink_ggw_code",
  "wink_ggw_naam"
)

winkelgebieden <- openxlsx::read.xlsx(
  "01 references/winkelgebieden_20_22_24_26_def.xlsx",
  sheet = 1
) |>
  set_names(wg_kol_namen) |>
  mutate(wink_code = (str_trim(wink_code)))


# In 2024 zijn de rapportcijfers opnieuw vastgesteld,
# daardoor zijn de rapportcijfers van 2022 en 2024 niet helemaal vergelijkbaar -

list_rapportcijfers = list()

### rapportcijfers 2020 ---

# rapportcijfers voor winkelgebieden dagelijkse boodschappen
list_rapportcijfers[["rap_20_dg"]] <- data_totaal[["data_20_weeg"]] |>
  select(any_of(c("respondent_id", "weeg_ams_ink", "w2", rap[["20_22_v7"]]))) |>
  add_column(productgroep = "winkelgebied voor dagelijkse boodschappen") |>
  filter(!is.na(w2)) |>
  mutate(w2 = str_trim(as.character(w2))) |>
  left_join(winkelgebieden, by = c("w2" = "wink_code")) |>
  rename("wink_code" = "w2")

# rapportcijfers voor winkelgebieden om te winkelen
list_rapportcijfers[["rap_20_ndg"]] <- data_totaal[["data_20_weeg"]] |>
  select(any_of(c(
    "respondent_id",
    "weeg_ams_ink",
    "v20",
    rap[["20_22_v22"]]
  ))) |>
  add_column(productgroep = "winkelgebied voor niet-dagelijkse boodschappen") |>
  filter(!is.na(v20)) |>
  mutate(v20 = str_trim(as.character(v20))) |>
  left_join(winkelgebieden, by = c("v20" = "wink_code")) |>
  rename("wink_code" = "v20")


### rapportcijfers 2022 ---

# winkelgebieden dagelijks
list_rapportcijfers[["rap_22_dg"]] <- data_totaal[["data_22_weeg"]] |>
  select(any_of(c("respondent_id", "weeg_ams_ink", "v4", rap[["20_22_v7"]]))) |>
  add_column(productgroep = "winkelgebied voor dagelijkse boodschappen") |>
  filter(!is.na(v4)) |>
  mutate(v4 = str_trim(as.character(v4))) |>
  left_join(winkelgebieden, by = c("v4" = "wink_code")) |>
  rename("wink_code" = "v4")


# winkelgebieden dagelijks
list_rapportcijfers[["rap_22_ndg"]] <- data_totaal[["data_22_weeg"]] |>
  select(any_of(c(
    "respondent_id",
    "weeg_ams_ink",
    "v20",
    rap[["20_22_v22"]]
  ))) |>
  add_column(productgroep = "winkelgebied voor niet-dagelijkse boodschappen") |>
  filter(!is.na(v20)) |>
  mutate(v20 = str_trim(as.character(v20))) |>
  left_join(winkelgebieden, by = c("v20" = "wink_code")) |>
  rename("wink_code" = "v20")


### rapportcijfers 2024 ---

# winkelgebieden dagelijks
list_rapportcijfers[["rap_24_dg"]] <- data_totaal[["data_24_weeg"]] |>
  select(any_of(c(
    "respondent_id",
    "weeg_ams_ink",
    "v4_nw",
    rap[["24_26_v7"]]
  ))) |>
  add_column(productgroep = "winkelgebied voor dagelijkse boodschappen") |>
  filter(!is.na(v4_nw)) |>
  mutate(v4_nw = str_trim(as.character(v4_nw))) |>
  left_join(winkelgebieden, by = c("v4_nw" = "wink_code")) |>
  rename("wink_code" = "v4_nw")

# winkelgebieden om te winkelen
list_rapportcijfers[["rap_24_ndg"]] <- data_totaal[["data_24_weeg"]] |>
  select(any_of(c(
    "respondent_id",
    "weeg_ams_ink",
    "v20",
    rap[["24_26_v22"]]
  ))) |>
  add_column(productgroep = "winkelgebied voor niet-dagelijkse boodschappen") |>
  filter(!is.na(v20)) |>
  mutate(v20 = str_trim(as.character(v20))) |>
  left_join(winkelgebieden, by = c("v20" = "wink_code")) |>
  rename("wink_code" = "v20")


### rapportcijfers 2026 ---

# winkelgebieden dagelijks
list_rapportcijfers[["rap_26_dg"]] <- data_totaal[["data_26_weeg"]] |>
  select(any_of(c(
    "respondent_id",
    "weeg_ams_ink",
    "v4_nw",
    rap[["24_26_v7"]]
  ))) |>
  add_column(productgroep = "winkelgebied voor dagelijkse boodschappen") |>
  filter(!is.na(v4_nw)) |>
  mutate(v4_nw = str_trim(as.character(v4_nw))) |>
  left_join(winkelgebieden, by = c("v4_nw" = "wink_code")) |>
  rename("wink_code" = "v4_nw")


# winkelgebieden om te winkelen
list_rapportcijfers[["rap_26_ndg"]] <- data_totaal[["data_26_weeg"]] |>
  select(any_of(c(
    "respondent_id",
    "weeg_ams_ink",
    "v20",
    rap[["24_26_v22"]]
  ))) |>
  add_column(productgroep = "winkelgebied voor niet-dagelijkse boodschappen") |>
  filter(!is.na(v20)) |>
  mutate(v20 = str_trim(as.character(v20))) |>
  left_join(winkelgebieden, by = c("v20" = "wink_code")) |>
  rename("wink_code" = "v20")


# #chistiaan huygensplein en weesp aanpassen
# my_weesp <- function(x) {
#   x |>
#     mutate(
#       winkelgebied_code = case_when(
#         winkelgebied_code == '105' ~ '106',
#         winkelgebied_code == '208' ~ '312',
#         TRUE ~ winkelgebied_code
#       )
#     ) |>

#     mutate(
#       winkelgebied_naam = case_when(
#         winkelgebied_naam ==
#           "Christiaan Huygensplein, Watergraafsmeer, Oost" ~ "Christiaan Huygensplein, Helmholzstraat, Watergraafsmeer, Oost",
#         winkelgebied_naam ==
#           "Weesp" ~ 'Weesp, Centrum (oude binnenstad, Achtergracht, Nieuwstad, Oude gracht, Nieuwstraat)',
#         TRUE ~ winkelgebied_naam
#       )
#     )
# }

write_rds(list_rapportcijfers, "01 references/data_rapportcijfers.rds")
