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
  "wink_ggw_naam",
  "winkelgebied_oisnaam",
  "winkelgebied_oiscode"
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

my_rapport_list <- function(dataset, rapvars, winkelvar, prodgroep) {
  basis <- data_totaal[[dataset]] |>
    rename(wink_code = {{ winkelvar }}) |>
    select(any_of(c(
      "respondent_id",
      "weeg_ams_ink",
      "wink_code",
      rap[[rapvars]]
    ))) |>
    add_column(productgroep = prodgroep) |>
    filter(!is.na(wink_code)) |>
    mutate(wink_code = str_trim(as.character(wink_code))) |>
    left_join(winkelgebieden, by = "wink_code")

  # verwijder het algemene rapportcijfer
  rap_selectie_items <- rap[[rapvars]][-1]
  rap_selectie_een <- rap[[rapvars]][1]

  # berekening gemiddelde rapportcijfers van items
  gem <- basis |>
    select(respondent_id, wink_code, all_of(rap_selectie_items)) |>
    pivot_longer(cols = all_of(rap_selectie_items)) |>
    group_by(respondent_id, wink_code) |>
    summarise("{rap_selectie_een}_gem" := mean(value, na.rm = T))

  output <- basis |>
    left_join(gem, by = c("respondent_id", "wink_code"))

  print(output)
}

list_rapportcijfers[["rap_20_dg"]] <- my_rapport_list(
  dataset = 'data_20_weeg',
  rapvars = '20_22_v7',
  winkelvar = 'w2',
  prodgroep = "winkelgebied voor dagelijkse boodschappen"
)

list_rapportcijfers[["rap_20_ndg"]] <- my_rapport_list(
  dataset = 'data_20_weeg',
  rapvars = '20_22_v22',
  winkelvar = 'v20',
  prodgroep = "winkelgebied om te winkelen"
)


### rapportcijfers 2022 ---

# dagelijks
list_rapportcijfers[["rap_22_dg"]] <- my_rapport_list(
  dataset = 'data_22_weeg',
  rapvars = '20_22_v7',
  winkelvar = 'v4',
  prodgroep = "winkelgebied voor dagelijkse boodschappen"
)
# niet dagelijks
list_rapportcijfers[["rap_22_ndg"]] <- my_rapport_list(
  dataset = 'data_22_weeg',
  rapvars = '20_22_v22',
  winkelvar = 'v20',
  prodgroep = "winkelgebied om te winkelen"
)

### rapportcijfers 2024 ---

# dagelijks
list_rapportcijfers[["rap_24_dg"]] <- my_rapport_list(
  dataset = 'data_24_weeg',
  rapvars = '24_26_v7',
  winkelvar = 'v4_nw',
  prodgroep = "winkelgebied voor dagelijkse boodschappen"
)
# niet dagelijks
list_rapportcijfers[["rap_24_ndg"]] <- my_rapport_list(
  dataset = 'data_24_weeg',
  rapvars = '24_26_v22',
  winkelvar = 'v20',
  prodgroep = "winkelgebied om te winkelen"
)


### rapportcijfers 2026 ---

# dagelijks
list_rapportcijfers[["rap_26_dg"]] <- my_rapport_list(
  dataset = 'data_26_weeg',
  rapvars = '24_26_v7',
  winkelvar = 'v4_nw',
  prodgroep = "winkelgebied voor dagelijkse boodschappen"
)
# niet dagelijks
list_rapportcijfers[["rap_26_ndg"]] <- my_rapport_list(
  dataset = 'data_26_weeg',
  rapvars = '24_26_v22',
  winkelvar = 'v20',
  prodgroep = "winkelgebied om te winkelen"
)


write_rds(list_rapportcijfers, "01 references/data_rapportcijfers.rds")
