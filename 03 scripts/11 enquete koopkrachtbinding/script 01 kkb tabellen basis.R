library(tidyverse)
library(openxlsx)
library(haven)
# nb: haven is nodig om havel_labeled om te zetten naar factor

### inlezen werkbetand

data_totaal <- read_rds("01 references/data_totaal_20_22_24_26.rds")

source("02 scr/01 script factor levels.R")

# inlezen winkelgebieden met ggw gebied en stadsdeel

wg_kol_namen <- c(
  "wink_code",
  "wink_naam",
  "wink_stadsdeel_code",
  "wink_stadsdeel_naam",
  "wink_ggw_code",
  "wink_ggw_naam",
  "wink_oisnaam",
  "wink_oiscode"
)

winkelgebieden <- read.xlsx(
  "01 references/winkelgebieden_20_22_24_26_def.xlsx",
  sheet = 1
) |>
  set_names(wg_kol_namen) |>
  mutate(wink_code = (forcats::as_factor(str_trim(wink_code))))


# winkelgebied_26 <- read.xlsx(
#   "01 references/winkelgebieden_20_22_24_26_def.xlsx", sheet = 4)

# selectie voor koopkrachtbinding rechte tellingen

nd_20 <- c("w3", "w4", "w5", "w6", "w7", "w8", "w9", "w10")
nd_22 <- c(
  "v18_modeartikelen_gv1",
  "v18_elektronica_gv1",
  "v18_huishoudelijk_gv1",
  "v18_woninginrichting_gv1",
  "v18_doehetzelf_gv1",
  "v18_planten_gv1",
  "v18_media_gv1",
  "v18_sportspel_gv1"
)

# zelfde als voor monitor 26
nd_24_26 <- c(
  "v18_nw_modeartikelen_gv1",
  "v18_nw_elektronica_gv1",
  "v18_nw_huishoudelijk_gv1",
  "v18_nw_woninginrichting_gv1",
  "v18_nw_doehetzelf_gv1",
  "v18_nw_planten_gv1",
  "v18_nw_media_gv1",
  "v18_nw_sportspel_gv1"
)


## selectie 2020
data_dg_ndg <- list(
  dg_ndg_20 = data_totaal$data_20_weeg |>
    select(w1, all_of(nd_20), gebied_ggw_code:weeg_geb_ink) |>
    filter(),

  ## selectie 2022
  dg_ndg_22 = data_totaal$data_22_weeg |>
    select(v1, all_of(nd_22), gebied_ggw_code:weeg_geb_ink),

  ## selectie 2024
  dg_ndg_24 = data_totaal$data_24_weeg |>
    select(v1_nw, all_of(nd_24_26), gebied_ggw_code:weeg_geb_ink),

  ## selectie 2026
  dg_ndg_26 = data_totaal$data_26_weeg |>
    select(v1_nw, all_of(nd_24_26), gebied_ggw_code:weeg_geb_ink)
)


# functie voor rechte tellingen per straat voor amsterdam
my_summary_ams <- function(x, group_var) {
  x |>
    group_by(
      across(all_of(group_var))
    ) |>
    summarise(
      aantal = n(),
      aantal_gew = sum(weeg_geb_ink, na.rm = T)
    ) |>
    add_column(
      woon_gebied_naam = "Amsterdam",
      woon_gebied_code = "Amsterdam"
    ) |>
    set_names(
      "wink_code",
      "aantal",
      "aantal_gew",
      "woon_gebied_naam",
      "woon_gebied_code"
    ) |>
    mutate(wink_code = forcats::as_factor(str_trim(wink_code))) |>
    left_join(winkelgebieden, by = "wink_code")
}

# functie voor rechte tellingen per straat en per woonstadsdeel---
my_summary_sd <- function(x, group_var) {
  x |>
    group_by(
      gebied_stadsdeel_code,
      gebied_stadsdeel_naam,
      across(all_of(group_var))
    ) |>
    summarise(
      aantal = n(),
      aantal_gew = sum(weeg_geb_ink, na.rm = T)
    ) |>
    set_names(
      "woon_gebied_code",
      "woon_gebied_naam",
      "wink_code",
      "aantal",
      "aantal_gew"
    ) |>
    mutate(wink_code = forcats::as_factor(str_trim(wink_code))) |>
    left_join(winkelgebieden, by = "wink_code")
}

# functie voor rechte tellingen per straat en per woongebied---
my_summary_ggw <- function(x, group_var) {
  x |>
    group_by(gebied_ggw_code, gebied_ggw_naam, across(all_of(group_var))) |>
    summarise(
      aantal = n(),
      aantal_gew = sum(weeg_geb_ink, na.rm = T)
    ) |>
    set_names(
      "woon_gebied_code",
      "woon_gebied_naam",
      "wink_code",
      "aantal",
      "aantal_gew"
    ) |>
    mutate(wink_code = forcats::as_factor(str_trim(wink_code))) |>
    left_join(winkelgebieden, by = "wink_code")
}


group_var_20 <- c("w1", nd_20)
group_var_22 <- c("v1", nd_22)
group_var_24_26 <- c("v1_nw", nd_24_26)

# inlezen omzetcijfers
df_omzet <- readxl::read_xlsx(
  "01 references/weging_artikelgroepen.xlsx",
  sheet = 2
) |>
  mutate(productgroep = str_trim(productgroep, "both"))


# my_omzetcijfers_mutate <- function(x) {
#   x |>
#     mutate(
#       omzetcijfers = case_when(
#         productgroep == "dagelijks" ~ 1,
#         productgroep == "modeartikelen" ~ 0.188,
#         productgroep == "elektronica" ~ 0.084,
#         productgroep == "huishoudelijk" ~ 0.171,
#         productgroep == "woninginrichting" ~ 0.258,
#         productgroep == "doehetzelf" ~ 0.043,
#         productgroep == "planten" ~ 0.050,
#         productgroep == "media" ~ 0.080,
#         productgroep == "sportspel" ~ 0.032
#       )
#     )
# }

### rechte tellingen per straat ---

tabel_dg_ndg_20 <- bind_rows(
  group_var_20 |>
    map(\(x) my_summary_ams(data_dg_ndg[["dg_ndg_20"]], x)) |>
    map2_df(productgroepen, \(x, y) mutate(x, productgroep = y)) |>
    add_column(gebied = 'gemeente') |>
    left_join(df_omzet[c("productgroep", "omz_2019")], by = "productgroep") |>
    mutate(aantal_gew_omz = aantal_gew * omz_2019),

  group_var_20 |>
    map(\(x) my_summary_sd(data_dg_ndg[["dg_ndg_20"]], x)) |>
    map2_df(productgroepen, \(x, y) mutate(x, productgroep = y)) |>
    add_column(gebied = 'stadsdeel') |>
    left_join(df_omzet[c("productgroep", "omz_2019")], by = "productgroep") |>
    mutate(aantal_gew_omz = aantal_gew * omz_2019),

  group_var_20 |>
    map(\(x) my_summary_ggw(data_dg_ndg[["dg_ndg_20"]], x)) |>
    map2_df(productgroepen, \(x, y) mutate(x, productgroep = y)) |>
    add_column(gebied = 'ggw_gebied') |>
    left_join(df_omzet[c("productgroep", "omz_2019")], by = "productgroep") |>
    mutate(aantal_gew_omz = aantal_gew * omz_2019)
) |>
  filter(
    wink_code != 999,
    wink_code != 900
  ) |>
  rename(omzetcijfers = omz_2019)


### tabel 2022
tabel_dg_ndg_22 <- bind_rows(
  group_var_22 |>
    map(\(x) my_summary_ams(data_dg_ndg[["dg_ndg_22"]], x)) |>
    map2_df(productgroepen, \(x, y) mutate(x, productgroep = y)) |>
    add_column(gebied = 'gemeente') |>
    left_join(df_omzet[c("productgroep", "omz_2021")], by = "productgroep") |>
    mutate(aantal_gew_omz = aantal_gew * omz_2021),

  group_var_22 |>
    map(\(x) my_summary_sd(data_dg_ndg[["dg_ndg_22"]], x)) |>
    map2_df(productgroepen, \(x, y) mutate(x, productgroep = y)) |>
    add_column(gebied = 'stadsdeel') |>
    left_join(df_omzet[c("productgroep", "omz_2021")], by = "productgroep") |>
    mutate(aantal_gew_omz = aantal_gew * omz_2021),

  group_var_22 |>
    map(\(x) my_summary_ggw(data_dg_ndg[["dg_ndg_22"]], x)) |>
    map2_df(productgroepen, \(x, y) mutate(x, productgroep = y)) |>
    add_column(gebied = 'ggw_gebied') |>
    left_join(df_omzet[c("productgroep", "omz_2021")], by = "productgroep") |>
    mutate(aantal_gew_omz = aantal_gew * omz_2021)
) |>
  filter(
    wink_code != 999,
    wink_code != 900
  ) |>
  rename(omzetcijfers = omz_2021)


### tabel 2024
tabel_dg_ndg_24 <- bind_rows(
  group_var_24_26 |>
    map(\(x) my_summary_ams(data_dg_ndg[["dg_ndg_24"]], x)) |>
    map2_df(productgroepen, \(x, y) mutate(x, productgroep = y)) |>
    add_column(gebied = 'gemeente') |>
    left_join(df_omzet[c("productgroep", "omz_2023")], by = "productgroep") |>
    mutate(aantal_gew_omz = aantal_gew * omz_2023),

  group_var_24_26 |>
    map(\(x) my_summary_sd(data_dg_ndg[["dg_ndg_24"]], x)) |>
    map2_df(productgroepen, \(x, y) mutate(x, productgroep = y)) |>
    add_column(gebied = 'stadsdeel') |>
    left_join(df_omzet[c("productgroep", "omz_2023")], by = "productgroep") |>
    mutate(aantal_gew_omz = aantal_gew * omz_2023),

  group_var_24_26 |>
    map(\(x) my_summary_ggw(data_dg_ndg[["dg_ndg_24"]], x)) |>
    map2_df(productgroepen, \(x, y) mutate(x, productgroep = y)) |>
    add_column(gebied = 'ggw_gebied') |>
    left_join(df_omzet[c("productgroep", "omz_2023")], by = "productgroep") |>
    mutate(aantal_gew_omz = aantal_gew * omz_2023)
) |>
  filter(
    wink_code != 999,
    wink_code != 900
  ) |>
  rename(omzetcijfers = omz_2023)


### tabel 2026
tabel_dg_ndg_26 <- bind_rows(
  group_var_24_26 |>
    map(\(x) my_summary_ams(data_dg_ndg[["dg_ndg_26"]], x)) |>
    map2_df(productgroepen, \(x, y) mutate(x, productgroep = y)) |>
    add_column(gebied = 'gemeente') |>
    left_join(df_omzet[c("productgroep", "omz_2024")], by = "productgroep") |>
    mutate(aantal_gew_omz = aantal_gew * omz_2024),

  group_var_24_26 |>
    map(\(x) my_summary_sd(data_dg_ndg[["dg_ndg_26"]], x)) |>
    map2_df(productgroepen, \(x, y) mutate(x, productgroep = y)) |>
    add_column(gebied = 'stadsdeel') |>
    left_join(df_omzet[c("productgroep", "omz_2024")], by = "productgroep") |>
    mutate(aantal_gew_omz = aantal_gew * omz_2024),

  group_var_24_26 |>
    map(\(x) my_summary_ggw(data_dg_ndg[["dg_ndg_26"]], x)) |>
    map2_df(productgroepen, \(x, y) mutate(x, productgroep = y)) |>
    add_column(gebied = 'ggw_gebied') |>
    left_join(df_omzet[c("productgroep", "omz_2024")], by = "productgroep") |>
    mutate(aantal_gew_omz = aantal_gew * omz_2024)
) |>
  filter(
    wink_code != 999,
    wink_code != 900
  ) |>
  rename(omzetcijfers = omz_2024)


### rechte tellingen per gebied of stadsdeel ---
my_summary_gebied <- function(x, group_var) {
  x |>
    group_by(
      gebied,
      woon_gebied_code,
      woon_gebied_naam,
      productgroep,
      omzetcijfers,
      across(all_of(group_var))
    ) |>
    summarise(
      aantal = sum(aantal),
      aantal_gew = sum(aantal_gew),
      aantal_gew_omz = sum(aantal_gew_omz)
    ) |>
    group_by(woon_gebied_code, woon_gebied_naam, productgroep, omzetcijfers) |>
    mutate(
      aandeel = aantal / sum(aantal),
      aandeel_gew = aantal_gew / sum(aantal_gew, na.rm = T),
      aandeel_gew_omz = aantal_gew_omz / sum(aantal_gew_omz, na.rm = T),
    )
}


# toevoegen hoofdgroep en subgroep
my_mutate_groep <- function(x) {
  x |>

    mutate(
      hoofdgroep = case_when(
        productgroep == "dagelijks" ~ 'dagelijks',
        TRUE ~ 'niet-dagelijks'
      ),
      subgroep = case_when(
        productgroep == "dagelijks" ~ "dagelijks",
        productgroep %in% recreatief ~ "recreatief",
        productgroep %in% doelgericht ~ "doelgericht"
      )
    )
}


### naar stadsdeel
tabel_sd <- bind_rows(
  tabel_dg_ndg_20 |>
    my_summary_gebied(
      group_var = c("wink_stadsdeel_code", "wink_stadsdeel_naam")
    ) |>
    add_column(monitor = 'monitor 2020') |>
    my_mutate_groep(),

  tabel_dg_ndg_22 |>
    my_summary_gebied(
      group_var = c("wink_stadsdeel_code", "wink_stadsdeel_naam")
    ) |>
    add_column(monitor = 'monitor 2022') |>
    my_mutate_groep(),

  tabel_dg_ndg_24 |>
    my_summary_gebied(
      group_var = c("wink_stadsdeel_code", "wink_stadsdeel_naam")
    ) |>
    add_column(monitor = 'monitor 2024') |>
    my_mutate_groep(),

  tabel_dg_ndg_26 |>
    my_summary_gebied(
      group_var = c("wink_stadsdeel_code", "wink_stadsdeel_naam")
    ) |>
    add_column(monitor = 'monitor 2026') |>
    my_mutate_groep()
)

### naar gebied
tabel_geb <- bind_rows(
  tabel_dg_ndg_20 |>
    my_summary_gebied(
      group_var = c("wink_ggw_code", "wink_ggw_naam")
    ) |>
    add_column(monitor = 'monitor 2020') |>
    my_mutate_groep(),

  tabel_dg_ndg_22 |>
    my_summary_gebied(
      group_var = c("wink_ggw_code", "wink_ggw_naam")
    ) |>
    add_column(monitor = 'monitor 2022') |>
    my_mutate_groep(),

  tabel_dg_ndg_24 |>
    my_summary_gebied(
      group_var = c("wink_ggw_code", "wink_ggw_naam")
    ) |>
    add_column(monitor = 'monitor 2024') |>
    my_mutate_groep(),

  tabel_dg_ndg_26 |>
    my_summary_gebied(
      group_var = c("wink_ggw_code", "wink_ggw_naam")
    ) |>
    add_column(monitor = 'monitor 2026') |>
    my_mutate_groep()
)


### data Amsterdam ---

sel_cols <- c(
  "monitor",
  "woon_gebied_code",
  "woon_gebied_naam",
  "wink_stadsdeel_code",
  "wink_ggw_code",
  "wink_ggw_naam",
  "wink_stadsdeel_naam",
  "productgroep",
  "hoofdgroep",
  "subgroep",
  "omzetcijfers",
  "aantal_gew",
  "aantal_gew_omz",
  "aandeel_gew",
  "aandeel_gew_omz"
)

tabel_ams_sd <- tabel_sd |>
  ungroup() |>
  filter(gebied == 'gemeente') |>
  select(any_of(sel_cols))

# totalen berekenen naar hoofdgroep en subgroep -

tabel_sd_sd <- tabel_sd |>
  ungroup() |>
  filter(gebied == 'stadsdeel') |>
  select(any_of(sel_cols))

tabel_geb_geb <- tabel_geb |>
  ungroup() |>
  filter(gebied == 'ggw_gebied') |>
  select(any_of(sel_cols))


my_totaal <- function(x, winkn, winkc, hoofd_sub) {
  x |>
    group_by(
      monitor,
      woon_gebied_code,
      woon_gebied_naam,
      {{ winkn }},
      {{ winkc }},
      {{ hoofd_sub }}
    ) |>
    summarise(
      aantal_gew_h = sum(aantal_gew),
      aantal_gew_omz_h = sum(aantal_gew_omz)
    ) |>
    group_by(
      monitor,
      woon_gebied_code,
      woon_gebied_naam,
      {{ hoofd_sub }}
    ) |>
    mutate(
      aandeel_gew = aantal_gew_h / sum(aantal_gew_h),
      aandeel_gew_omz = aantal_gew_omz_h / sum(aantal_gew_omz_h)
    )
}


### Dit zijn de eindtabellen ---

# to do::
## recode factor levels

## een functie om de volgorde van items te bepalen
## de vector_levels zijn te vinden in script factor levels
my_levels <- function(x, var, var_levels) {
  x |>
    mutate(
      {{ var }} := factor({{ var }}, levels = var_levels)
    )
}


# functie om kolom toe te voegen met eigen gebied
my_eigen_ams <- function(x) {
  x |>
    mutate(
      eigen_gebied = case_when(
        wink_stadsdeel_naam == 'online' ~ 'online',
        wink_stadsdeel_naam == 'MRA' ~ 'MRA',
        wink_stadsdeel_naam == 'Buiten MRA' ~ 'Buiten MRA',
        TRUE ~ 'Amsterdam'
      )
    ) |>
    mutate(
      eigen_gebied_centrum = case_when(
        wink_stadsdeel_naam == 'online' ~ 'online',
        wink_stadsdeel_naam == 'MRA' ~ 'MRA',
        wink_stadsdeel_naam == 'Buiten MRA' ~ 'Buiten MRA',
        wink_stadsdeel_naam == 'Centrum' ~ 'Centrum',
        TRUE ~ 'overig Amsterdam'
      )
    )
}

# functie om kolom toe te voegen met eigen gebied
my_eigen_sd <- function(x) {
  x |>
    mutate(
      eigen_gebied = case_when(
        woon_gebied_code == wink_stadsdeel_code ~ "zelfde stadsdeel",
        wink_stadsdeel_code == 'online' ~ 'online',
        wink_stadsdeel_code == 'MRA' ~ 'MRA',
        wink_stadsdeel_code == 'Buiten MRA' ~ 'Buiten MRA',
        TRUE ~ 'overig Amsterdam'
      )
    ) |>
    mutate(
      eigen_gebied_centrum = case_when(
        wink_stadsdeel_naam == 'Centrum' ~ 'Centrum',
        woon_gebied_code == wink_stadsdeel_code ~ "zelfde stadsdeel",
        wink_stadsdeel_naam == 'online' ~ 'online',
        wink_stadsdeel_naam == 'MRA' ~ 'MRA',
        wink_stadsdeel_naam == 'Buiten MRA' ~ 'Buiten MRA',
        TRUE ~ 'overig Amsterdam'
      )
    )
}

# functie om kolom toe te voegen met eigen gebied
my_eigen_geb <- function(x) {
  x |>
    mutate(
      eigen_gebied = case_when(
        woon_gebied_code == wink_ggw_code ~ "zelfde GGW-gebied",
        wink_ggw_code == 'online' ~ 'online',
        wink_ggw_code == 'MRA' ~ 'MRA',
        wink_ggw_code == 'Buiten MRA' ~ 'Buiten MRA',
        TRUE ~ 'overig Amsterdam'
      )
    ) |>
    mutate(
      eigen_gebied_centrum = case_when(
        wink_ggw_code %in% c('GA01', 'GA02') ~ 'GGW-gebieden Centrum',
        woon_gebied_code == wink_ggw_code ~ "zelfde GGW-gebied",
        wink_ggw_code == 'online' ~ 'online',
        wink_ggw_code == 'MRA' ~ 'MRA',
        wink_ggw_code == 'Buiten MRA' ~ 'Buiten MRA',
        TRUE ~ 'overig Amsterdam'
      )
    )
}


### totalen voor Amsterdam naar stadsdeel ---
pub_tabel_ams_sd <- list(
  # onderscheid dagelijks niet dagelijks
  ams_hoofd = tabel_ams_sd |>
    my_totaal(
      winkc = wink_stadsdeel_code,
      winkn = wink_stadsdeel_naam,
      hoofd_sub = hoofdgroep
    ) |>
    my_levels(
      var = hoofdgroep,
      var_levels = hoofdgroep_levels
    ) |>
    rename("productgroep" = "hoofdgroep"),

  # onderscheid dagelijks recreatief en doelgericht
  ams_sub = tabel_ams_sd |>
    my_totaal(
      winkc = wink_stadsdeel_code,
      winkn = wink_stadsdeel_naam,
      hoofd_sub = subgroep
    ) |>
    my_levels(
      var = subgroep,
      var_levels = subgroep_levels
    ) |>
    rename("productgroep" = "subgroep"),

  # onderscheid naar alle productgroepen
  ams_prodgroep = tabel_ams_sd |>
    my_totaal(
      winkc = wink_stadsdeel_code,
      winkn = wink_stadsdeel_naam,
      hoofd_sub = productgroep
    ) |>
    my_levels(
      var = productgroep,
      var_levels = productgroep_levels
    )
) |>
  map(\(x) {
    my_levels(
      x,
      var = wink_stadsdeel_code,
      var_levels = wink_stadsdeel_code_levels
    )
  }) |>
  map(\(x) my_eigen_ams(x))


### totalen van stadsdeel naar stadsdeel ---
pub_tabel_sd_sd <- list(
  # onderscheid dagelijks niet dagelijks
  ams_hoofd = tabel_sd_sd |>
    my_totaal(
      winkc = wink_stadsdeel_code,
      winkn = wink_stadsdeel_naam,
      hoofd_sub = hoofdgroep
    ) |>
    my_levels(
      var = hoofdgroep,
      var_levels = hoofdgroep_levels
    ) |>
    rename("productgroep" = "hoofdgroep"),

  # onderscheid dagelijks recreatief en doelgericht
  ams_sub = tabel_sd_sd |>
    my_totaal(
      winkc = wink_stadsdeel_code,
      winkn = wink_stadsdeel_naam,
      hoofd_sub = subgroep
    ) |>
    my_levels(
      var = subgroep,
      var_levels = subgroep_levels
    ) |>
    rename("productgroep" = "subgroep"),

  # onderscheid naar alle productgroepen
  ams_prodgroep = tabel_sd_sd |>
    my_totaal(
      winkc = wink_stadsdeel_code,
      winkn = wink_stadsdeel_naam,
      hoofd_sub = productgroep
    ) |>
    my_levels(
      var = productgroep,
      var_levels = hoofdgroep_levels
    )
) |>
  map(\(x) {
    my_levels(
      x,
      var = wink_stadsdeel_code,
      var_levels = wink_stadsdeel_code_levels
    )
  }) |>
  map(\(x) my_eigen_sd(x)) |>
  map(\(x) {
    my_levels(x, var = woon_gebied_code, var_levels = woongebied_code_levels)
  })


### van gebied naar gebied ---
pub_tabel_geb_geb <- list(
  # onderscheid dagelijks niet dagelijks
  ams_hoofd = tabel_geb_geb |>
    my_totaal(
      winkc = wink_ggw_code,
      winkn = wink_ggw_naam,
      hoofd_sub = hoofdgroep
    ) |>
    my_levels(
      var = hoofdgroep,
      var_levels = hoofdgroep_levels
    ) |>
    rename("productgroep" = "hoofdgroep"),

  # onderscheid dagelijks recreatief en doelgericht
  ams_sub = tabel_geb_geb |>
    my_totaal(
      winkc = wink_ggw_code,
      winkn = wink_ggw_naam,
      hoofd_sub = subgroep
    ) |>
    my_levels(
      var = subgroep,
      var_levels = subgroep_levels
    ) |>
    rename("productgroep" = "subgroep"),

  # onderscheid naar alle productgroepen
  ams_prodgroep = tabel_geb_geb |>
    my_totaal(
      winkc = wink_ggw_code,
      winkn = wink_ggw_naam,
      hoofd_sub = productgroep
    ) |>
    my_levels(
      var = productgroep,
      var_levels = hoofdgroep_levels
    )
) |>
  map(\(x) my_eigen_geb(x)) |>
  map(\(x) {
    my_levels(x, var = wink_ggw_code, var_levels = wink_ggw_code_levels)
  }) |>
  map(\(x) {
    my_levels(x, var = woon_gebied_code, var_levels = woon_ggw_code_levels)
  })

###

### opsommingen naar eigen gebied ---

my_sum_eigen_gebied <- function(x) {
  x |>
    group_by(
      monitor,
      woon_gebied_code,
      woon_gebied_naam,
      productgroep,
      eigen_gebied
    ) |>
    summarise(aandeel_gew_omz = sum(aandeel_gew_omz)) |>
    mutate(
      eigen_gebied = factor(
        eigen_gebied,
        levels = c(
          "zelfde GGW-gebied",
          "zelfde stadsdeel",
          'overig Amsterdam',
          "Amsterdam",
          "MRA",
          "Buiten MRA",
          "online"
        )
      )
    )
}


my_sum_eigen_gebied_centrum <- function(x) {
  x |>
    group_by(
      monitor,
      woon_gebied_code,
      woon_gebied_naam,
      productgroep,
      eigen_gebied_centrum
    ) |>
    summarise(aandeel_gew_omz = sum(aandeel_gew_omz)) |>
    mutate(
      eigen_gebied_centrum = factor(
        eigen_gebied_centrum,
        levels = c(
          "GGW-gebieden Centrum",
          "Centrum",
          "zelfde GGW-gebied",
          "zelfde stadsdeel",
          'overig Amsterdam',
          "Amsterdam",
          "MRA",
          "Buiten MRA",
          "online"
        )
      )
    )
}


pub_tabel_ams_sd_tot <- pub_tabel_ams_sd |>
  map(\(x) my_sum_eigen_gebied(x))

pub_tabel_sd_sd_tot <- pub_tabel_sd_sd |>
  map(\(x) my_sum_eigen_gebied(x))

pub_tabel_geb_geb_tot <- pub_tabel_geb_geb |>
  map(\(x) my_sum_eigen_gebied(x))


pub_tabel_ams_sd_centrum <- pub_tabel_ams_sd |>
  map(\(x) my_sum_eigen_gebied_centrum(x))

pub_tabel_sd_sd_centrum <- pub_tabel_sd_sd |>
  map(\(x) my_sum_eigen_gebied_centrum(x))

pub_tabel_geb_geb_centrum <- pub_tabel_geb_geb |>
  map(\(x) my_sum_eigen_gebied_centrum(x))


write_rds(pub_tabel_ams_sd, "01 references/pub_tabel_ams_sd.rds")
write_rds(pub_tabel_sd_sd, "01 references/pub_tabel_sd_sd.rds")
write_rds(pub_tabel_geb_geb, "01 references/pub_tabel_geb_geb.rds")

write_rds(pub_tabel_ams_sd_tot, "01 references/pub_tabel_ams_sd_tot.rds")
write_rds(pub_tabel_sd_sd_tot, "01 references/pub_tabel_sd_sd_tot.rds")
write_rds(pub_tabel_geb_geb_tot, "01 references/pub_tabel_geb_geb_tot.rds")

write_rds(
  pub_tabel_ams_sd_centrum,
  "01 references/pub_tabel_ams_sd_centrum.rds"
)
write_rds(pub_tabel_sd_sd_centrum, "01 references/pub_tabel_sd_sd_centrum.rds")
write_rds(
  pub_tabel_geb_geb_centrum,
  "01 references/pub_tabel_geb_geb_centrum.rds"
)

## weghalen dagelijks uit sub en productgoep

my_list <- function(a, b, c) {
  list(
    ams_sd = bind_rows(
      a[["ams_hoofd"]],
      a[["ams_sub"]] |>
        filter(productgroep != 'dagelijks'),
      a[["ams_prodgroep"]] |>
        filter(productgroep != 'dagelijks')
    ),

    sd_sd = bind_rows(
      b[["ams_hoofd"]],
      b[["ams_sub"]] |>
        filter(productgroep != 'dagelijks'),
      b[["ams_prodgroep"]] |>
        filter(productgroep != 'dagelijks')
    ),

    geb_geb = bind_rows(
      c[["ams_hoofd"]],
      c[["ams_sub"]] |>
        filter(productgroep != 'dagelijks'),
      c[["ams_prodgroep"]] |>
        filter(productgroep != 'dagelijks')
    )
  )
}


tabel_binding_basis <- my_list(
  a = pub_tabel_ams_sd,
  b = pub_tabel_sd_sd,
  c = pub_tabel_geb_geb
) |>
  write.xlsx("04 reports/03 tabellen/tabel_binding_basis.xlsx")


tabel_binding_totaal <- my_list(
  a = pub_tabel_ams_sd_tot,
  b = pub_tabel_sd_sd_tot,
  c = pub_tabel_geb_geb_tot
) |>
  write.xlsx("04 reports/03 tabellen/tabel_binding_totaal.xlsx")


tabel_binding_centrum <- my_list(
  a = pub_tabel_ams_sd_centrum,
  b = pub_tabel_sd_sd_centrum,
  c = pub_tabel_geb_geb_centrum
) |>
  write.xlsx("04 reports/03 tabellen/tabel_binding_centrum.xlsx")
