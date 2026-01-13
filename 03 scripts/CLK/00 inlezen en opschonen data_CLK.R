setwd("C:/Users/knijff008/OneDrive - Gemeente Amsterdam/Data - afdeling Onderzoek en Statistiek - OS 26 Monitor Detailhandel GIT")
install.packages("readr")
install.packages("here")
install.packages("tidyverse")
install.packages("haven")
install.packages("janitor")
install.packages("openxlsx")
install.packages("dplyr")
library(readr)
library(tidyverse)
library(haven)
library(janitor)
library(openxlsx)
library(here)
library(dplyr)

### --- inlezen postcodes ---
# NB: postcodes zijn gedownload op 25 10 2025 van azure databricks
data_pc6 <- read_csv(
  "01 references/postcode6 2025.csv"
) |>
  mutate(
    gebied_wijk_code = str_replace_na(gebied_wijk_code, "NA")
  )
### ---

# omdat sommige 4-cijferige postcodes in meerdere wijken vallen is gekozen voor de meest voorkomende wijk
data_pc4 <- data_pc6 |>
  mutate(postcode = str_sub(postcode, 1, 4)) |>
  group_by(postcode, gebied_wijk_code) |>
  mutate(aantal = n()) |>
  distinct() |>
  group_by(postcode) |>
  filter(aantal == max(aantal)) |>
  select(-aantal)

data_pc_def <- bind_rows(data_pc6, data_pc4)


### --- inlezen bevolking per wijk ---
data_bev <- read.xlsx(
  "01 references/bev_totaal_20_22_24_25.xlsx"
) |>
  filter(Gebiedsindeling == 'Wijken') |>
  select(Gebiedscode, Jaar, Bevolking.totaal) |>
  set_names(c("gebied_wijk_code", "Jaar", "bev_aantal")) |>
  mutate(Jaar = as.integer(Jaar))

### --- data inkomen ---
data_inkomen <- read.xlsx("01 references/inkomen_20_22_24_26.xlsx") |>
  set_names(
    c(
      "gebiedsindeling",
      "gebied_naam",
      "gebied_code",
      "jaar",
      "thema",
      "bevolking",
      "med_hh_inkomen",
      "gem_hh_inkomen"
    )
  ) |>
  filter(gebiedsindeling == 'Wijken') |>
  select(gebied_naam, gebied_code, jaar, med_hh_inkomen, gem_hh_inkomen) |>
  mutate(gebied_code = replace_na(gebied_code, "NA"))


gem_ink_ams <- tibble(
  jaar = c(2020, 2022, 2024, 2025),
  med_hh_inkomen = c(32910, 36896, 36896, 36896),
  gem_hh_inkomen = c(45900, 51974, 51974, 51974)
) |>
  mutate(jaar = as.integer(jaar))


###############################################
### inlezen datasets 2020, 2022, 2024, 2026 ###
###############################################

# dataset 2020
data_20 <- haven::read_sav(
  "00 data/ruw enquete/19015 Amsterdam DEF 2020.sav"
) |>
  clean_names() |>
  mutate(postcode = str_trim(str_to_upper(pttkod), "both")) |>
  left_join(data_pc_def, by = c("postcode"))

# dataset 2022
data_22 <- list(
  data_22_1 = haven::read_sav("00 data/ruw enquete/20015_1_Amsterdam.sav"),
  data_22_2 = haven::read_sav("00 data/ruw enquete/20015_2_Amsterdam.sav"),
  data_22_3 = haven::read_sav("00 data/ruw enquete/20015_3_Amsterdam.sav"),
  data_22_4 = haven::read_sav("00 data/ruw enquete/20015_4_Amsterdam.sav")
) |>
  map(\(x) clean_names(x)) |>
  bind_rows() |>
  mutate(postcode = str_trim(str_to_upper(postcode_def), "both")) |>
  left_join(data_pc_def, by = c("postcode"))

# dataset 2024
# wijken en stadsdelen zijn al aanwezig
data_24 <- haven::read_sav(
  "00 data/ruw enquete/230349 Amsterdam DEF 2024.sav"
) |>
  clean_names() |>
  mutate(postcode = str_trim(str_to_upper(postcode_def), "both")) |>
  left_join(data_pc_def, by = c("postcode"))


data_26 <- haven::read_sav(
  "00 data/ruw enquete/240095 Amsterdam DEF 2026.sav"
) |>
  clean_names() |>
  mutate(postcode = str_trim(str_to_upper(postcode_def), "both")) |>
  left_join(data_pc_def, by = c("postcode"))

### weegfactoren toevoegen ---

my_weegfactor <- function(x, jaartal) {
  x |>
    group_by(
      gebied_wijk_code,
      gebied_wijk_naam,
      gebied_ggw_code,
      gebied_ggw_naam
    ) |>
    summarise(aantal_resp = n()) |>
    left_join(
      data_bev[data_bev[['Jaar']] == jaartal, ],
      by = "gebied_wijk_code"
    ) |>
    left_join(
      data_inkomen[data_inkomen[['jaar']] == 2022, ],
      by = c("gebied_wijk_code" = "gebied_code")
    ) |>
    filter(gebied_ggw_naam != '') |>
    mutate(
      bev_aantal = case_when(
        is.na(bev_aantal) ~ 0,
        TRUE ~ bev_aantal
      )
    ) |>
    group_by(gebied_ggw_code) |>
    mutate(
      resp_aandeel_geb = aantal_resp / sum(aantal_resp),
      bev_aandeel_geb = bev_aantal / sum(bev_aantal),
      resp_aandeel_geb = aantal_resp / sum(aantal_resp),
      weegfactor_geb = resp_aandeel_geb / bev_aandeel_geb
    ) |>
    ungroup() |>
    mutate(
      resp_aandeel_ams = aantal_resp / sum(aantal_resp),
      bev_aandeel_ams = bev_aantal / sum(bev_aantal),
      weegfactor_ams = resp_aandeel_ams / bev_aandeel_ams
    ) |>
    mutate(
      ink_aandeel_med = med_hh_inkomen /
        pull(gem_ink_ams[gem_ink_ams[['jaar']] == 2022, c("med_hh_inkomen")]),
      ink_aandeel_med = replace_na(ink_aandeel_med, 1),
      weeg_geb_ink = weegfactor_geb * ink_aandeel_med,
      weeg_ams_ink = weegfactor_ams * ink_aandeel_med
    ) |>
    select(
      gebied_wijk_code,
      weegfactor_geb,
      weegfactor_ams,
      ink_aandeel_med,
      weeg_geb_ink,
      weeg_ams_ink
    )
}

my_aftopping <- function(x) {
  x |>
    mutate(across(
      weegfactor_geb:weeg_ams_ink,
      ~ replace(.x, !is.finite(.x), 1)
    )) |>
    mutate(across(
      weegfactor_geb:weeg_ams_ink,
      ~ replace(.x, .x > 3, 3)
    ))
}

tabel_20 <- data_20 |>
  my_weegfactor(2020) |>
  my_aftopping()

tabel_22 <- data_22 |>
  my_weegfactor(2022) |>
  my_aftopping()

tabel_24 <- data_24 |>
  my_weegfactor(2024) |>
  my_aftopping()

tabel_26 <- data_26 |>
  my_weegfactor(2025) |>
  my_aftopping()

### functie om met een selectie van kolommen labels toe te voegen
my_label <- function(x, cols) {
  x |>
    mutate(
      across({{ cols }}, haven::as_factor, .names = "{.col}_label")
    )
}

### ---
data_20_weeg <- data_20 |>
  left_join(tabel_20, by = "gebied_wijk_code") |>
  my_label(cols = c(w1:w10)) |>
  mutate(across(
    weegfactor_geb:weeg_ams_ink,
    ~ replace(.x, is.na(.x), 1)
  ))

data_22_weeg <- data_22 |>
  left_join(tabel_22, by = "gebied_wijk_code") |>
  my_label(cols = c(v1, v1_codes, v4, v4_codes)) |>
  mutate(across(
    weegfactor_geb:weeg_ams_ink,
    ~ replace(.x, is.na(.x), 1)
  ))

data_24_weeg <- data_24 |>
  left_join(tabel_24, by = "gebied_wijk_code") |>
  my_label(cols = c(v1_nw, v1_nw_codes, v4_nw, v4_nw_codes)) |>
  mutate(across(
    weegfactor_geb:weeg_ams_ink,
    ~ replace(.x, is.na(.x), 1)
  ))

data_26_weeg <- data_26 |>
  left_join(tabel_26, by = "gebied_wijk_code") |>
  my_label(cols = c(v1_nw, v1_nw_codes, v4_nw, v4_nw_codes)) |>
  mutate(across(
    weegfactor_geb:weeg_ams_ink,
    ~ replace(.x, is.na(.x), 1)
  ))
