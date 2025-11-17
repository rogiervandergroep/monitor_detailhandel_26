library(janitor)
library(tidyverse)
library(openxlsx)
library(haven)

### opstarten ---

data_totaal <- read_rds("01 references/data_totaal_20_22_24_26.rds")

# v2: hoeveel uitgegeven aan boodschappen

# V3_1: supermarkt
# V3_2: andere winkels
# V3_3: markt
# V3_4: online
# V3_5: anders

# v13: Hoe vaak bezoekt u een markt?
# v14: Wat zijn de drie belangrijkste redenen om de markt niet te bezoeken
# v15: Op welke markt koopt u het meest?
# v15_anders: andere markt namelijk ...
# v16: hoeveel besteedt u op de markt?

# _klas zijn geklassificeerde achtergrondvariabelen
#  opleiding, inkomen huishoude en leeftijd

data_markt <- list()

# selectie van data met v13 tm v15 warenmarkten
data_markt$data_20 <- data_totaal$data_20_weeg |>
  clean_names() |>
  haven::as_factor() |>
  select(
    v2,
    contains("v3"),
    v13:v15_anders,
    v16,
    starts_with("gebied_"),
    contains("_klas"),
    weegfactor_ams,
    weeg_ams_ink
  )

data_markt$data_22 <- data_totaal$data_22_weeg |>
  clean_names() |>
  haven::as_factor() |>
  select(
    v2,
    contains("v3"),
    v13:v15_anders,
    v16,
    starts_with("gebied_"),
    contains("_klas"),
    weegfactor_ams,
    weeg_ams_ink
  ) |>
  mutate(v13 = case_when(is.na(v13) ~ "zelden tot nooit", TRUE ~ v13))


data_markt$data_24 <- data_totaal$data_24_weeg |>
  clean_names() |>
  haven::as_factor() |>
  select(
    v2,
    contains("v3"),
    v13:v15_anders,
    v16,
    starts_with("gebied_"),
    contains("_klas"),
    weegfactor_ams,
    weeg_ams_ink
  ) |>
  mutate(v13 = case_when(v13 == '1 keer week' ~ "1 keer per week", TRUE ~ v13))

# inlezen opgeschoonde namen v15
markt_uniek <- openxlsx::read.xlsx("01 references/markt_uniek.xlsx")


my_markt_function <- function(x) {
  x |>

    left_join(markt_uniek, by = "v15") |>

    mutate(
      v13 = case_when(
        v13 == '1 keer week' ~ '1 keer per week',
        TRUE ~ v13
      )
    ) |>

    # wel versus geen marktbezoek
    mutate(
      v15_schoon = case_when(
        v13 %in%
          c(
            'weet niet, geen antwoord',
            'zelden tot nooit'
          ) ~ 'bezoekt geen markt',
        is.na(v15) &
          (v13 != 'weet niet, geen antwoord' |
            v13 != 'zelden tot nooit') ~ 'bezoekt markt, markt onbekend',
        TRUE ~ v15_schoon
      )
    ) |>

    mutate(
      marktbezoek = case_when(
        v15_schoon == 'bezoekt geen markt' ~ 'bezoekt geen markt',
        TRUE ~ 'bezoekt wel een markt'
      )
    ) |>

    # opdelen stadsdelen in drie gebieden -
    mutate(
      gebieden = case_when(
        gebied_stadsdeel_naam == 'Centrum' ~ 'Centrum',
        gebied_stadsdeel_naam %in%
          c('Zuid', 'West', 'Oost') ~ 'Zuid, West Oost',
        TRUE ~ 'NW, Noord, ZO, Weesp'
      )
    )
}

data_markt_def <- list()


jaren <- c("monitor 2020", "monitor 2022", "monitor 2024")

data_markt_def <- data_markt |>
  map(\(x) my_markt_function(x)) |>
  map2(jaren, \(x, y) add_column(x, monitor = y))

write_rds(data_markt_def, file = "01 references/data_markt_def.RDS")
