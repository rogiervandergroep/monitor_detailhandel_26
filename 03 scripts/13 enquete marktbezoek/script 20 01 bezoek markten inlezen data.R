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

jaren <- c("monitor 2020", "monitor 2022", "monitor 2024", "monitor 2026")

my_cleaning <- function(x) {
  x |>
    clean_names() |>
    haven::as_factor() |>
    select(
      monitor,
      v2,
      contains("v3"),
      v13:v15_anders,
      v16,
      starts_with("gebied_"),
      contains("_klas"),
      contains("weeg")
    )
}

# selectie van data met v13 tm v15 warenmarkten
data_markt <- data_totaal |>
  map2(jaren, \(x, y) add_column(x, monitor = y)) |>
  map(\(x) my_cleaning(x))

# inlezen opgeschoonde namen v15
markt_uniek <- openxlsx::read.xlsx("01 references/markt_uniek.xlsx")


my_markt_function <- function(x) {
  x |>

    left_join(markt_uniek, by = "v15") |>

    mutate(
      v13 = case_when(
        v13 == '1 keer week' ~ '1 keer per week',
        v13 == 'weet niet, geen antwoord' ~ 'onbekend',
        is.na(v13) ~ "onbekend",
        TRUE ~ v13
      )
    ) |>

    mutate(
      v15_schoon = case_when(
        v13 == 'zelden tot nooit' ~ 'bezoekt geen markt',
        v13 != 'zelden tot nooit' & is.na(v15) ~ 'onbekend',
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

data_markt_def <- data_markt |>
  map(\(x) my_markt_function(x)) |>
  map(\(x) select(x, monitor, v2, v13, v15, v15_schoon, v16, everything()))

write_rds(data_markt_def, file = "01 references/data_markt_def.rds")
