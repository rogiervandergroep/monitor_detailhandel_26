# hercoderen achtergrondvars

source(
  "03 scripts/10 enquete opschonen data/script 00 inlezen en opschonen data.R"
)

lev_opl <- c(
  "basisopgeleid (maximaal mbo-1)",
  "mbo-opgeleid (mbo-2 tm 4 opleiding of havo of vwo afgerond)",
  "hbo/wo-opgeleid (hbo- of wo-opleiding afgerond)",
  "opleiding onbekend"
)

lev_ink <- c(
  "inkomen laag",
  "inkomen midden",
  "inkomen hoog",
  "inkomen onbekend"
)

lev_lft <- c(
  "34 jaar of jonger",
  "35 jaar tot en met 55 jaar",
  "56 jaar of ouder",
  "onbekend"
)


### hercoderen opleiding ---
my_opl <- function(x) {
  x |>
    mutate(
      opleiding_klas = case_when(
        opleid %in%
          c(
            "lagere school, basisschool, speciaal onderwijs",
            "geen opleiding gevolgd of enkele jaren lagere school, basisschool gevolgd",
            "VBO/LBO (huishoud-, ambachtsschool, LTS, interne bedrijfsopleiding), MBO-kort, BBL/BOL 1-2, leerlingwezen, ULO"
          ) ~
          "basisopgeleid (maximaal mbo-1)",
        opleid %in%
          c(
            "MAVO, MULO, VMBO",
            "MBO-lang, interne opleiding op mbo-niveau, BBL/BOL 3-4",
            "HAVO, VWO, HBS, MMS"
          ) ~
          'mbo-opgeleid (mbo-2 tm 4 opleiding of havo of vwo afgerond)',
        opleid %in%
          c(
            "WO, universiteit, kandidaatsexamen",
            "HBO, interne opleiding op hbo-niveau"
          ) ~
          "hbo/wo-opgeleid (hbo- of wo-opleiding afgerond)",
        TRUE ~ 'opleiding onbekend'
      )
    ) |>
    mutate(opleiding_klas = factor(opleiding_klas, levels = lev_opl))
}

### herocoderen inkomen ---
my_inkomen <- function(x) {
  x |>
    mutate(
      inkomen_klas = case_when(
        inkomen %in%
          c(
            "netto 1.000 euro per maand of minder",
            "netto tussen de 1.001 en 1.350 euro per maand",
            "netto tussen de 1.351 en 1.750 euro per maand"
          ) ~
          "inkomen laag",
        inkomen %in%
          c(
            "netto tussen de 1.751 en 3.050 euro per maand",
            "netto tussen de 3.051 en 4.000 euro per maand",
            "netto tussen de 4.001 en 5.000 euro per maand"
          ) ~
          "inkomen midden",
        inkomen %in%
          c(
            "netto tussen de 5.001 en 6.000 euro per maand",
            "netto meer dan 6.000 euro per maand"
          ) ~
          "inkomen hoog",
        TRUE ~ 'weet niet, geen antwoord'
      )
    ) |>
    mutate(inkomen_klas = factor(inkomen_klas, levels = lev_ink))
}

### hercoderen geslacht --
my_gesl <- function(x) {
  x |>
    mutate(
      gesl = case_when(
        gesl == 'man' ~ 'man',
        gesl == 'vrouw' ~ 'vrouw',
        TRUE ~ 'onbekend'
      )
    )
}


### herocderen huishouden ---
my_huish <- function(x) {
  x |>
    mutate(
      huishouden_klas = case_when(
        hhsam == "een persoon, alleenstaande" ~ "een persoon",
        hhsam == "(echt)paar zonder kinderen thuis" ~ "paar zonder kinderen",
        hhsam == "(echt)paar met kind(eren) thuis" ~ "paar met kinderen",
        hhsam == "een ouder met kind(eren) thuis" ~ "een ouder",
        TRUE ~ "overig, onbekend"
      )
    )
}

### hercoderen leeftijd ---
my_leeftijd <- function(x) {
  x |>
    mutate(
      leeftijd_klas = case_when(
        leeftd < 35 ~ "34 jaar of jonger",
        leeftd %in% c(35:55) ~ "35 jaar tot en met 55 jaar",
        leeftd > 55 ~ "56 jaar of ouder",
        TRUE ~ "onbekend"
      )
    ) |>
    mutate(
      leeftijd_klas = factor(leeftijd_klas, levels = lev_lft)
    )
}


## def herkomst functie
my_herkomst <- function(x) {
  x |>
    my_opl() |>
    my_gesl() |>
    my_inkomen() |>
    my_huish() |>
    my_leeftijd()
}

achtergrond_var <- c("opleid", "inkomen", "hhsam", "gesl")

data_20_weeg <- data_20_weeg |>
  mutate(across(
    all_of(achtergrond_var),
    ~ haven::as_factor(.x)
  )) |>
  my_herkomst()

data_22_weeg <- data_22_weeg |>
  mutate(across(
    all_of(achtergrond_var),
    ~ haven::as_factor(.x)
  )) |>
  my_herkomst()

data_24_weeg <- data_24_weeg |>
  mutate(across(
    all_of(achtergrond_var),
    ~ haven::as_factor(.x)
  )) |>
  my_herkomst()

data_26_weeg <- data_26_weeg |>
  mutate(across(
    all_of(achtergrond_var),
    ~ haven::as_factor(.x)
  )) |>
  my_herkomst()


### samenvoegen data in een list ---

data_totaal = list(
  data_20_weeg = data_20_weeg,
  data_22_weeg = data_22_weeg,
  data_24_weeg = data_24_weeg,
  data_26_weeg = data_26_weeg
)

write_rds(data_totaal, "01 references/data_totaal_20_22_24_26.rds")
