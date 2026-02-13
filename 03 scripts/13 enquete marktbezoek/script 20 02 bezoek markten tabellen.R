library(tidyverse)

# inlezen ruwe data markten -
data_markt_def <- read_rds("01 references/data_markt_def.rds")


# frequentietabellen

my_table_ams <- function(x, group_vars1, group_vars2) {
  x |>
    group_by(monitor, across(all_of(group_vars1))) |>
    summarise(
      aantal = n(),
      aantal_gew = sum(weeg_ams_ink, na.rm = T)
    ) |>
    group_by(monitor, across(all_of(group_vars2))) |>
    mutate(
      aandeel = aantal / sum(aantal) * 100,
      aandeel_gew = aantal_gew / sum(aantal_gew) * 100
    ) |>
    add_column(
      gebied_type = 'Gemeente',
      gebied_naam = 'Amsterdam',
      gebied_code = '0363'
    )
}

my_table_sd <- function(x, group_vars1, group_vars2) {
  x |>
    group_by(
      monitor,
      gebied_stadsdeel_naam,
      gebied_stadsdeel_code,
      across(all_of(group_vars1))
    ) |>
    summarise(
      aantal = n(),
      aantal_gew = sum(weeg_ams_ink, na.rm = T)
    ) |>
    group_by(
      monitor,
      gebied_stadsdeel_naam,
      gebied_stadsdeel_code,
      across(all_of(group_vars2))
    ) |>
    mutate(
      aandeel = aantal / sum(aantal) * 100,
      aandeel_gew = aantal_gew / sum(aantal_gew) * 100
    ) |>
    add_column(gebied_type = 'stadsdeel') |>
    rename(
      gebied_naam = gebied_stadsdeel_naam,
      gebied_code = gebied_stadsdeel_code
    )
}

my_table_geb <- function(x, group_vars1, group_vars2) {
  x |>
    group_by(
      monitor,
      gebied_ggw_naam,
      gebied_ggw_code,
      across(all_of(group_vars1))
    ) |>
    summarise(
      aantal = n(),
      aantal_gew = sum(weeg_ams_ink, na.rm = T)
    ) |>
    group_by(
      monitor,
      gebied_ggw_naam,
      gebied_ggw_code,
      across(all_of(group_vars2))
    ) |>
    mutate(
      aandeel = aantal / sum(aantal) * 100,
      aandeel_gew = aantal_gew / sum(aantal_gew) * 100
    ) |>
    add_column(gebied_type = 'ggw') |>
    rename(
      gebied_naam = gebied_ggw_naam,
      gebied_code = gebied_ggw_code
    )
}


my_table_wijk <- function(x, group_vars1, group_vars2) {
  x |>
    group_by(
      monitor,
      gebied_wijk_naam,
      gebied_wijk_code,
      across(all_of(group_vars1))
    ) |>
    summarise(
      aantal = n(),
      aantal_gew = sum(weeg_ams_ink, na.rm = T)
    ) |>
    group_by(
      monitor,
      gebied_wijk_naam,
      gebied_wijk_code,
      across(all_of(group_vars2))
    ) |>
    mutate(
      aandeel = aantal / sum(aantal) * 100,
      aandeel_gew = aantal_gew / sum(aantal_gew) * 100
    ) |>
    add_column(gebied_type = 'wijk') |>
    rename(
      gebied_naam = gebied_wijk_naam,
      gebied_code = gebied_wijk_code
    )
}


my_table_markt <- function(x, group_vars1, group_vars2) {
  x |>
    group_by(monitor, v15_schoon, across(all_of(group_vars1))) |>
    summarise(
      aantal = n(),
      aantal_gew = sum(weeg_ams_ink, na.rm = T)
    ) |>
    group_by(monitor, v15_schoon, across(all_of(group_vars2))) |>
    mutate(
      aandeel = aantal / sum(aantal) * 100,
      aandeel_gew = aantal_gew / sum(aantal_gew) * 100
    )
}


tabel_list <- list()

# geen marktbezoek
tabel_list[['freq_bezoek']] <- bind_rows(
  # amsterdam totaal
  data_markt_def |>
    map_df(\(x) {
      my_table_ams(
        x,
        group_vars1 = c("v13"),
        group_vars2 = NULL
      )
    }),

  # stadsdeel
  data_markt_def |>
    map_df(\(x) {
      my_table_sd(
        x,
        group_vars1 = c("v13"),
        group_vars2 = NULL
      )
    }),

  # gebieden
  data_markt_def |>
    map_df(\(x) {
      my_table_geb(
        x,
        group_vars1 = c("v13"),
        group_vars2 = NULL
      )
    })
)

# naar achtergrondkenmerken
tabel_list[['freq_achtergr']] <- bind_rows(
  # amsterdam leeftijd
  data_markt_def |>
    map_df(\(x) {
      my_table_ams(
        x,
        group_vars1 = c("v13", "leeftijd_klas"),
        group_vars2 = c("leeftijd_klas")
      )
    }) |>
    rename(achtergrond_cat = leeftijd_klas) |>
    add_column(achtergrond_naam = "leeftijd_klas"),

  # amsterdam huishouden
  data_markt_def |>
    map_df(\(x) {
      my_table_ams(
        x,
        group_vars1 = c("v13", "huishouden_klas"),
        group_vars2 = c("huishouden_klas")
      )
    }) |>
    rename(achtergrond_cat = huishouden_klas) |>
    add_column(achtergrond_naam = "huishouden_klas"),

  # amsterdam opleiding
  data_markt_def |>
    map_df(\(x) {
      my_table_ams(
        x,
        group_vars1 = c("v13", "opleiding_klas"),
        group_vars2 = c("opleiding_klas")
      )
    }) |>
    rename(achtergrond_cat = opleiding_klas) |>
    add_column(achtergrond_naam = "opleiding_klas"),

  # data inkomen
  data_markt_def |>
    map_df(\(x) {
      my_table_ams(
        x,
        group_vars1 = c("v13", "inkomen_klas"),
        group_vars2 = c("inkomen_klas")
      )
    }) |>
    rename(achtergrond_cat = inkomen_klas) |>
    add_column(achtergrond_naam = "inkomen_klas")
)


# meest bezochte markt
tabel_list[['marktnaam']] <- bind_rows(
  data_markt_def |>
    map_df(\(x) {
      my_table_ams(
        x,
        group_vars1 = c("v15_schoon"),
        group_vars2 = NULL
      )
    }),

  data_markt_def |>
    map_df(\(x) {
      my_table_sd(
        x,
        group_vars1 = c("v15_schoon"),
        group_vars2 = NULL
      )
    }),

  data_markt_def |>
    map_df(\(x) {
      my_table_geb(
        x,
        group_vars1 = c("v15_schoon"),
        group_vars2 = NULL
      )
    }),

  data_markt_def |>
    map_df(\(x) {
      my_table_wijk(
        x,
        group_vars1 = c("v15_schoon"),
        group_vars2 = NULL
      )
    })
)


### huishouden
tabel_list[['achtergrond']] <- bind_rows(
  data_markt_def |>
    map_df(\(x) {
      my_table_ams(
        x,
        group_vars1 = c("marktbezoek", "huishouden_klas"),
        group_vars2 = "marktbezoek"
      )
    }) |>
    add_column(achtergrond_naam = 'huishouden_klas') |>
    rename(achtergrond_cat = huishouden_klas),

  data_markt_def |>
    map_df(\(x) {
      my_table_ams(
        x,
        group_vars1 = c("huishouden_klas"),
        group_vars2 = NULL
      )
    }) |>
    add_column(
      marktbezoek = 'totaal',
      achtergrond_naam = 'huishouden_klas'
    ) |>
    rename(achtergrond_cat = huishouden_klas),

  # leeftijd
  data_markt_def |>
    map_df(\(x) {
      my_table_ams(
        x,
        group_vars1 = c("marktbezoek", "leeftijd_klas"),
        group_vars2 = "marktbezoek"
      )
    }) |>
    add_column(achtergrond_naam = 'leeftijd_klas') |>
    rename(achtergrond_cat = leeftijd_klas),

  data_markt_def |>
    map_df(\(x) {
      my_table_ams(
        x,
        group_vars1 = c("leeftijd_klas"),
        group_vars2 = NULL
      )
    }) |>
    add_column(
      marktbezoek = 'totaal',
      achtergrond_naam = 'leeftijd_klas'
    ) |>
    rename(achtergrond_cat = leeftijd_klas),

  # inkomen
  data_markt_def |>
    map_df(\(x) {
      my_table_ams(
        x,
        group_vars1 = c("marktbezoek", "inkomen_klas"),
        group_vars2 = "marktbezoek"
      )
    }) |>
    add_column(achtergrond_naam = 'inkomen_klas') |>
    rename(achtergrond_cat = inkomen_klas),

  data_markt_def |>
    map_df(\(x) {
      my_table_ams(
        x,
        group_vars1 = c("inkomen_klas"),
        group_vars2 = NULL
      )
    }) |>
    add_column(
      marktbezoek = 'totaal',
      achtergrond_naam = 'inkomen_klas'
    ) |>
    rename(achtergrond_cat = inkomen_klas)
)


write_rds(tabel_list, "01 references/tabellen_markt_basis.rds")


write.xlsx(
  tabel_list,
  "04 reports/03 tabellen/markten_basis.xlsx",
  withFilter = TRUE
)
