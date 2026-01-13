library(tidyverse)
library(openxlsx)
# inlezen ruwe data markten -
data_markt_def <- read_rds("01 references/data_markt_def.rds")


achtergrondvar <- c(
  "opleiding_klas",
  "inkomen_klas",
  "huishouden_klas",
  "leeftijd_klas",
  "gebied_wijk_code",
  "gebied_wijk_naam",
  "gebied_ggw_code",
  "gebied_ggw_naam",
  "gebied_stadsdeel_code",
  "gebied_stadsdeel_naam"
)


### aandeel marktbezoek ---

data_markt_def[["data_26_weeg"]] <- data_markt_def[["data_26_weeg"]] |>
  rename(
    v14_1 = v14_nw1,
    v14_2 = v14_nw2,
    v14_3 = v14_nw3
  )

data_markt_def[["data_24_weeg"]] <- data_markt_def[["data_24_weeg"]] |>
  rename(
    v14_1 = v14_nw1,
    v14_2 = v14_nw2,
    v14_3 = v14_nw3
  )

data_markt_def[["data_20_weeg"]] <- data_markt_def[["data_20_weeg"]] |>
  rename(
    v14_1 = v141,
    v14_2 = v142,
    v14_3 = v143
  )


my_geen_redenen <- function(x, achtergrond_var) {
  bind_rows(
    x |>
      filter(marktbezoek == 'bezoekt geen markt') |>
      select(
        monitor,
        v14_1,
        weegfactor_ams,
        all_of(achtergrond_var)
      ) |>
      group_by(monitor, v14_1, across(all_of(achtergrond_var))) |>
      summarise(aantal = n()) |>
      rename(v14_cat = v14_1) |>
      add_column(v14_name = 'v14_1'),

    x |>
      filter(marktbezoek == 'bezoekt geen markt') |>
      select(
        monitor,
        v14_2,
        weegfactor_ams,
        all_of(achtergrond_var)
      ) |>
      group_by(monitor, v14_2, across(all_of(achtergrond_var))) |>
      summarise(aantal = n()) |>
      rename(v14_cat = v14_2) |>
      add_column(v14_name = 'v14_2'),

    x |>
      filter(marktbezoek == 'bezoekt geen markt') |>
      select(
        monitor,
        v14_3,
        weegfactor_ams,
        all_of(achtergrond_var)
      ) |>
      group_by(monitor, v14_3, across(all_of(achtergrond_var))) |>
      summarise(aantal = n()) |>
      rename(v14_cat = v14_3) |>
      add_column(v14_name = 'v14_3')
  ) |>
    filter(!is.na(v14_cat)) |>
    group_by(monitor, v14_cat, across(all_of(achtergrond_var))) |>
    mutate(v14_aantal = sum(aantal, na.rm = T)) |>
    filter(v14_name == 'v14_1') |>
    group_by(monitor, v14_name, across(all_of(achtergrond_var))) |>
    mutate(v14_niet_naar_markt = sum(aantal, na.rm = T)) |>
    mutate(aandeel = v14_aantal / v14_niet_naar_markt)
}

## genoemde redenen totaal
tabel_geen_reden_v14 <- bind_rows(
  data_markt_def |>
    map_df(\(x) my_geen_redenen(x, achtergrond_var = NULL)) |>
    add_column(
      achtergrond_type = 'totaal',
      achtergrond_naam = 'totaal'
    ),

  ### geen reden huishouden
  data_markt_def |>
    map_df(\(x) my_geen_redenen(x, achtergrond_var = c("huishouden_klas"))) |>
    add_column(achtergrond_naam = 'huishouden_klas') |>
    rename(achtergrond_type = huishouden_klas),

  ### geen reden stadsdeel
  data_markt_def |>
    map_df(\(x) {
      my_geen_redenen(x, achtergrond_var = c("gebied_stadsdeel_naam"))
    }) |>
    add_column(achtergrond_naam = 'gebied_stadsdeel_naam') |>
    rename(achtergrond_type = gebied_stadsdeel_naam),

  ### geen reden leeftijd
  data_markt_def |>
    map_df(\(x) my_geen_redenen(x, achtergrond_var = c("leeftijd_klas"))) |>
    add_column(achtergrond_naam = 'leeftijd_klas') |>
    rename(achtergrond_type = leeftijd_klas)
) |>
  mutate(
    v14_cat = case_when(
      v14_cat ==
        'combinatie van winkels in deze buurt en een oninteressante markt' ~ 'genoeg winkels en oninteressante markt in buurt',
      TRUE ~ v14_cat
    )
  )

write.xlsx(
  tabel_geen_reden_v14,
  "04 reports/03 tabellen/tabel_v14_reden_nietbezoek.xlsx",
  withFilter = TRUE
)


write_rds(tabel_geen_reden_v14, "01 references/tabellen_markt_geenreden.rds")
