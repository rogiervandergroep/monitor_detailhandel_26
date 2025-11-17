library(tidyverse)

# inlezen ruwe data markten -
data_markt_def <- read_rds("01 references/data_markt_def.RDS")

data_markt_def <- data_markt_def |>
  map(\(x) filter(x, !is.na(v15_schoon)))

# frequentietabellen

my_table <- function(x, marktvar = marktbezoek, groupvars) {
  x |>
    group_by(monitor, {{ marktvar }}, {{ groupvars }}, .drop = F) |>
    summarise(
      aantal = n(),
      aantal_gew = sum(weeg_ams_ink, na.rm = T)
    ) |>

    group_by(monitor, {{ groupvars }}) |>

    mutate(
      aandeel = aantal / sum(aantal) * 100,
      aandeel_gew = aantal_gew / sum(aantal_gew) * 100
    )
}

lev_v13 <- c(
  "een aantal keer per week",
  "1 keer per week",
  "1 keer per twee weken",
  "1 keer per maand",
  "minder dan 1 keer per maand",
  "zelden tot nooit",
  "weet niet, geen antwoord"
)

my_markt_bind_rows <- function(x, marktvar) {
  bind_rows(
    # totaal stad
    x |>
      group_by(monitor, {{ marktvar }}) |>
      summarise(aantal = n(), aantal_gew = sum(weeg_ams_ink, na.rm = T)) |>
      group_by(monitor) |>
      mutate(
        aandeel = aantal / sum(aantal) * 100,
        aandeel_gew = aantal_gew / sum(aantal_gew) * 100
      ) |>
      add_column(
        "gebied_stadsdeel_code" = "0363",
        "gebied_stadsdeel_naam" = "Amsterdam",
        "spatial_type" = 'gemeente'
      ),

    # per stadsdeel
    x |>
      my_table(
        {{ marktvar }},
        groupvars = across(all_of(c(
          "gebied_stadsdeel_code",
          "gebied_stadsdeel_naam"
        )))
      ) |>
      add_column(
        spatial_type = 'stadsdeel'
      ),

    # per wijk
    x |>
      my_table(
        {{ marktvar }},
        groupvars = across(all_of(c(
          "gebied_wijk_code",
          "gebied_wijk_naam",
          "gebied_stadsdeel_code",
          "gebied_stadsdeel_naam"
        )))
      ) |>
      add_column(
        spatial_type = 'wijken'
      ),

    # per gebied
    x |>
      my_table(
        {{ marktvar }},
        groupvars = across(all_of(c(
          "gebied_ggw_code",
          "gebied_ggw_naam",
          "gebied_stadsdeel_code",
          "gebied_stadsdeel_naam"
        )))
      ) |>
      add_column(
        spatial_type = 'ggw_gebieden'
      )
  )
}

tabel_list <- list()

# geen marktbezoek
tabel_list[['geenmarkt']] <- data_markt_def |>
  map_df(\(x) my_markt_bind_rows(x, marktvar = marktbezoek))

# bezoekfrequentie
tabel_list[['marktfreq']] <- data_markt_def |>
  map_df(\(x) my_markt_bind_rows(x, marktvar = v13)) |>
  mutate(v13 = factor(v13, levels = lev_v13))

# meest bezochte markt
tabel_list[['marktnaam']] <- data_markt_def |>
  map_df(\(x) my_markt_bind_rows(x, marktvar = v15_schoon))

write_rds(tabel_list, "01 references/tabellen_markt_basis.RDS")

#   # per inkomensgroep
#   tab_v1_ink = x |>
#     my_table({{marktvar}}, inkomen_klas),

#   # per opleidingsgroep
#   tab_v1_opl = x |>
#     my_table({{marktvar}}, opleiding_klas),

#   # per huishoudgroep
#   tab_v1_hhsam = x |>
#     my_table({{marktvar}},huishouden_klas),

#   # per leeftijdsgroep
#   tab_v1_leefklas = x |>
#     my_table({{marktvar}}, leeftijd_klas),

#   # inkomen en sd
#   tab_v1_sd_ink = bind_rows(

#     x |>
#       my_table({{marktvar}}, groupvars = across(all_of(c("inkomen_klas", "gbd_sdl_naam"))))|>

#     x |>
#       my_table({{marktvar}}, groupvars = across(all_of(c("gbd_sdl_code", "gbd_sdl_naam"))))|>

#       add_column(inkomen_klas="totaal")
#   ),

#   # leeftijd en sd
#   tab_v1_sd_lft = bind_rows(

#     x |>
#       my_table({{marktvar}}, groupvars = across(all_of(c("leeftijd_klas", "gbd_sdl_naam"))))|>

#     x |>
#       my_table({{marktvar}}, groupvars = across(all_of(c("gbd_sdl_code", "gbd_sdl_naam"))))|>

#       add_column(inkomen_klas="totaal")
#   )
# )
