
source("01 scripts/script 00 references.R")
source("01 scripts/script 01 LOCATUS detailhandel functies.R")

# locatus data voor winkeldynamiek
data_locatus <- readr::read_csv(
  "00 ruwe data/ruw locatus/notebook_locatus.csv"
) |>
  mutate(winkelvloeroppervlakte = as.double(winkelvloeroppervlakte))


# amsterdam totaal
df_loc_ams <- data_locatus |>
  my_group_by() |>
  group_by(peildatum, dg_nd) |>
  my_summarise() |>
  group_by(peildatum, name) |>
  mutate(aandeel = value / sum(value)) |>
  add_column(
    gbd_sdl_code = '0363',
    gbd_sdl_naam = 'Amsterdam'
  )

# naar stadsdelen
df_loc_sd <- data_locatus |>
  my_group_by() |>
  group_by(peildatum, gbd_sdl_code, gbd_sdl_naam, dg_nd) |>
  my_summarise() |>
  group_by(peildatum, gbd_sdl_code, gbd_sdl_naam, name) |>
  mutate(aandeel = value / sum(value))

df_loc_totaal <- bind_rows(
  df_loc_ams,
  df_loc_sd
)
