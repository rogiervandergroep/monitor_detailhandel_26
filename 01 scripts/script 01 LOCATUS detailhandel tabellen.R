
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

######################
### WINKELGEBIEDEN ---
######################

## leegstand naar winkelgebieden
data_locatus_wg <- readr::read_csv2(
  "00 ruwe data/ruw locatus/tabel_locatus_leegst_winkelgeb.csv"
) |>
  filter(
    peildatum %in% c('2025-01-01', '2022-01-01', '2019-01-01' ),
    !is.na(winkelgebied_code),
    groep!= '45-Transp&Brand',
    groep!= '80-ATM'
  )|>
  mutate(vloeroppervlakte = as.double(vloeroppervlakte))|>
  group_by(peildatum, winkelgebied_naam, winkelgebied_code)|>
  mutate(
    aand_vestigingen = aant_vestigingen/sum(aant_vestigingen)*100,
    aand_vloeropp = vloeroppervlakte/sum(vloeroppervlakte)*100
   )


# url met winkelgebieden
wg_url <-'https://onderzoek.amsterdam.nl/static/datavisualisatie-onderzoek-en-statistiek/geo/winkelgebieden/2022/winkelgebieden-2022-geo.json'

# url met stadsdelen
sd_url <-'https://onderzoek.amsterdam.nl/static/datavisualisatie-onderzoek-en-statistiek/geo/amsterdam/2022/stadsdelen-2022-zw-geo.json'


# inlezen geojson van de winkelgebieden en omzetten naar spatial feature
winkelgebieden_geo <- sf::st_read(wg_url)|>
  mutate(winkelgebied_code = glue::glue("W{code}"))

# inlezen geojson van de stadsdelen en omzetten naar spatial feature
stadsdelen_geo <- sf::st_read(sd_url)|>
  rename(
    gbd_sdl_naam = naam,
    gbd_sdl_code = code)





# koppelen geoinformatie winkelgebieden aan winkelgebieddata
data_locatus_wg_geo <- data_locatus_wg |>
  left_join(winkelgebieden_geo, by = "winkelgebied_code")

wg_selectie <- data_locatus_wg_geo |>
  filter(
    peildatum == '2025-01-01',
    groep == '00-Leegstand'
  )
  

ggplot()+  
  geom_sf(data = stadsdelen_geo, color = 'white', fill = "#cfcfcf", linewidth = 0.6)+
  geom_sf(data = wg_selectie,
    aes(fill = aand_vestigingen, geometry = geometry),
    color = NA, 
    linewidth = 0.7)+
  labs(title = NULL,  x = NULL, y = NULL) +
  scale_fill_gradient(name = NULL, low =  '#e7e8f4', high = '#004699')+
  theme_os3(legenda_pos = "right")+
  guides(
    fill = guide_colourbar(reverse = TRUE)
    )
ggsave("03 tabellen/fig_locatus_leegst_wg.svg", width = 12, height = 6)



ggplot()+  
  geom_sf(data = stadsdelen_geo, color = 'white', fill = "#cfcfcf", linewidth = 0.6)+
  geom_sf(data = wg_selectie,
    aes(fill = aand_vloeropp, geometry = geometry),
    color = NA, 
    linewidth = 0.7)+
  labs(title = NULL,  x = NULL, y = NULL) +
  scale_fill_gradient(name = NULL, low =  '#e7e8f4', high = '#004699')+
  theme_os3(legenda_pos = "right")+
  guides(
    fill = guide_colourbar(reverse = TRUE)
    )
ggsave("03 tabellen/fig_locatus_leegst_opp_wg.svg", width = 12, height = 6)









