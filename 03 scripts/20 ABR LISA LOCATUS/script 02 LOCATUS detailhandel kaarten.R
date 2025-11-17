# url met winkelgebieden
wg_url <- 'https://onderzoek.amsterdam.nl/static/datavisualisatie-onderzoek-en-statistiek/geo/winkelgebieden/2022/winkelgebieden-2022-geo.json'

# url met stadsdelen
sd_url <- 'https://onderzoek.amsterdam.nl/static/datavisualisatie-onderzoek-en-statistiek/geo/amsterdam/2022/stadsdelen-2022-zw-geo.json'


# inlezen geojson van de winkelgebieden en omzetten naar spatial feature
winkelgebieden_geo <- sf::st_read(wg_url) |>
  mutate(winkelgebied_code = glue::glue("W{code}"))

# inlezen geojson van de stadsdelen en omzetten naar spatial feature
stadsdelen_geo <- sf::st_read(sd_url) |>
  rename(
    gbd_sdl_naam = naam,
    gbd_sdl_code = code
  )


# koppelen geoinformatie winkelgebieden aan winkelgebieddata
data_locatus_wg_geo <- data_locatus_wg |>
  left_join(winkelgebieden_geo, by = "winkelgebied_code")

wg_selectie <- data_locatus_wg_geo |>
  filter(
    peildatum == '2025-01-01',
    groep == '00-Leegstand'
  )


ggplot() +
  geom_sf(
    data = stadsdelen_geo,
    color = 'white',
    fill = "#cfcfcf",
    linewidth = 0.6
  ) +
  geom_sf(
    data = wg_selectie,
    aes(fill = aand_vestigingen, geometry = geometry),
    color = NA,
    linewidth = 0.7
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradient(name = NULL, low = "#ffe9f0", high = "#e50082") +
  theme_os3(legenda_pos = "right") +
  guides(
    fill = guide_colourbar(reverse = TRUE)
  )
ggsave(
  "04 reports/04 figuren/fig_locatus_leegst_wg.svg",
  width = 12,
  height = 6
)


ggplot() +
  geom_sf(
    data = stadsdelen_geo,
    color = 'white',
    fill = "#cfcfcf",
    linewidth = 0.6
  ) +
  geom_sf(
    data = wg_selectie,
    aes(fill = aand_vloeropp, geometry = geometry),
    color = NA,
    linewidth = 0.7
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradient(name = NULL, low = "#ffe9f0", high = "#e50082") +
  theme_os3(legenda_pos = "right") +
  guides(
    fill = guide_colourbar(reverse = TRUE)
  )
ggsave(
  "04 reports/04 figuren/fig_locatus_leegst_opp_wg.svg",
  width = 12,
  height = 6
)
