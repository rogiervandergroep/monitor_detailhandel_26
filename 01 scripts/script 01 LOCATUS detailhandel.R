library(tidyverse)

# locatus data voor winkeldynamiek
data_locatus <- readr::read_csv(
  "00 ruwe data/ruw locatus/notebook_locatus.csv"
) |>
  mutate(winkelvloeroppervlakte = as.integer(winkelvloeroppervlakte))


df_loc_def <- data_locatus |>
  filter(peildatum > "2014-01-01") |>
  mutate(
    dg_nd = case_when(
      groep == '11-Dagelijks' ~ 'detailhandel dagelijks',
      groep %in%
        c(
          '22-Mode & Luxe',
          '35-Vrije Tijd',
          '37-In/Om Huis',
          '38-Detailh Overig'
        ) ~
        'detailhandel niet-dagelijks'
    )
  ) |>
  group_by(peildatum, dg_nd) |>
  summarise(
    `aantal vestigingen` = sum(aantal_vestigingen, na.rm = T),
    `winkelvloeroppervlakte (m2)` = sum(winkelvloeroppervlakte, na.rm = T)
  ) |>
  pivot_longer(cols = c(`aantal vestigingen`, `winkelvloeroppervlakte (m2)`)) |>
  filter(!is.na(dg_nd))


source(
  "http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R"
)
discreet <- c(
  "#004699",
  "#ec0000",
  "#ff9100",
  "#d48fb9",
  "#ffe600",
  "#6cbd74",
  "#009dec"
)

library(svglite)
df_loc_def |>
  ggplot(aes(
    x = lubridate::year(peildatum),
    y = value,
    group = dg_nd,
    colour = dg_nd
  )) +

  geom_line(linewidth = 0.9) +
  labs(y = NULL, x = NULL) +
  theme_os(orientation = "vertical") +
  scale_color_manual(name = NULL, values = discreet) +
  scale_linewidth_manual(name = NULL, values = c(0.8, 1, 2)) +
  guides(colour = guide_legend(nrow = 1, reverse = T), linewidth = 'none') +
  scale_x_continuous(limits = c(2015, 2025), breaks = c(2015, 2020, 2025)) +

  scale_y_continuous(
    labels = scales::label_comma(big.mark = ".", decimal.mark = ",")
  ) +
  facet_wrap(~name, scale = 'free_y')
ggsave("R 03 tabellen/fig_locatus.svg", width = 12, height = 4)

tabel <- df_loc_def |>
  pivot_wider(names_from = dg_nd, values_from = value)


write.xlsx(tabel, "03 tabellen/tabel_locatus.xlsx")
